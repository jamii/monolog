(ns monolog.core
  (:require [reagent.core :as r :refer [atom]]
            [reagent.ratom :refer [make-reaction]]
            [alandipert.storage-atom :refer [local-storage]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [cljs.reader :refer [read-string]]
            [cljs.js :refer [empty-state eval js-eval]]
            [goog.structs :as structs]
            [goog.net.XhrIo :as xhrio]
            [goog.string :refer [format]]
            goog.string.format))

(js/Notification.requestPermission)

(def log (local-storage (atom []) :log))

(defn log! [entry]
  (swap! log conj (assoc entry
                   :ix (count @log)
                   :time (js/Date.))))

(def now (atom (js/Date.)))
(defonce always-now (js/setInterval #(reset! now (js/Date.)) (* 60 1000)))

(defonce console-contents (atom ""))

(defonce editing (atom nil))
(defonce hovering (atom nil))
(defonce highlighting (atom [-1 nil]))

(defonce current-filter (atom :all))

(defonce natty (atom {}))

(defn contains [string substring]
  (not= -1 (.indexOf string substring)))

(defn time->string [time]
  (format "%04d/%02d/%02d %02d:%02d:%02d"
          (.getFullYear time)
          (inc (.getMonth time))
          (.getDate time)
          (.getHours time)
          (.getMinutes time)
          (.getSeconds time)))

(defn re-spans [re string]
  (let [re (js/RegExp. (.-source re) "g")]
    (loop [spans []]
      (if-let [match (.exec re string)]
        (recur (conj spans {:start (.-index match)
                            :end (+ (.-index match) (count (aget match 0)))
                            :match match}))
        spans))))

(defn update-natty [messages]
  (doseq [message messages
          :when (nil? (@natty message))]
    (xhrio/send "/natty"
                #(swap! natty assoc message (read-string (-> % .-target .getResponseText)))
                "POST"
                (pr-str {:message message})
                (structs/Map. #js {:Content-Type "application/edn"}))))

(add-watch log :natty (fn [_ _ _ messages] (update-natty messages)))
(update-natty @log)

(defn task-kind [contents]
  (cond
    (contains contents "#task") :task
    (contains contents "#break") :break
    true nil))

(def tasks
  (make-reaction
   (fn []
     @now ; depend on time
     (let [now (js/Date.)]
       (into [] (for [message @log]
                  (when-not (:deleted message)
                    (when-let [kind (task-kind (:contents message))]
                      (let [start (:time message)
                             next-message (first (for [ix (range (-> message :ix inc) (count @log))
                                                       :let [next-message (@log ix)]
                                                       :when (task-kind (:contents next-message))
                                                       :when (not (:deleted next-message))]
                                                   next-message))
                             end (if next-message
                                   (:time next-message)
                                   now)
                             duration (Math.floor (/ (- end start) 1000 60))
                             estimate (if-let [estimated-end (apply max (flatten (for [group (@natty message)] (:times group))))]
                                        (Math.floor (/ (- estimated-end start) 1000 60))
                                        (condp = kind
                                          :task 0
                                          :break js/Infinity))]
                        {:kind kind
                         :start start
                         :end end
                         :duration duration
                         :estimate estimate})))))))))

(def todos
  (make-reaction
   (fn []
     (into [] (for [message @log]
                (when-not (:deleted message)
                  (when (contains (:contents message) "#todo")
                    (let [done (first (for [ix (range (-> message :ix inc) (count @log))
                                            :let [next-message (@log ix)]
                                            :when (contains (:contents next-message) (str "#done #" (:ix message)))
                                            :when (not (:deleted next-message))]
                                        next-message))]
                      (if done :done :todo)))))))))

(defn rating [tag contents]
  (cond
    (contains contents (str tag "++")) 2
    (contains contents (str tag "+")) 1
    (contains contents (str tag "-")) -1
    (contains contents (str tag "--")) -2))

(def samples
  (make-reaction
   (fn []
     (into [] (for [message @log]
                (when-not (:deleted message)
                  (when (contains (:contents message) "#sample")
                    (let [next-sample (apply max (flatten (for [group (@natty message)] (:times group))))]
                      {:time (:time message)
                       :next-sample next-sample
                       :heart (rating "heart" (:contents message))
                       :mind (rating "mind" (:contents message))
                       :body (rating "body" (:contents message))}))))))))

(def lb-minutes
  (make-reaction
   (fn []
     (reduce (fn [acc message]
               (if (contains (:contents message) "#lb")
                 (+ acc (:duration (@tasks (:ix message))))
                 acc))
             0
             @log))))

(def filters
  {:all #(do true)
   :todo #(= :todo (@todos (:ix %)))
   :lb #(contains (:contents %) "#lb")})

(defn filters-ui []
  (into [:div]
        (for [filter (keys filters)]
          [:button {:on-click #(reset! current-filter filter)
                    :style {:font-weight (if (= @current-filter filter)
                                           "bold"
                                           "normal")}}
           (name filter)])))

(defn eval-code [code]
  (eval (empty-state)
        code
        {:eval       js-eval
         :source-map true
         :context    :expr}
        (fn [result] result)))

(defn editable-messsage-ui-inner [message]
  (let [contents (atom (:contents message))]
    (fn [_]
      [:input {:type "text"
               :style {:flex "1"
                       :margin-left "5px"
                       :margin-right "5px"
                       :margin-bottom "0px"
                       :padding "0"}
               :value @contents
               :on-change (fn [event]
                            (reset! contents (-> event .-target .-value)))
               :on-key-down (fn [event]
                              (when (and (== (.-keyCode event) 13) (not (.-shiftKey event)))
                                (.preventDefault event)
                                (reset! editing nil)
                                (swap! log assoc-in [(:ix message) :contents] @contents))
                              (when (== (.-keyCode event) 27)
                                (.preventDefault event)
                                (reset! editing nil)))
               :on-blur (fn [event]
                          (when (and (== (.-keyCode event) 13) (not (.-shiftKey event)))
                            (.preventDefault event)
                            (reset! editing nil)))}])))

(def editable-message-ui
  (with-meta editable-messsage-ui-inner
    {:component-did-mount #(.select (r/dom-node %))}))

(defn parsed-message [parsed contents pos]
  (if (empty? parsed)
    [[:span (.substring contents pos)]]
    (let [section (first parsed)]
      (if (>= (:start section) pos)
        (cons [:span (.substring contents pos (:start section))]
              (cons
               [:span (:props section)
                (.substring contents (:start section) (:end section))]
               (parsed-message (rest parsed) contents (:end section))))
        (do
          (prn "ignoring" section)
          (parsed-message (rest parsed) contents pos))))))

(defn fixed-message-ui [message]
  (let [parsed (concat
                 (for [span (re-spans #"#(\d+)" (:contents message))
                       :let [ix (js/parseInt (aget (:match span) 1))]
                       :when (not (js/isNaN ix))]
                   (assoc span :props
                          {:style {:color "blue"}
                           :title (get-in @log [ix :contents])
                           :on-mouse-down (fn [event]
                                            (.stopPropagation event)
                                            (.scrollIntoView (js/document.getElementById (str "message-" ix))))}))
                 (for [group (@natty message)]
                   {:start (:start group)
                    :end (:end group)
                    :props {:style {:color "green"}
                            :title (time->string (apply max (:times group)))}}))]
    (into [:span {:style {:flex "1" :margin-left "5px" :margin-right "5px"}
                  :on-mouse-down (fn [event]
                                   (reset! editing [:contents (:ix message)]))}]
          (parsed-message (sort-by :start parsed) (:contents message) 0))))

(defn editable-time-ui-inner [message]
  (let [contents (atom (pr-str (:time message)))]
    (fn [_]
      [:input {:type "text"
               :style {:flex 1
                       :margin-left "5px"
                       :margin-right "5px"
                       :margin-bottom "0px"
                       :padding "0"}
               :value @contents
               :on-change (fn [event]
                            (reset! contents (-> event .-target .-value)))
               :on-key-down (fn [event]
                              (when (and (== (.-keyCode event) 13) (not (.-shiftKey event)))
                                (.preventDefault event)
                                (reset! editing nil)
                                (swap! log assoc-in [(:ix message) :time] (read-string @contents)))
                              (when (== (.-keyCode event) 27)
                                (.preventDefault event)
                                (reset! editing nil)))
               :on-blur (fn [event]
                          (when (and (== (.-keyCode event) 13) (not (.-shiftKey event)))
                            (.preventDefault event)
                            (reset! editing nil)))}])))

(def editable-time-ui
  (with-meta editable-time-ui-inner
    {:component-did-mount #(.select (r/dom-node %))}))

(defn fixed-time-ui [message]
  [:span {:style {:margin-left "5px" :margin-right "5px"}
          :on-mouse-down (fn [event]
                           (reset! editing [:time (:ix message)]))} 
   (time->string (:time message))])

(defn message-ui-inner [message]
  ^{:key (str "message-" (:ix message))}
  [:div {:id (str "message-" (:ix message))
         :style {:width "100%"
                 :display "flex"}}
   (if (= @editing [:contents (:ix message)])
     [editable-message-ui message]
     [fixed-message-ui message])
   (when-let [task (@tasks (:ix message))]
     [:span {:style {:margin-left "5px" :margin-right "5px"}}
      (:duration task) " / " (if (= js/Infinity (:estimate task)) "-" (:estimate task)) " mins"])
   (when (= :todo (@todos (:ix message)))
     [:button {:style {:margin "0px 5px 0px 5px"
                       :padding "0px 10px 0px 10px"}
               :on-click #(log! {:username "jamii"
                                 :contents (.replace (:contents message) "#todo" "#task")})}
      "↻"])
   (when (= :todo (@todos (:ix message)))
     [:button {:style {:margin "0px 5px 0px 5px"
                       :padding "0px 10px 0px 10px"}
             :on-click #(log! {:username "jamii"
                               :contents (str "#done #" (:ix message))})}
      "✓"])
   [:span {:style {:margin-left "5px" :margin-right "5px"}
           :on-mouse-enter #(reset! hovering (:ix message))
           :on-mouse-leave #(reset! hovering nil)
           :on-click #(swap! log assoc-in [(:ix message) :deleted] true)}
    (if (= @hovering (:ix message)) "X" "#")
    (:ix message)]
   (if (= @editing [:time (:ix message)])
     [editable-time-ui message]
     [fixed-time-ui message])])

(def message-ui
  (with-meta message-ui-inner
    {:component-did-mount #(.scrollIntoView (r/dom-node %))}))

(defn nudge [template]
  (reset! console-contents template)
  (.select (js/document.getElementById "console")))

(defn nudge-ui-inner [text template]
  [:div {:style {:font-weight "bold"
                  :text-align "center"
                  :flex 1}
          :on-click #(nudge template)}
   text])

(def nudge-ui
  (with-meta nudge-ui-inner
    {:component-will-receive-props (fn [_ [_ text template]]
                                     (let [notification (new js/Notification text
                                                          #js {:requireInteraction true})]
                                       (set! (.-onclick notification) (fn [] (nudge template)))))
     :component-did-mount #(.scrollIntoView (r/dom-node %))}))

(defn messages-ui []
  (conj
   (into [:div {:style {:overflow-y "scroll"
                        :flex 1}}]
         (for [message @log
               :when (:contents message)
               :when (not (:deleted message))
               :when ((@current-filter filters) message)]
           [message-ui message]))
   (when (= :lb @current-filter)
     [:div {:style {:font-weight "bold"
                     :text-align "center"
                     :flex 1}}
      (format "%.0f hours - £%.2f" (/ @lb-minutes 60) (* (/ @lb-minutes 60 6) 400))])
   (when-let [last-task (first (for [task (reverse @tasks)
                                     :when task]
                                 task))]
     (when (> (:duration last-task) (:estimate last-task))
       [nudge-ui (str "Your last " (-> last-task :kind name) " is at " (:duration last-task) " / " (:estimate last-task) " mins! What are you up to?") "#task "]))
   (let [last-sample (first (for [sample (reverse @samples)
                                  :when sample]
                              sample))]
     (when false ; (or (nil? last-sample) (> @now (:next-sample last-sample)))
       [nudge-ui (str "It's sampling time!") (str "#sample heart mind body (next sample " (time->string (js/Date. (+ (.getTime @now) (* (js/Math.random) 1000 60 60 8)))) ")")]))))

(defn console-ui []
  [:textarea#console {:rows 1
              :on-change (fn [event]
                           (reset! console-contents (-> event .-target .-value)))
              :on-key-down (fn [event]
                           (when (and (== (.-keyCode event) 13) (not (.-shiftKey event)))
                             (do
                               (.preventDefault event)
                               (log! {:username "jamii"
                                      :contents (-> event .-target .-value)})
                               (reset! console-contents ""))))
              :value @console-contents}])

(defn debug-ui []
  [:div
   [:h "Debug"]
   [:button {:on-click #(reset! log [])} "clear log!"]])

(defn page-ui []
  [:div {:style {:height "100vh"
                 :width "100vw"
                 :display "flex"
                 :flex-direction "column"
                 :padding "36px"}}
   [filters-ui]
   [messages-ui]
   [console-ui]
   [debug-ui]])

(secretary/defroute "/" []
  (session/put! :current-page #'page-ui))

(defn mount-root []
  (r/render [page-ui] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (secretary/dispatch! path))
    :path-exists?
    (fn [path]
      (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
