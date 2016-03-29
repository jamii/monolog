(ns monolog.core
  (:require [reagent.core :as r :refer [atom]]
            [reagent.ratom :refer [make-reaction]]
            [alandipert.storage-atom :refer [local-storage]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [cljs.reader :refer [read-string]]
            [cljs.js :refer [empty-state eval js-eval]]
            [cljs-time.core :as time]
            cljs-time.format
            cljs-time.coerce
            [goog.structs :as structs]
            [goog.net.XhrIo :as xhrio]))

(defn date-time->str [date-time]
  (cljs-time.format/unparse (cljs-time.format/formatter "YYYY-MM-dd HH:mm:ss") date-time))

(defn str->date-time [str]
  (cljs-time.format/parse (cljs-time.format/formatter "YYYY-MM-dd HH:mm:ss") str))

(def log (local-storage (atom []) :log))

(defn log! [entry]
  (let [now (time/now)]
    (swap! log conj (assoc entry
                   :ix (count @log)
                   :date-time (date-time->str now)
                   :time (cljs-time.coerce/to-date now)
                   :natty nil))))

(defn natty [message]
  (xhrio/send "/natty"
              #(swap! log assoc-in [(:ix message) :natty] (read-string (-> % .-target .getResponseText)))
              "POST"
              (pr-str {:message message})
              (structs/Map. #js {:Content-Type "application/edn"})))

(add-watch log :natty (fn [_ _ _ messages]
                        (doseq [message messages]
                          (when (nil? (:natty message))
                            (natty message)))))

(def now (atom (time/now)))
(defonce always-now (js/setInterval #(reset! now (time/now)) (* 60 1000)))

(defonce console-contents (atom ""))

(defonce editing (atom nil))
(defonce hovering (atom nil))

(defonce current-filter (atom :all))

(js/Notification.requestPermission)

(defn minutes-between [start end]
  (if (time/before? start end) ; interval blows up if end-points are not <
    (time/in-minutes (time/interval start end))
    0))

(defn minutes-in [text]
  (when-let [[_ mins] (re-find #"for (\d+) min" text)]
    (let [mins (js/parseInt mins)]
      (when-not (js/isNaN mins)
        mins))))

(defn task-kind [contents]
  (cond
    (.contains contents "#task") :task
    (.contains contents "#break") :break
    true nil))

(def tasks
  (make-reaction
   (fn []
     (into [] (for [message @log]
                (when-not (:deleted message)
                  (when-let [kind (task-kind (:contents message))]
                    (let [message-time (str->date-time (:date-time message))
                          next-message (first (for [ix (range (-> message :ix inc) (count @log))
                                                    :let [next-message (@log ix)]
                                                    :when (task-kind (:contents next-message))
                                                    :when (not (:deleted next-message))]
                                                next-message))
                          next-message-time (if next-message
                                              (str->date-time (:date-time next-message))
                                              @now)]
                      {:kind kind
                       :start (str->date-time (:date-time message))
                       :duration (minutes-between message-time next-message-time)
                       :estimate (or (minutes-in (:contents message))
                                     (condp = kind
                                       :task 0
                                       :break js/Infinity))}))))))))

(def todos
  (make-reaction
   (fn []
     (into [] (for [message @log]
                (when-not (:deleted message)
                  (when (.contains (:contents message) "#todo")
                    (let [done (first (for [ix (range (-> message :ix inc) (count @log))
                                            :let [next-message (@log ix)]
                                            :when (.contains (:contents next-message) (str "#done " (:ix message)))
                                            :when (not (:deleted next-message))]
                                        next-message))]
                      (if done :done :todo)))))))))

(def filters
  {:all #(do true)
   :todo #(= :todo (@todos (:ix %)))})

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
  (let [original-message message]
    (fn [message]
      [:input {:type "text"
               :style {:flex "1"
                       :margin-left "5px"
                       :margin-right "5px"
                       :margin-bottom "0px"
                       :padding "0"}
               :value (:contents message)
               :on-change (fn [event]
                            (swap! log assoc-in [(:ix message) :contents] (-> event .-target .-value)))
               :on-key-down (fn [event]
                              (when (and (== (.-keyCode event) 13) (not (.-shiftKey event)))
                                (.preventDefault event)
                                (reset! editing nil))
                              (when (== (.-keyCode event) 27)
                                (.preventDefault event)
                                (reset! editing nil)
                                (swap! log assoc-in [(:ix message) :contents] (:contents original-message))))
               :on-blur (fn [event]
                          (when (and (== (.-keyCode event) 13) (not (.-shiftKey event)))
                            (.preventDefault event)
                            (reset! editing nil)))}])))

(def editable-message-ui
  (with-meta editable-messsage-ui-inner
    {:component-did-mount #(.select (r/dom-node %))}))

(defn message-ui-inner [message]
  ^{:key (str "message-" (:ix message))}
  [:div {:style {:width "100%"
                 :display "flex"}}
   (if (= @editing (:ix message))
     [editable-message-ui message]
     [:span {:style {:flex "1" :margin-left "5px" :margin-right "5px"}
             :on-mouse-down (fn [event]
                              (reset! editing (:ix message)))}
      (:contents message)])
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
                               :contents (str "#done " (:ix message))})}
      "✓"])
   [:span {:style {:margin-left "5px" :margin-right "5px"}
           :on-mouse-enter #(reset! hovering (:ix message))
           :on-mouse-leave #(reset! hovering nil)
           :on-click #(swap! log assoc-in [(:ix message) :deleted] true)}
    (if (= @hovering (:ix message)) "X" "#")
    (:ix message)]
   [:span {:style {:margin-left "5px" :margin-right "5px"}} (:date-time message)]])

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
                                       (set! (.-onclick notification) (fn [] (nudge template)))))}))

(defn messages-ui []
  (conj
   (into [:div {:style {:overflow-y "scroll"
                        :flex 1}}]
         (for [message @log
               :when (:contents message)
               :when (not (:deleted message))
               :when ((@current-filter filters) message)]
           [message-ui message]))
   (when-let [last-task (first (for [task (reverse @tasks)
                                     :when task]
                                 task))]
     (when (> (:duration last-task) (:estimate last-task))
       [nudge-ui (str "Your last " (-> last-task :kind name) " is at " (:duration last-task) " / " (:estimate last-task) " mins! What are you up to?") "#task "]))))

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
