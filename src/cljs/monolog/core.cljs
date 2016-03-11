(ns monolog.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.ratom :refer [make-reaction]]
            [alandipert.storage-atom :refer [local-storage]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [cljs.tools.reader :refer [read-string]]
            [cljs.js :refer [empty-state eval js-eval]]
            [cljs-time.core :as time]
            cljs-time.format))

(defn date-time->str [date-time]
  (cljs-time.format/unparse (cljs-time.format/formatter "YYYY-MM-dd HH:mm:ss") date-time))

(defn str->date-time [str]
  (cljs-time.format/parse (cljs-time.format/formatter "YYYY-MM-dd HH:mm:ss") str))

(def log (local-storage (atom []) :log))

; (defonce log (atom []))

(defn log! [entry] (swap! log conj (assoc entry
                                          :ix (count @log)
                                          :date-time (date-time->str (time/now)))))

(defonce current-view (atom :all))

(defonce console-contents (atom ""))

(defn eval-code [code]
  (eval (empty-state)
        code
        {:eval       js-eval
         :source-map true
         :context    :expr}
        (fn [result] result)))

(def views
  (make-reaction
   (fn []
     {:all (into [] (for [message @log]
                      message))
      :todo (into [] (for [message @log]
                       (when (re-find #"/todo" (:contents message))
                         (when-not (some #(re-find (re-pattern (str "/done " (:ix message))) (:contents %)) @log)
                           (assoc message :reaction
                                  [:button {:on-click (fn [event]
                                                        (.preventDefault event)
                                                        (log! {:username "todo" :contents (str "/done " (:ix message))}))}
                                   "✓"])))))
      :eval (into [] (for [message @log]
                       (when (re-find #"/eval" (:contents message))
                         (let [code-str (.replace (:contents message) "/eval", "")]
                           (assoc message :reaction
                                  (try
                                    (let [code (read-string code-str)
                                          result (eval-code code)]
                                      (if (:error result)
                                        [:space " ! " (.-message (.-cause (:error result)))]
                                        [:span " => " (:value result)]))
                                    (catch js/Object error
                                      [:span " ! " (.-message error)])))))))
      })))

(def prefixes
  {:all ""
   :todo "/todo "
   :eval "/eval "})

(defn view-chooser []
  (into [:div] (for [view (keys @views)]
                 ^{:key (str "view-" view)}
                 [:button {:style {:text-decoration (if (= view @current-view) "underline" "none")}
                           :on-click (fn [event]
                                       (reset! current-view view)
                                       (reset! console-contents (prefixes view)))}
                  (name view) " "])))

(defn messages []
  (into [:div {:style {:overflow-y "scroll"
                       :height "200px"}}]
        (for [message (@views @current-view)]
          ^{:key (str "message-" (:ix message))}
          (when message
            [:div
             [:span "#" (:ix message) " "]
             [:span (:date-time message) " | "]
             [:span (:contents message)]
             (:reaction message)]))))

(defn console []
  (let [prefix (prefixes @current-view)]
    (when-not (.startsWith @console-contents prefix)
      (reset! console-contents prefix))
    [:textarea {:rows 1
                :on-change (fn [event]
                             (reset! console-contents (-> event .-target .-value)))
                :on-key-up (fn [event]
                             (when (and (== (.-keyCode event) 13) (not (.-shiftKey event)))
                               (do
                                 (.preventDefault event)
                                 (log! {:username "jamii"
                                        :contents (-> event .-target .-value)})
                                 (reset! console-contents prefix))))
                :value @console-contents}]
    ))

(defn debug []
  [:div
   [:h "Debug"]
   [:button {:on-click #(reset! log [])} "clear log!"]])

(defn page []
  [:div
   [view-chooser]
   [messages]
   [console]
   [debug]])

(secretary/defroute "/" []
  (session/put! :current-page #'page))

(defn mount-root []
  (reagent/render [page] (.getElementById js/document "app")))

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
