(ns monolog.core
  (:require [reagent.core :as reagent :refer [atom]]
            [alandipert.storage-atom :refer [local-storage]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [cljs.tools.reader :refer [read-string]]
            [cljs.js :refer [empty-state eval js-eval]]))

(def log (local-storage (atom []) :log))

(defn log! [entry] (swap! log conj entry))

(defonce current-filter (atom :all))

(defonce console-contents (atom ""))

(defn eval-code [code]
  (eval (empty-state)
        code
        {:eval       js-eval
         :source-map true
         :context    :expr}
        (fn [result] result)))

(def filters
  {:all {:ixes (fn [log]
                 (for [[message ix] (map vector log (range))]
                   ix))
         :prefix ""}
   :todo {:ixes (fn [log]
                  (for [[message ix] (map vector log (range))
                        :when (re-find #"#todo" (:contents message))
                        :when (not (some #(re-find (re-pattern (str "#done " ix)) (:contents %)) log))]
                    ix))
          :prefix "#todo "}
   :repl {:ixes (fn [log]
                  (for [[message ix] (map vector log (range))
                        :when (re-find #"#repl" (:contents message))]
                    ix))
          :prefix "#repl "}})

(defn reactions [log]
  (into {}
        (concat
         (for [ix ((get-in filters [:todo :ixes]) log)]
           [ix [:button {:on-click (fn [event]
                                     (.preventDefault event)
                                     (log! {:username "todo" :contents (str "#done " ix)}))}
                "✓"]])
         (for [ix ((get-in filters [:repl :ixes]) log)]
           (let [code-str (.replace (:contents (log ix)) "#repl", "")]
             (try
               (let [code (read-string code-str)
                     result (eval-code code)]
                 (if (:error result)
                   [ix [:space " ! " (.-message (.-cause (:error result)))]]
                   [ix [:span " => " (:value result)]]))
               (catch js/Object error
                 [ix [:span " ! " (.-message error)]])))))))

(defn filter-chooser []
  (into [:div] (for [filter (keys filters)]
                 ^{:key (str "filter-" filter)}
                 [:button {:style {:text-decoration (if (= filter @current-filter) "underline" "none")}
                           :on-click (fn [event]
                                       (reset! current-filter filter)
                                       (reset! console-contents (get-in filters [filter :prefix])))}
                  (name filter) " "])))

(defn messages []
  (let [reactions (reactions @log)
        ixes ((get-in filters [@current-filter :ixes]) @log)]
    (into [:div {:style {:overflow-y "scroll"
                         :height "200px"}}]
          (for [ix ixes]
            (let [message (@log ix)]
              ^{:key (str "message-" ix)}
              [:div [:span ix " "] [:span (:contents message)] (reactions ix)])))))

(defn console []
  (let [prefix (get-in filters [@current-filter :prefix])]
    (when-not (.startsWith @console-contents prefix)
      (reset! console-contents prefix))
    [:textarea {:rows 1
                :on-change (fn [event]
                             (reset! console-contents (-> event .-target .-value)))
                :on-key-up (fn [event]
                             (when (and (== (.-keyCode event) 13) (not (.-shiftKey event)))
                               (do
                                 (.preventDefault event)
                                 (log! {:username "jamii" :contents (-> event .-target .-value)})
                                 (reset! console-contents prefix))))
                :value @console-contents}]
    ))

(defn debug []
  [:div
   [:h "Debug"]
   [:button {:on-click #(reset! log [])} "clear log!"]])

(defn page []
  [:div
   [filter-chooser]
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
