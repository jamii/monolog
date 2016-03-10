(ns monolog.core
  (:require [reagent.core :as reagent :refer [atom]]
    [reagent.session :as session]
    [secretary.core :as secretary :include-macros true]
    [accountant.core :as accountant]))

(defonce log (atom []))

(defonce current-filter (atom :all))

(defonce console-contents (atom ""))

(defn log! [entry] (swap! log conj entry))

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
          :prefix "#todo "}})

(defn reactions [log]
  (into {}
        (for [ix ((get-in filters [:todo :ixes]) log)]
          [ix [:button {:on-click (fn [event]
                                    (.preventDefault event)
                                    (log! {:username "todo" :contents (str "#done " ix)}))}
               "✓"]])))

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
    (into [:div]
          (for [ix ixes]
            (let [message (@log ix)]
              ^{:key (str "message-" ix)}
              [:div [:span ix " "] [:span (:contents message)] (reactions ix)])))))

(defn console []
  (let [prefix (get-in filters [@current-filter :prefix])]
    (when-not (.startsWith @console-contents prefix)
      (reset! console-contents prefix))
    [:input {:rows 3
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

(defn page []
  [:div
   [filter-chooser]
   [messages]
   [console]])

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
