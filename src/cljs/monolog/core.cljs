(ns monolog.core
  (:require [reagent.core :as reagent :refer [atom]]
    [reagent.session :as session]
    [secretary.core :as secretary :include-macros true]
    [accountant.core :as accountant]))

(defonce log (atom []))

(defonce current-filter (atom :all))

(defn log! [entry] (swap! log conj entry))

(def filters
  {:all (fn [log]
          (for [[message ix] (map vector log (range))]
            ix))
   :todo (fn [log]
           (for [[message ix] (map vector log (range))
                 :when (re-find #"#todo" (:contents message))
                 :when (not (some #(re-find (re-pattern (str "#done " ix)) (:contents %)) log))]
             ix))})

(defn reactions [log]
  (into {}
    (for [ix ((:todo filters) log)]
          [ix [:button {:on-click (fn [event]
                                    (.preventDefault event)
                                    (log! {:username "todo" :contents (str "#done " ix)}))}
                "✓"]])))

(defn filter-chooser []
  (into [:div] (for [filter (keys filters)]
    ^{:key (str "filter-" filter)}
    [:button {:style {:text-decoration (if (= filter @current-filter) "underline" "none")}
              :on-click (fn [event]
                          (reset! current-filter filter))}
     (name filter) " "])))

(defn messages []
  (let [reactions (reactions @log)
        ixes ((@current-filter filters) @log)]
    [:div
      (for [ix ixes]
        (let [message (@log ix)]
          ^{:key (str "message-" ix)}
          [:div [:span ix " "] [:span (:contents message)] (reactions ix)]))]))

(defn console []
  (let [contents (reagent/atom "")]
    (fn []
      [:form {:on-submit (fn [event]
                           (.preventDefault event)
                           (log! {:username "jamii" :contents @contents})
                           (reset! contents ""))}
        [:input {:type "text"
         :value @contents
         :on-change (fn [event]
           (reset! contents (-> event .-target .-value)))}]])))

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
