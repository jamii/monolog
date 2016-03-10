(ns monolog.core
  (:require [reagent.core :as reagent :refer [atom]]
    [reagent.session :as session]
    [secretary.core :as secretary :include-macros true]
    [accountant.core :as accountant]))

(defonce log (atom []))

(defn log! [entry] (swap! log conj entry))

(def bots [
  (fn [log]
    (into {}
      (for [[message ix] (map vector log (range))
            :when (re-find #"#todo" (:contents message))
            :when (not (some #(re-find (re-pattern (str "#done " ix)) (:contents %)) log))]
            [ix [:button {:on-click (fn [event]
                                      (.preventDefault event)
                                      (log! {:username "todo" :contents (str "#done " ix)}))}
                 "✓"]])))
])

(defn messages []
  (let [reactions (doall (for [bot bots] (bot @log)))]

  [:div#messages
   (for [[message ix] (map vector @log (range))]
     (let [reaction (into [:span] (for [reaction reactions
                                   :when (reaction ix)]
                                   (reaction ix)))]
    ^{:key (str "message-" ix)}
    [:div [:span ix " "] [:span (:contents message)] reaction]))]))

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
