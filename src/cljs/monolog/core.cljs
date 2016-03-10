(ns monolog.core
  (:require [reagent.core :as reagent :refer [atom]]
    [reagent.session :as session]
    [secretary.core :as secretary :include-macros true]
    [accountant.core :as accountant]))

(defonce log (atom []))

(defn log! [entry] (swap! log conj entry))

(defn text-input [value]
  [:input {:type "text"
           :value @value
           :on-change #(reset! value (-> % .-target .-value))}])

(defn messages []
  [:div#messages
   (for [[message index] (map vector @log (range))]
    ^{:key (str "message-" index)}
    [:div [:span.username (:username message) ": "] [:span.message (:contents message)]])])

(defn console []
  (let [contents (reagent/atom "")]
  (fn []
    [:form {:on-submit (fn [e]
                         (.preventDefault e)
                         (log! {:username "jamii" :contents @contents})
                         (reset! contents ""))}
      [:input {:type "text"
               :value @contents
               :on-change (fn [e]
                            (reset! contents (-> e .-target .-value)))}]])))

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
