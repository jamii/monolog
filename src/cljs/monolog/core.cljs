(ns monolog.core
  (:require [reagent.core :as r :refer [atom]]
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

(defn log! [entry] (swap! log conj (assoc entry
                                          :ix (count @log)
                                          :date-time (date-time->str (time/now)))))

(defonce console-contents (atom ""))

(defonce editing (atom nil))

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
                                (do
                                  (.preventDefault event)
                                  (reset! editing nil)))
                              (when (== (.-keyCode event) 27)
                                (do
                                  (.preventDefault event)
                                  (reset! editing nil)
                                  (swap! log assoc-in [(:ix message) :contents] (:contents original-message)))))
               :on-blur (fn [event]
                          (when (and (== (.-keyCode event) 13) (not (.-shiftKey event)))
                            (do
                              (.preventDefault event)
                              (reset! editing nil))))}])))

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
   [:span {:style {:margin-left "5px" :margin-right "5px"}} (:reaction message)]
   [:span {:style {:margin-left "5px" :margin-right "5px"}} "#" (:ix message)]
   [:span {:style {:margin-left "5px" :margin-right "5px"}} (:date-time message)]])

(def message-ui
  (with-meta message-ui-inner
    {:component-did-mount #(.scrollIntoView (r/dom-node %))}))

(defn messages-ui []
  (into [:div {:style {:overflow-y "scroll"
                       :flex 1}}]
        (for [message @log
              :when (:contents message)]
          [message-ui message])))

(defn console-ui []
  [:textarea {:rows 1
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
