(ns monolog.handler
  (:require [compojure.core :refer [GET POST defroutes]]
            [compojure.route :refer [not-found resources]]
            [hiccup.page :refer [include-js include-css html5]]
            [monolog.middleware :refer [wrap-middleware]]
            [environ.core :refer [env]])
  (:import [com.joestelmach.natty Parser]))

(def mount-target
  [:div#app
      [:h3 "ClojureScript has not been compiled!"]
      [:p "please run "
       [:b "lein figwheel"]
       " in order to start the compiler"]])

(def loading-page
  (html5
   [:head
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport"
             :content "width=device-width, initial-scale=1"}]
     (include-css "http://yegor256.github.io/tacit/tacit.min.css")]
   [:body {:style "padding: 0"}
    mount-target
    (include-js "/js/app.js")]))

(defn parse-dates [messages]
  {:status 200
   :headers {"Content-Type" "application/edn"}
   :body [(into []
               (for [message messages
                     group (.parse (new Parser) (:contents message) (:date-time message))]
                 (into [] (.getDates group))))]})

(defroutes routes
  (GET "/" [] loading-page)
  (GET "/about" [] loading-page)
  (POST "/parse-dates" [messages] (parse-dates messages))

  (resources "/")
  (not-found "Not Found"))

(def app (wrap-middleware #'routes))
