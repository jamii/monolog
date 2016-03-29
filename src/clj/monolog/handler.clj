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

(defn natty [message]
  {:status 200
   :headers {"Content-Type" "application/edn"}
   :body [(into [] (for [group (.parse (new Parser) (:contents message) (:time message))]
                     (into [] (.getDates group))))]})

(defroutes routes
  (GET "/" [] loading-page)
  (GET "/about" [] loading-page)
  (POST "/natty" [message] (natty message))

  (resources "/")
  (not-found "Not Found"))

(def app (wrap-middleware #'routes))
