(ns app.worker.cf
  "Cloudflare Worker <-> reitit glue, modelled on bases/worker/src/app/worker/cf.cljs
  in yalayo/property-management-v, but speaking HTML instead of EDN."
  (:require [reitit.core :as r]
            [app.worker.html :as html]))

;; ---------------------------------------------------------------- responses

(defn response
  ([body] (response body {}))
  ([body {:keys [status headers]}]
   (js/Response. body #js {:status  (or status 200)
                           :headers (clj->js (or headers {}))})))

(defn html
  "Render hiccup as an HTML response. htmx swaps whatever HTML comes back."
  ([hiccup] (html hiccup {}))
  ([hiccup opts]
   (response (html/render hiccup)
             (assoc-in opts [:headers "Content-Type"] "text/html; charset=utf-8"))))

(defn html-page
  "Full document (first load / non-htmx request)."
  [hiccup]
  (response (html/page hiccup) {:headers {"Content-Type" "text/html; charset=utf-8"}}))

(defn hx-redirect
  "Tell htmx to do a client-side redirect."
  [url]
  (response "" {:status 200 :headers {"HX-Redirect" url}}))

;; ----------------------------------------------------------------- requests

(defn htmx-request? [^js request]
  (= "true" (.get (.-headers request) "HX-Request")))

(defn form-params
  "htmx posts forms as application/x-www-form-urlencoded by default.
  Returns a promise of a keyword map."
  [^js request]
  (-> (.formData request)
      (.then (fn [^js fd]
               (reduce (fn [m [k v]] (assoc m (keyword k) v))
                       {}
                       (es6-iterator-seq (.entries fd)))))))

(defn- query-params [^js url]
  (reduce (fn [m [k v]] (assoc m (keyword k) v))
          {}
          (es6-iterator-seq (.entries (.-searchParams url)))))

;; ------------------------------------------------------------------ router

(defn- not-found []
  (html [:p "Not found"] {:status 404}))

(defn- method-not-allowed []
  (html [:p "Method not allowed"] {:status 405}))

(defn ring-ish-handler
  "Given a reitit router whose route data looks like
     {:get {:handler f} :post {:handler g}}
  return a Cloudflare `fetch` function. Handlers receive a map and may return
  a js/Response or a Promise of one."
  [router]
  (fn [^js request env ctx]
    (let [url    (js/URL. (.-url request))
          match  (r/match-by-path router (.-pathname url))
          method (keyword (.toLowerCase (.-method request)))]
      (if-not match
        (not-found)
        (if-let [handler (get-in match [:data method :handler])]
          (-> (js/Promise.resolve
               (handler {:request      request
                         :env          env
                         :ctx          ctx
                         :match        match
                         :path-params  (:path-params match)
                         :query-params (query-params url)
                         :htmx?        (htmx-request? request)}))
              (.catch (fn [err]
                        (js/console.error "Handler error:" err)
                        (html [:p.error "Something went wrong"] {:status 500}))))
          (method-not-allowed))))))
