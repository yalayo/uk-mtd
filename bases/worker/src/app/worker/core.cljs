(ns app.worker.core
  (:require [reitit.core :as r]
            [app.worker.cf :as cf]))

;; ------------------------------------------------------------------ state
;; Demo only: Worker memory is per-isolate and can be wiped at any time.
;; In a real app use D1 via (aget env "DB"), like the reference project does.

(defonce todos (atom (sorted-map 1 {:id 1 :title "Try htmx on a Worker" :done? false})))
(defonce next-id (atom 1))

;; ------------------------------------------------------------------- views

(defn todo-item [{:keys [id title done?]}]
  [:li {:id (str "todo-" id) :class (when done? "done")}
   [:input {:type      "checkbox"
            :checked   done?
            :hx-put    (str "/todos/" id "/toggle")
            :hx-target (str "#todo-" id)
            :hx-swap   "outerHTML"}]
   [:span title]
   [:button {:hx-delete  (str "/todos/" id)
             :hx-target  (str "#todo-" id)
             :hx-swap    "outerHTML"
             :hx-confirm "Delete this todo?"}
    "×"]])

(defn todo-list []
  [:ul#todos (map todo-item (vals @todos))])

(defn layout [& body]
  [:html {:lang "en"}
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
    [:title "Reitit + htmx on Cloudflare"]
    [:script {:src "https://unpkg.com/htmx.org@2.0.4"}]
    [:style "body{font-family:system-ui;max-width:32rem;margin:2rem auto;padding:0 1rem}
             li{display:flex;gap:.5rem;align-items:center} .done span{text-decoration:line-through;opacity:.6}"]]
   [:body body]])

;; ---------------------------------------------------------------- handlers

(defn home [_]
  (cf/html-page
   (layout
    [:h1 "Todos"]
    [:form {:hx-post     "/todos"
            :hx-target   "#todos"
            :hx-swap     "beforeend"
            :hx-on--after-request "this.reset()"}
     [:input {:name "title" :placeholder "What needs doing?" :required true}]
     [:button "Add"]]
    ;; Lazy-load the list as a fragment to show a plain GET swap
    [:div {:hx-get "/todos" :hx-trigger "load" :hx-swap "outerHTML"} "Loading…"])))

(defn list-todos [{:keys [htmx?]}]
  ;; Same URL serves a fragment to htmx and a full page to a direct visit
  (if htmx?
    (cf/html (todo-list))
    (cf/html-page (layout [:h1 "Todos"] (todo-list)))))

(defn create-todo [{:keys [request]}]
  (-> (cf/form-params request)
      (.then (fn [{:keys [title]}]
               (let [id   (swap! next-id inc)
                     todo {:id id :title title :done? false}]
                 (swap! todos assoc id todo)
                 (cf/html (todo-item todo) {:status 201}))))))

(defn toggle-todo [{:keys [path-params]}]
  (let [id (js/parseInt (:id path-params))]
    (if (contains? @todos id)
      (cf/html (todo-item (get (swap! todos update-in [id :done?] not) id)))
      (cf/html [:p "Not found"] {:status 404}))))

(defn delete-todo [{:keys [path-params]}]
  (swap! todos dissoc (js/parseInt (:id path-params)))
  ;; Empty 200 body + hx-swap="outerHTML" removes the element
  (cf/html nil))

;; ------------------------------------------------------------------ routes

(def routes
  [["/"                 {:get    {:handler home}}]
   ["/todos"            {:get    {:handler list-todos}
                         :post   {:handler create-todo}}]
   ["/todos/:id"        {:delete {:handler delete-todo}}]
   ["/todos/:id/toggle" {:put    {:handler toggle-todo}}]])

(def router (r/router routes))

;; Cloudflare Workers module export: `export default { fetch }`
(def handler
  #js {:fetch (cf/ring-ish-handler router)})
