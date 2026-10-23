(ns app.worker.html
  "Tiny hiccup -> HTML string renderer. No DOM needed, so it runs in a Worker."
  (:require [clojure.string :as str]))

(defn escape [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" "&#39;")))

(deftype Raw [s])
(defn raw "Mark a string as trusted HTML (not escaped)." [s] (->Raw s))

(def ^:private void-tags
  #{"area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "source" "track" "wbr"})

(defn- parse-tag
  "[:div#id.a.b ...] -> [\"div\" \"id\" [\"a\" \"b\"]]"
  [k]
  (let [[_ tag id classes] (re-matches #"([^#.]+)(?:#([^.]+))?(?:\.(.+))?" (name k))]
    [tag id (when classes (str/split classes #"\."))]))

(defn- attr-str [attrs]
  (->> attrs
       (keep (fn [[k v]]
               (cond
                 (or (nil? v) (false? v)) nil
                 (true? v) (str " " (name k))
                 :else (str " " (name k) "=\""
                            (escape (if (keyword? v) (name v) v)) "\""))))
       (apply str)))

(defn render [node]
  (cond
    (nil? node) ""
    (instance? Raw node) (.-s node)
    (vector? node)
    (let [[k & more]      node
          [tag id cls]    (parse-tag k)
          [attrs children] (if (map? (first more)) [(first more) (rest more)] [{} more])
          classes         (cond-> (vec cls)
                            (:class attrs) (conj (let [c (:class attrs)]
                                                   (if (coll? c) (str/join " " c) c))))
          attrs           (cond-> (dissoc attrs :class)
                            id (assoc :id id)
                            (seq classes) (assoc :class (str/join " " classes)))]
      (if (void-tags tag)
        (str "<" tag (attr-str attrs) ">")
        (str "<" tag (attr-str attrs) ">"
             (apply str (map render children))
             "</" tag ">")))
    (seq? node) (apply str (map render node))
    :else (escape node)))

(defn page [hiccup]
  (str "<!doctype html>" (render hiccup)))
