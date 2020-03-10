(ns tubax.helpers)

;; Datastructure access
(defn is-node
  "Checks if the parameter matchs the tubax node contract"
  [node]
  (and (map? node)
       (contains? node :tag)
       (keyword? (:tag node))
       (contains? node :attributes)
       (map? (:attributes node))
       (contains? node :content)
       (vector? (:content node))))

(defn tag [{:keys [tag]}] tag)
(defn attributes [{:keys [attributes]}] attributes)
(defn children [{:keys [content]}] content)

(defn text [node]
  (let [[value & _] (children node)]
    (if (string? value) value nil)))

(defn seq-tree [tree]
  (tree-seq is-node children tree))

(defn filter-tree [search-fn tree]
  (->> tree seq-tree (filter search-fn)))

(defn first-tree [search-fn tree]
  (->> tree (filter-tree search-fn) first))

(defn find-first-by-path [path-left node]
  (cond
    (empty? path-left) node
    (nil? node) nil
    (string? node) nil
    :else
    (let [subtree (some #(if (= (tag %) (first path-left)) % nil)
                        (children node))]
      (recur (rest path-left)
             subtree))))

(defn find-all-by-path [path node]
  (cond
    (empty? path) '()
    (and (= (count path) 1) (= (tag node) (first path))) (list node)
    (and (= (count path) 1)) '()
    :else
    (if (= (tag node) (first path))
      (apply concat (map (partial find-all-by-path (rest path))
                         (children node)))
      '())))

;; Dispatcher function for both 'find-first' an 'find-all'
(defn- find-multi-dispatcher [_ param]
  (let [key (-> param keys first)]
    (cond
      (and (= key :attribute)
           (keyword? (get param key)))
      [:attribute :keyword]
      (and (= key :attribute)
           (vector? (get param key)))
      [:attribute :vector]
      :else key
      )))

;; Find first
(defmulti find-first find-multi-dispatcher)

(defmethod find-first :tag [tree param]
  (first-tree #(= (tag %) (:tag param)) tree))

(defmethod find-first [:attribute :keyword] [tree param]
  (first-tree #(contains? (attributes %) (:attribute param)) tree))

(defmethod find-first [:attribute :vector] [tree param]
  (let [[key value] (:attribute param)]
    (first-tree #(and (contains? (attributes %) key)
                      (= (get (attributes %) key) value)) tree)))

(defmethod find-first :path [tree {:keys [path]}]
  (if (and (not (empty? path)) (= (first path) (tag tree)))
    (find-first-by-path (rest path) tree)
    nil))

;; Find all
(defmulti find-all find-multi-dispatcher)

(defmethod find-all :tag [tree param]
  (filter-tree #(= (tag %) (:tag param)) tree))

(defmethod find-all [:attribute :keyword] [tree param]
  (filter-tree #(contains? (attributes %) (:attribute param)) tree))

(defmethod find-all [:attribute :vector] [tree param]
  (let [[key value] (:attribute param)]
    (filter-tree #(and (contains? (attributes %) key)
                       (= (get (attributes %) key) value)) tree)))

(defmethod find-all :path [tree {:keys [path]}]
  (find-all-by-path path tree))
