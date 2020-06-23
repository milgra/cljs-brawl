(ns brawl.svg
  (:require [tubax.core :refer [xml->clj]]
            [clojure.string :as s]))


(defn parse-path
  "parse path, convert string pairs starting with M L to coord array"
  [path id]
  (loop [points [[0 0]]
         part path]
    (if-not part
      (map (fn [[x y]] [(* 2.5 x) (* 2.5 y)]) (rest points))
      (let [command (first part)
            sub-path (subs part 1)
            next-match (re-find  #"[a-zA-Z]" sub-path)
            index (if next-match (s/index-of sub-path next-match))
            sub-word (if-not next-match sub-path (subs sub-path 0 index))
            next-part (if-not next-match nil (subs part (+ index 1)))
            words (remove s/blank? (s/split sub-word #" "))
            coords (map (fn [str] (s/split str #",")) words)
            finalpoints (reduce (fn [oldpoints [x y :as pair]]
                                  (let [[px py] (last oldpoints)]
                                    (case command
                                      "M" (conj oldpoints [(cljs.reader/read-string x)
                                                           (cljs.reader/read-string y)])
                                      "m" (conj oldpoints [(+ px (cljs.reader/read-string x))
                                                           (+ py (cljs.reader/read-string y))])
                                      "L" (conj oldpoints [(cljs.reader/read-string x)
                                                           (cljs.reader/read-string y)])
                                      "l" (conj oldpoints [(+ px (cljs.reader/read-string x))
                                                           (+ py (cljs.reader/read-string y))])
                                      "H" (conj oldpoints [(cljs.reader/read-string x)
                                                           py])
                                      "h" (conj oldpoints [(+ px (cljs.reader/read-string x))
                                                           py])
                                      "V" (conj oldpoints [px
                                                           (+ py (cljs.reader/read-string x))])
                                      "v" (conj oldpoints [px
                                                           (+ py (cljs.reader/read-string x))])
                                      oldpoints))) points coords)]
        (recur finalpoints next-part)))))


(defn parse-shape
  "parse shape, extract color and path from xml node"
  [attrs id]
  (let [myid (if (= id "Surfaces") id  (or (:id attrs) id))]
    (if (contains? attrs :fill)
      {:type "shape"
       :id myid
       :color (js/parseInt (subs (:fill attrs) 1) 16)
       :path (parse-path (:d attrs) (:id attrs))}
      {:type "shape"
       :id myid
       :path (parse-path (:d attrs) (:id attrs))})))


(defn parse-svg
  "parse svg recursively"
  [element id]
  (if (not= nil element)
    (cond
      (map? element) (let [{:keys [tag attributes content]} element]
                       (cond
                         (= tag :g)
                         (parse-svg content (:id attributes))
                         (= tag :path)
                         (concat [(parse-shape attributes id)] (parse-svg content id))
                         :else
                         (parse-svg content id)))
      (vector? element) (reduce #(concat %1 (parse-svg %2 id)) [] element))))


;; (parse-svg (xml->clj
;; "<?xml version='1.0' encoding='UTF-8'?>
;; <svg version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' x='0' y='0' width='612' height='792' viewBox='0, 0, 612, 792'>
;;   <g id='Layer_1'>
;;     <path d='M-422.3,-143.529 L2666.779,-122.25 L2659.686,948.82 L-425.847,948.82 L-422.3,-143.529 z' fill='#8AE0F5'/>
;;     <path d='M1091.441,578.941 L1112.857,463.762 L995.799,459.01 L1099.204,403.94 L1029.933,309.458 L1137.46,355.969 L1168.14,242.902 L1198.82,355.969 L1306.347,309.46 L1237.076,403.94 L1340.481,459.01 L1223.423,463.762 L1244.839,578.941 L1168.14,490.384 z' fill='#FF8200' fill-opacity='0.594'/>
;;     <path d='M1098.243,559.819 L1117.471,456.404 L1012.371,452.139 L1105.212,402.694 L1043.018,317.864 L1139.561,359.622 L1167.107,258.106 L1194.652,359.622 L1291.195,317.864 L1229.001,402.694 L1321.843,452.139 L1216.742,456.404 L1235.971,559.819 L1167.107,480.307 z' fill='#F6FF00' fill-opacity='0.594'/>
;;     <path d='M1155.295,381.893 L1180.372,377.732 L1177.814,439.493 L1161.4,439.493 L1155.295,381.893 z' fill='#000000' fill-opacity='0.594' id='Pivot_l0_t0'/>
;;   </g>
;;   <g id='Surfaces'>
;;     <path d='M38.613,241.018 L53.321,396.925 L1103.035,464.38 L1204.737,465.087 L2197.789,408.692 L2218.38,258.667' fill-opacity='0' stroke='#000000' stroke-width='1'/>
;;   </g>
;; </svg>") "")
