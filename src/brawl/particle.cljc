(ns brawl.particle
  (:require [brawl.floatbuffer :as fb]))

(defn init [x y color type]
  {:pos [x y]
   :spd [(+ 0.1 (rand 0.6))  (+ 0.05 (rand 0.3))]
   :col color
   :type type})


(defn upd-seed [{[x y] :pos [sx sy] :spd :as particle} [l r b t]]
  (let [nx (- x sx)
        nnx (cond
              (< nx l) (+ r (rand 100))
              (> nx r) (- l (rand 100))
              :else nx)
        ny (+ y sy)
        nny (cond
              (> ny b) (- t (rand 100))
              (< ny t) (+ b (rand 100))
              :else ny)]
    (assoc particle :pos [nnx nny])))


(defn upd [{[x y] :pos type :type :as particle} vis-rect]
  (cond
    (= type :seed) (upd-seed particle vis-rect)
    (= type :blood) particle
    (= type :dust) particle
    :else particle))


(defn get-point [{[x y] :pos [r g b a] :col} floatbuffer]
  (fb/append! floatbuffer (array x y r g b a)))
