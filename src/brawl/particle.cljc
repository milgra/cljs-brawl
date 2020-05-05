(ns brawl.particle
  (:require [brawl.floatbuffer :as fb]
            [mpd.math2 :as math2]
            ))

(defn init [x y color speed type]
  {:pos [x y]
   :spd speed
   :col color
   :type type
   :cnt 0})


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


(defn upd-dust [{[x y] :pos [sx sy] :spd cnt :cnt [r g b a] :col  :as particle}]
  (let [nx (+ x sx)
        ny (+ y sy)
        nc (inc cnt)]
    (if (> nc 10) nil (assoc particle :pos [nx ny] :cnt nc :col [r g b (/ 1.0 nc ) ] ))))


(defn upd-blood [{[x y] :pos [sx sy] :spd cnt :cnt [r g b a] :col  :as particle} [l r b t]]
  (let [nx (+ x sx)
        ny (+ y sy)
        nsy (+ sy 0.1)
        nc (inc cnt)]
    (if (> nc 100) nil (assoc particle :pos [nx ny] :spd [sx nsy] :cnt nc))))


(defn upd [{[x y] :pos type :type :as particle} vis-rect]
  (cond
    (= type :seed) (upd-seed particle vis-rect)
    (= type :blood)(upd-blood particle vis-rect)
    (= type :dust) (upd-dust particle)
    :else particle))


(defn get-point [{[x y] :pos [r g b a] :col} floatbuffer]
  (fb/append! floatbuffer (array x y r g b a)))
