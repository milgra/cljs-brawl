(ns brawl.particle
  (:require
    [brawl.floatbuffer :as fb]
    [mpd.math2 :as math2]))


(defn init
  "create particle"
  [x y color speed type]
  {:pos [x y]
   :spd speed
   :col color
   :type type
   :cnt 0})


(defn update-seed
  "seeds float slowly and reappear on opposing edge of screen"
  [particle rect]
  (let [{[x y] :pos [sx sy] :spd} particle
        [l r b t] rect
        nx (- x sx)
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


(defn update-dust
  "dust should go in radial direction and fade out quickly"
  [particle]
  (let [{[x y] :pos [sx sy] :spd cnt :cnt [r g b a] :col} particle
        nx (+ x sx)
        ny (+ y sy)
        nc (inc cnt)]
    (if (> nc 20) nil (assoc particle :pos [nx ny] :cnt nc :col [r g b a]))))


(defn update-blood
  "blood should drop with gravity"
  [particle rect]
  (let [{[x y] :pos [sx sy] :spd cnt :cnt [r g b a] :col} particle
        [l r b t] rect
        nx (+ x sx)
        ny (+ y sy)
        nsy (+ sy 0.1)
        nc (inc cnt)]
    (if (> nc 100) nil (assoc particle :pos [nx ny] :spd [sx nsy] :cnt nc))))


(defn update-particle
  [particle rect]
  (case (:type particle)
    :seed (update-seed particle rect)
    :blood (update-blood particle rect)
    :dust (update-dust particle)
    particle))


(defn get-point
  [particle floatbuffer]
  (let [{[x y] :pos [r g b a] :col} particle]
    (fb/append! floatbuffer (array x y r g b a))))
