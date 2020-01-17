(ns mpd.mass
  (:require [mpd.math2 :as math2]))


(defn surfaces-from-pointlist
  "Generates physics/segment2-s from surface point list"
  [surfacepoints]
  (loop [src surfacepoints
         res []]
    (if (not-empty src)
      (concat res
       (reduce
        (fn builder [res [x y]] (conj res (math2/segment2 x y)))
        []
        (partition 2 1 (first src))))
       (recur (rest src) res))))


(defn collect-colliding-surfaces
  "collect surfaces touched by masspoint movement"
  [mass surfaces]
  (let [mtrans (:trans mass)
        mbasis (:basis mass)]
    (reduce
     (fn checktouch [res {strans :trans sbasis :basis :as onesurface }]
       (let [cross (math2/cross_vec2
                    mtrans
                    mbasis
                    strans
                    sbasis )]
         (if (not= cross nil)
           (conj res onesurface)
           res)))
     []
     surfaces)))


(defn mass2
  "create basic structure"
  [x y r w e]
  {:trans [x y]
   :basis [0 0]
   :weight w
   :radius r
   :elasticity e
   :segmentgroups []})


(defn move-mass [{[tx ty] :trans
                  basis :basis
                  radius :radius
                  elast :elasticity :as mass} surfaces time]
  "check collision of mass basis with all surfaces, moves mass to next iteration point based on time"
  (println "move mass" tx ty basis)
  (let [touched (collect-colliding-surfaces mass surfaces)
        [bx by :as newbasis] (if (not-empty touched)
                               (let [[mx my] (math2/scale_vec2 (math2/mirror_vec2 ((first touched) :basis ) basis) elast) ]
                                 (if (and (< mx radius) (< my radius))
                                   [0.0 0.0]
                                   [mx my]
                                 ))
                               basis)]
        (-> mass
            (assoc :trans [(+ tx (* bx time))(+ ty (* by time ))])
            (assoc :basis newbasis))))


(defn moves-masses
  "check collisions and move masses to new positions considering collisions"
  [masses surfaces time]
  (map (fn [element]
         (move-mass element surfaces time))
       masses))

(defn add-gravity
  "adds gravity vector to masspoints basises"
  [masses gravity time]
  (map (fn [{[bx by] :basis :as element}]
         (assoc element :basis [bx (+ by 0.5)]))
       masses))


(defn update-masses
  "Moves masses to new positions considering collisions"
  [masses surfaces time]
  (let [newmasses
        (-> masses
            (add-gravity 0.5 time)
            (moves-masses surfaces time))]
    newmasses))
