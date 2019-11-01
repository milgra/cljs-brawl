(ns brawl.surface
  (:require [brawl.math2 :as math2]))


(defn generate-from-pointlist
  "Generates physics/segment2-s from surface point list"
  [surfacepoints]
  (loop [src surfacepoints
         res []]
    (if (not-empty src)
      (concat res
       (reduce
        (fn builder [res [x y]] (conj res (math2/segment2 x y)))
        []
        (partition 2 1 (:path (first src)))))
       (recur (rest src) res))))


(defn collect-colliding
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

