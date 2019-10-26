(ns brawl.mass
  (:require [brawl.surface :as s]
            [brawl.physics :as p]))


(defn mass2
  "create basic structure"
  [x y]
  {:trans [x y]
   :basis [0 0]
   :weight 0
   :radius 0
   :elasticity 0
   :segmentgroups []})


(defn move-mass
  "check collision of mass basis with all surfaces, moves mass to next iteration point based on time"
  [{[tx ty] :trans basis :basis :as mass} surfaces time]
  (let [touched (s/collect-colliding mass surfaces)
        [bx by :as newbasis] (if (not-empty touched)
                               (p/mirror_vec2 ((first touched) :basis ) basis)
                               basis)]
        (-> mass
            (assoc ,,, :trans [(+ tx (* bx time))(+ ty (* by time ))])
            (assoc ,,, :basis newbasis))))


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
