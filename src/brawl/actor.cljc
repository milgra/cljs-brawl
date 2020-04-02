(ns brawl.actor
  (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))

(declare update-jump)
(declare update-walk)
(declare update-idle)
(declare step-feet)

(defn init [x y]
  {; common state
   :next nil
   :speed 0.0
   :facing 1.0
   :update-fn update-jump
   ; walk state
   :foot-order {:active :base_a :passive :base_b}
   :foot-target nil
   :foot-surfaces {:active nil :passive nil}
   :jump-state 0
   :activesurf nil
   :passivesurf nil   
   ; command collector
   :commands []

   :masses {:head (phys2/mass2 x y 4.0 1.0 1.0)
            :neck (phys2/mass2 x y 4.0 1.0 1.0)
            :hip  (phys2/mass2 x y 4.0 1.0 1.0)

            :hand_a (phys2/mass2 x y 4.0 1.0 1.0)
            :hand_b (phys2/mass2 x y 4.0 1.0 1.0)
            
            :elbow_a (phys2/mass2 x y 4.0 1.0 1.0)
            :elbow_b (phys2/mass2 x y 4.0 1.0 1.0)
            
            :knee_a (phys2/mass2 x y 4.0 1.0 1.0)
            :knee_b (phys2/mass2 x y 4.0 1.0 1.0)

            :base_a (phys2/mass2 (+ x 20.0) y 4.0 10.0 0.0)
            :base_b (phys2/mass2 (- x 20.0) y 4.0 10.0 0.0)}
   ; debug
   :step-zone [x y]
   
   :metrics {:headl 20.0 :bodyl 50.0 :arml 50.0  :legl 60.0 ; lengths
             :headw 40.0 :neckw 4.0  :armw 4.0   :bodyw 6.0 :hipw 6.0 :legw 6.0 ; widths
             :walks 0.6  :runs 0.4   :punchs 7.0 :kicks 0.2 ; speed
             :maxp 100.0 :hitp 30.0  :kickp 30.0 ; power
             :maxh 100.0 :maxl 10 ; max health and level
             :col 0xFF0000FF :cola 0xAA0000FF :colb 0x00AA00FF :colc 0x0000AAFF :cold 0xAA00AAFF}}) ; colors


(defn triangle_with_bases
  "calculates third point of triangle based on the two base points, side length and direction"
  [a b size dir]
  (let [[x y :as ab2] (math2/scale-v2 (math2/sub-v2 b a) 0.5)
        ab2l (math2/length-v2 ab2)]
    (if (< ab2l size)
      (let [needed (Math/sqrt (- (* size size) (* ab2l ab2l)))
            normal (math2/resize-v2 [(* dir (- y)) (* dir x)] needed)]
        (math2/add-v2 (math2/add-v2 a ab2) normal))
      (math2/add-v2 a ab2))))


(defn update-skeleton [ {{{[hipx hipy] :p} :hip
                          {[txa tya :as base_a] :p} :base_a
                          {[txb tyb :as base_b] :p} :base_b} :masses :as state} ]
  (let [facing (state :facing)
        neck [hipx (- hipy 50.0)]
        head [hipx (- hipy 70.0)]
        knee_a (triangle_with_bases base_a [hipx hipy] 30.0 facing)
        knee_b (triangle_with_bases base_b [hipx hipy] 30.0 facing)
        hand_a [(+ hipx (* facing 40.0)) (- hipy 50.0)]
        hand_b [(+ hipx (* facing 30.0)) (- hipy 40.0)]
        elbow_a (triangle_with_bases neck hand_a 30.0 facing)
        elbow_b (triangle_with_bases neck hand_b 30.0 facing)]
    (-> state
      (assoc-in [:masses :knee_a :p] knee_a ) 
      (assoc-in [:masses :knee_b :p] knee_b ) 
      (assoc-in [:masses :neck :p] neck)
      (assoc-in [:masses :head :p] head)
      (assoc-in [:masses :hand_a :p] hand_a) 
      (assoc-in [:masses :hand_b :p] hand_b) 
      (assoc-in [:masses :elbow_a :p] elbow_a) 
      (assoc-in [:masses :elbow_b :p] elbow_b))))


(defn update-mode [{:keys [next] {ba :base_a bb :base_b} :masses :as state}]
  (cond
    (= next nil) state
    (= next "walk")
    (do
      ; reset walk state
      (println "switching to walk mode")
      (-> state
          (assoc :jump-state 0)
          (assoc :next nil)
          (assoc :update-fn update-walk)))
    (= next "jump")
    (do
      ; reset jump state
      (println "switching to jump mode")
      (-> state
          (assoc-in [:masses :base_a :p] (math2/add-v2 (:p ba) [0 -5]))
          (assoc-in [:masses :base_b :p] (math2/add-v2 (:p bb) [0 -5]))
          (assoc-in [:masses :base_a :d] [2 -5])
          (assoc-in [:masses :base_b :d] [2 -5])          
          (assoc :next nil)
          (assoc :update-fn update-jump)))))


(defn update-idle [state] state)


(defn update-jump
  "jump state update, update base masses in the world, check if they reached ground" 
  [{:keys [masses jump speed] :as state}
   {:keys [left right up down] :as control}
   surfaces
   time]
  (let [newbases (-> (select-keys masses [:base_a :base_b])
                     (phys2/add-gravity [0.0 0.5])
                     (phys2/move-masses surfaces))
        a_on_ground (every? #(= % 0.0) (get-in newbases [:base_a :d]))
        b_on_ground (every? #(= % 0.0) (get-in newbases [:base_b :d]))
        next (cond
               (and a_on_ground b_on_ground) "walk"
               (< speed -15) "idle"
               :else nil)
        result (cond-> state
                 next (assoc :next next)
                 true (assoc-in [:masses :base_a] (:base_a newbases))
                 true (assoc-in [:masses :base_b] (:base_b newbases)))]
    result))


(defn move-hip [{:keys [next jump-state] {{[hx hy] :p} :hip {[ax ay] :p} :base_a {[bx by] :p} :base_b } :masses :as state}
                {:keys [down up]}]
  (let [x (+ ax (/ (- bx ax) 2))
        y (+ ay (/ (- by ay) 2))
        t (- y 50.0)
        b (- y 20.0)
        d (cond
            (or down (and up (= jump-state 0)))
            (/ (- b hy) 3)
            (or (not down) (and up (= jump-state 1)))
            (/ (- t hy) 3))
        s (cond
            (and up (= jump-state 0) (< (Math/abs d) 0.1)) 1
            (and up (= jump-state 1) (< (Math/abs d) 1.0)) 2
            :else jump-state)
        nnext (if (= s 2) "jump" next)]
    (-> state
        (assoc :next next)
        (assoc-in [:masses :hip :p]  [x (+ hy d)])
        (assoc :jump-state s))))


(defn get-step-zone [[x y] speed]
  (let [size (cond
               (and (> speed -1.0) (<  speed 0.0)) -10.0
               (and (< speed  1.0) (>= speed 0.0))  10.0
               :else (+ (* (/ speed (Math/abs speed ) 40.0 ) ) (* speed 8.0)))
        A [(+ x size) y]
        B [(- size) (/ (Math/abs size) 2.0)]
        C [(- size) (-(/ (Math/abs size) 2.0))]]
    {:A A :B B :C C}))


(defn get-foot-order [masses speed]
  (let [{[bax bay] :p} (masses :base_a)
        {[bbx bby] :p} (masses :base_b)]
    (if (or (and (< bax bbx) (>= speed 0.0)) (and (> bax bbx) (< speed 0.0)))
      {:active :base_a :passive :base_b}
      {:active :base_b :passive :base_a})))


(defn step-feet
  "puts a triangle from the passive foot on the surfaces, collision ponit is the new foot target for the active foot"
  [{ :keys [masses speed activesurf] :as state} surfaces]
  ; speed must not be 0
  (let [foot-order (get-foot-order masses speed)
        step-zone (get-step-zone (:p (masses (:passive foot-order))) speed)
        collided (sort-by first < (concat
                                   (phys2/get-colliding-surfaces (:A step-zone) (:B step-zone) 10.0 surfaces)
                                   (phys2/get-colliding-surfaces (:A step-zone) (:C step-zone) 10.0 surfaces)))
        surf (first collided)
        foot-target (if surf (nth surf 1) (:A step-zone))
        newpassivesurf activesurf
        newactivesurf (if surf (nth surf 1) nil)]
    (-> state
        (assoc :step-zone {:A (:A step-zone)
                           :B (math2/add-v2 (:A step-zone)(:B step-zone))
                           :C (math2/add-v2 (:A step-zone)(:C step-zone))})
        (assoc :dostep! false)
        (assoc :foot-order foot-order)
        (assoc :foot-target foot-target)
        (assoc :foot-surfaces {:active newactivesurf :passive newpassivesurf}))))


(defn move-feet
  "move active foot towards target point"
  [{:keys [masses speed foot-order foot-target] :as state}
   surfaces
   time]
  (if (> (Math/abs speed) 0.1)
    (if foot-target
      (let [akw (:active foot-order)
            apt (:p (masses akw))
            stepv (math2/sub-v2 foot-target apt)
            stepvl (math2/length-v2 stepv)
            currl (* (Math/abs speed) time)
            currv (math2/resize-v2 stepv currl)
            currp (math2/add-v2 apt currv)
            nbases (assoc-in masses [akw :p] currp)
            step? (if (< stepvl currl) true false)]
        (cond-> state
          true (assoc-in [:masses :base_a] (:base_a nbases))
          true (assoc-in [:masses :base_b] (:base_b nbases))
          step? (step-feet surfaces))) ; step if foot target is close
      (step-feet state surfaces)) ; step if no foot target
    (assoc state :foot-target nil))) ; stay still if no speed


(defn update-speed
  "update speed based on pressed buttons"
  [{:keys [speed facing] :as state}
   {:keys [left right up down]}
   time]
  (let [nsx (cond
              right (if (and (> speed -2.0) (< speed 0.0))
                      0
                      (max speed (+ (* 0.4 time)) 10.0))
              left (if (and (< speed 2.0) (> speed 0.0))
                     0
                     (min speed (- (* 0.4 time)) -10.0))
              :else (* speed 0.9))
        dir (cond
              (and (> nsx 0.0 ) right) 1
              (and (< nsx 0.0 ) left ) -1
              :else facing)]
    (-> state
        (assoc :speed nsx)
        (assoc :facing dir)))) ; TODO replace facing with dir


(defn update-walk [state control surfaces time]
  (-> state
      (update-speed control time)
      (move-feet surfaces time)))


(defn update-actor [{mode :mode update-fn :update-fn :as state} control surfaces time]
  (-> state
      (update-fn control surfaces time)
      (move-hip control)
      (update-skeleton)
      (update-mode)))
