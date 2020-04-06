(ns brawl.actor
  (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))

(def MPI2 (* Math/PI 2))

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
   :idle-angle 0
   ; walk state
   :foot-order {:active :foot_a :passive :foot_b}
   :foot-target nil
   :foot-surfaces {:active nil :passive nil}
   :jump-state 0
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

            :foot_a (phys2/mass2 (+ x 20.0) y 4.0 10.0 0.0)
            :foot_b (phys2/mass2 (- x 20.0) y 4.0 10.0 0.0)}
   ; debug
   :step-zone [x y]
   ; body metrics
   :metrics {:headl 20.0 :bodyl 50.0 :arml 70.0  :legl 70.0 ; lengths
             :headw 40.0 :neckw 4.0  :armw 4.0   :bodyw 6.0 :hipw 6.0 :legw 6.0 ; widths
             :walks 0.6  :runs 0.4   :punchs 7.0 :kicks 0.2 ; speed
             :maxp 100.0 :hitp 30.0  :kickp 30.0 ; power
             :maxh 100.0 :maxl 10 ; max health and level
             :col 0xFF0000FF :cola 0xAA0000FF :colb 0x00AA00FF :colc 0x0000AAFF :cold 0xAA00AAFF}}) ; colors


(defn triangle_with_bases
  "calculates third point of triangle based on the two base points, side length and direction, used for knee and elbow"
  [a b size dir]
  (let [[x y :as ab2] (math2/scale-v2 (math2/sub-v2 b a) 0.5)
        ab2l (math2/length-v2 ab2)]
    (if (< ab2l size)
      (let [needed (Math/sqrt (- (* size size) (* ab2l ab2l)))
            normal (math2/resize-v2 [(* dir (- y)) (* dir x)] needed)]
        (math2/add-v2 (math2/add-v2 a ab2) normal))
      (math2/add-v2 a ab2))))


(defn hit [{{:keys [head neck hip hand_a hand_b elbow_a elbow_b knee_a knee_b foot_a foot_b]} :masses} hitt hitd]
  (let [headv (math2/sub-v2 neck head)
        bodyv (math2/sub-v2 hip neck)
        legav (math2/sub-v2 knee_a hip)
        legbv (math2/sub-v2 knee_b hip)
        headisp (math2/isp-v2-v2 hitt hitd head headv 0.0)
        bodyisp (math2/isp-v2-v2 hitt hitd neck bodyv 0.0)
        legaisp (math2/isp-v2-v2 hitt hitd hip legav 0.0)
        legbisp (math2/isp-v2-v2 hitt hitd hip legbv 0.0)
        result [(if headisp "head" nil)
                (if bodyisp "body" nil)
                (if legaisp "lega" nil)
                (if legbisp "legb" nil)]]
    result))


(defn move-hand-walk
  "move head point"
  [{:keys [facing] {{[hx hy] :p} :hip {[ax ay] :p} :foot_a {[bx by] :p} :foot_b {[nx ny :as neck] :p} :neck } :masses { arml :arml } :metrics angle :idle-angle :as state}
   {:keys [down up left right punch]}]
  (let [nax (+ (* facing (+ (* arml 0.4 ) (/ (Math/abs (- bx ax )) 8.0 ))) (* (Math/sin angle ) 5.0))
        nbx (- (* facing (- (* arml 0.4 ) (/ (Math/abs (- bx ax )) 8.0 ))) (* (Math/sin angle ) 5.0))
        nay (+ (* arml -0.1 ) (* (Math/cos angle ) 5.0))
        nby (- (* arml -0.14 )(* (Math/cos angle ) 5.0))
        hand_a [(+ nx nax) (+ ny nay)]
        hand_b [(+ nx nbx) (+ ny nby)]
        elbow_a (triangle_with_bases neck hand_a (if punch 20.0 30.0) facing)
        elbow_b (triangle_with_bases neck hand_b 30.0 facing)]
    (-> state
        (assoc-in [:masses :hand_a :p] hand_a)
        (assoc-in [:masses :hand_b :p] hand_b)
        (assoc-in [:masses :elbow_a :p] elbow_a)
        (assoc-in [:masses :elbow_b :p] elbow_b))))


(defn move-head-walk
  "move head point"
  [{:keys [facing] {{[hx hy] :p} :hip {[ax ay] :p} :foot_a {[bx by] :p} :foot_b } :masses { legl :legl } :metrics :as state}
   {:keys [down up left right]}]
  (let [nx (* facing (/ (Math/abs (- bx ax )) 8 )) ; head move forward and backwards when stepping
        nnx (if down (* facing 20.0)) ; head move even forward when squatting
        ny (if down 20.0 0.0) ; head should move lower when squatting
        neck [(+ hx nx nnx) (- hy 50.0)]
        head [(+ hx nx nnx) (- (+ hy ny) 70.0)]]
    (-> state
        (assoc-in [:masses :neck :p] neck)
        (assoc-in [:masses :head :p] head))))


(defn move-hip-jump
  "move hip points, handle jumping"
  [{:keys [next jump-state] {{[hx hy] :p} :hip {[ax ay] :p} :foot_a {[bx by] :p} :foot_b } :masses :as state}
   {:keys [down up]}]
  (let [x (+ ax (/ (- bx ax) 2))
        y (+ ay (/ (- by ay) 2))]
    (assoc-in state [:masses :hip :p] [x (- y 50)])))


(defn move-hip-walk
  "move hip points, handle jumping"
  [{:keys [next jump-state idle-angle facing speed] {{[hx hy] :p} :hip {[ax ay] :p} :foot_a {[bx by] :p} :foot_b } :masses { legl :legl } :metrics :as state}
   {:keys [down up left right]}]
  (let [x (+ ax (/ (- bx ax) 2))
        y (+ ay (/ (- by ay) 2))
        t (- y (* legl 0.85) )
        b (- y 20.0)
        d (cond
            (or down (and up (= jump-state 0)))
            (/ (- b hy) 3)
            (or (not down) (and up (= jump-state 1)))
            (/ (- t hy) 3))
        breathdy (if (and (not left) (not right)) (+ -2.0 (* (Math/sin idle-angle) 2.0)) 2.0)
        legdisty (Math/abs (/ (- bx ax) 10))
        y (+ hy d breathdy legdisty) ; breathing movement when idle, standing should be higher than waling
        dx (if (or left right) (* 0.5 speed) 0.0) ; hip leand forward when moving with speed
        s (cond
            (and up (= jump-state 0) (< (Math/abs d) 0.5)) 1
            (and up (= jump-state 1) (< (Math/abs d) 1.0)) 2
            :else jump-state)
        nnext (if (= s 2) "jump" next)]
    (-> state
        (assoc :next nnext)
        (assoc-in [:masses :hip :p] [(+ x dx) y])
        (assoc :jump-state s))))


(defn get-step-zone
  "gets foot collision triangle"
  [[x y] speed]
  (let [size (cond
               (and (> speed -1.0) (<  speed 0.0)) -10.0
               (and (< speed  1.0) (>= speed 0.0))  10.0
               :else (+ (* (/ speed (Math/abs speed ) 40.0 ) ) (* speed 8.0)))
        A [(+ x size) y]
        B [(- size) (/ (Math/abs size) 2.0)]
        C [(- size) (-(/ (Math/abs size) 2.0))]]
    {:A A :B B :C C}))


(defn get-foot-order
  "based on direction decides active and passive foot"
  [masses speed]
  (let [{[bax bay] :p} (masses :foot_a)
        {[bbx bby] :p} (masses :foot_b)]
    (if (or (and (< bax bbx) (>= speed 0.0)) (and (> bax bbx) (< speed 0.0)))
      {:active :foot_a :passive :foot_b}
      {:active :foot_b :passive :foot_a})))


(defn step-feet-walk
  "puts a triangle from the passive foot on the surfaces, collision ponit is the new foot target for the active foot"
  [{ :keys [masses speed foot-surfaces] :as state} surfaces]
  ; speed must not be 0
  (let [foot-order (get-foot-order masses speed)
        step-zone (get-step-zone (:p (masses (:passive foot-order))) speed)
        collided (sort-by first < (concat
                                   (phys2/get-colliding-surfaces (:A step-zone) (:B step-zone) 10.0 surfaces)
                                   (phys2/get-colliding-surfaces (:A step-zone) (:C step-zone) 10.0 surfaces)))
        surf (first collided)
        foot-target (if surf (nth surf 1) (:A step-zone))
        newpassivesurf (:active foot-surfaces)
        newactivesurf (if surf (nth surf 2) nil)]
    (-> state
        (assoc :step-zone {:A (:A step-zone)
                           :B (math2/add-v2 (:A step-zone)(:B step-zone))
                           :C (math2/add-v2 (:A step-zone)(:C step-zone))})
        (assoc :dostep! false)
        (assoc :foot-order foot-order)
        (assoc :foot-target foot-target)
        (assoc :foot-surfaces {:active newactivesurf :passive newpassivesurf}))))


(defn move-feet-walk
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
            nmasses (assoc-in masses [akw :p] currp)
            step? (if (< stepvl currl) true false)]
        (cond-> state
          true (assoc :masses nmasses)
          step? (step-feet-walk surfaces))) ; step if foot target is close
      (step-feet-walk state surfaces)) ; step if no foot target
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


(defn update-mode
  "if next mode is set, switch to that mode"
  [{:keys [next speed] {ba :foot_a bb :foot_b} :masses :as state}]
  (cond
    (= next nil) state
    (= next "walk")
    (do
      ; reset walk state
      (println "switching to walk mode")
      (-> state
          (update-in [:masses :hip :p] math2/add-v2 [0 25]) ; squat when reaching ground
          (assoc :jump-state 0) ; reset jump state
          (assoc :next nil)
          (assoc :foot-target nil) ; reset stepping
          (assoc :update-fn update-walk)))
    (= next "jump")
    (do
      ; reset jump state
      (println "switching to jump mode")
      (-> state
          (assoc-in [:masses :foot_a :p] (math2/add-v2 (:p ba) [0 -5]))
          (assoc-in [:masses :foot_b :p] (math2/add-v2 (:p bb) [0 -5]))
          (assoc-in [:masses :foot_a :d] [(/ speed 2) -10])
          (assoc-in [:masses :foot_b :d] [(/ speed 2) -10])          
          (assoc :next nil)
          (assoc :update-fn update-jump)))))


(defn update-skeleton
  "update all points"
  [{{{[hipx hipy] :p} :hip
     {neck :p} :neck
     {[txa tya :as foot_a] :p} :foot_a
     {[txb tyb :as foot_b] :p} :foot_b} :masses :as state}
   {:keys [left right up down punch]}]
  (let [facing (state :facing)
        knee_a (triangle_with_bases foot_a [hipx hipy] 30.0 facing)
        knee_b (triangle_with_bases foot_b [hipx hipy] 30.0 facing)]
    (-> state
      (assoc-in [:masses :knee_a :p] knee_a ) 
      (assoc-in [:masses :knee_b :p] knee_b ))))


(defn update-jump
  "jump state update, update foot masses in the world, check if they reached ground" 
  [{:keys [masses speed] :as state}
   {:keys [left right up down] :as control}
   surfaces
   time]
  (let [bases (select-keys masses [:foot_a :foot_b])
        newbases (-> bases
                    (phys2/add-gravity [0.0 0.5])
                    (phys2/move-masses surfaces))
        ground (every? #(and (= % 0.0)) (flatten (map :d (vals newbases))))
        next (cond
               ground "walk"
               (< speed -15) "idle"
               :else nil)
        result (cond-> state
                 next (assoc :next next)
                 true (assoc-in [:masses :foot_a] (:foot_a newbases))
                 true (assoc-in [:masses :foot_b] (:foot_b newbases))
                 true (move-hip-jump control)
                 true (move-head-walk control)
                 true (move-hand-walk control))]
    result))


(defn update-walk
  "update walk state"
  [state control surfaces time]
  (-> state
      (update-speed control time)
      (move-feet-walk surfaces time)
      (move-hip-walk control)
      (move-head-walk control)
      (move-hand-walk control)))


(defn update-idle [state] state)


(defn update-angle
  [{angle :idle-angle :as state}]
  (if (> angle MPI2)
    (assoc state :idle-angle (- angle MPI2))
    (update state :idle-angle + 0.05)))


(defn update-actor [{mode :mode update-fn :update-fn :as state} control surfaces time]
  "update actor state"
  (-> state
      (update-angle)
      (update-fn control surfaces time)
      (update-skeleton control)
      (update-mode)))
