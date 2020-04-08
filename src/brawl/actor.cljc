(ns brawl.actor
  (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]
            [clojure.spec.alpha :as spec]))

(def MPI2 (* Math/PI 2))
(spec/def ::norm (spec/and #(> % 0.0) #(< % 1.0)))        

(declare update-jump)
(declare update-walk)
(declare update-idle)
(declare step-feet)


(defn basemetrics-default []
  {:height 0.5
   :hitpower 0.5
   :hitrate 0.5
   :stamina 0.5
   :speed 0.5
   :color_a [1.0 0.0 0.0 1.0]
   :color_b [0.0 0.0 1.0 1.0]})


(defn basemetrics-random []
  {:height (/ (rand 10) 10)
   :hitpower (/ (rand 10) 10)
   :hitrate (/ (rand 10) 10)
   :stamina (/ (rand 10) 10)
   :speed (/ (rand 10) 10)
   :color_a [(rand) (rand) (rand) 1.0]
   :color_b [(rand) (rand) (rand) 1.0]})


(defn generate-metrics [{:keys [hitpower hitrate stamina speed height color_a color_b] :as base}]
  (println "generate metrics" base)
  (let [hp (cond (> hitpower 1.0) 1.0 (< hitpower 0.0) 0.0 :else hitpower)
        hr (cond (> hitrate 1.0) 1.0 (< hitrate 0.0) 0.0 :else hitrate)
        st (cond (> stamina 1.0) 1.0 (< stamina 0.0) 0.0 :else stamina)
        sp (cond (> speed 1.0) 1.0 (< speed 0.0) 0.0 :else speed)

        delta (-(- 2.5 height) (+ st sp hp hr))
        size (cond (> (+ height delta) 1.0) 1.0 (< (+ height delta) 0.0) 0.0 :else (+ height delta))

        headl (+ 16.0 (* size 8.0))
        bodyl (+ 50.0 (* size 20.0)) 
        arml (+ 50.0 (* size 20.0)) 
        legl (+ 60.0 (* size 20.0)) 

        headw (+ 36.0 (* size 8.0)) 
        neckw (+ 4.0 (* hp 5.0)) 
        armw (+ 4.0 (* hp 7.0)) 
        hipw (+ 6.0 (* st 20.0)) 
        legw (+ 6.0 (* st 5.0)) 
        
        runs (+ 5.0 (* speed 4.0) height )
        walks (* runs 0.6)
        punchs (+ 7.0 (* hr 2.0))
        kicks (+ 0.2 hr)

        maxh (+ 100.0 (* st 10.0))
        maxp (+ 100.0 (* hp 10.0))

        hitp (+ (* maxp 0.3) (* maxp 0.2 hp ) )
        kickp (+ (* maxp 0.3) (* maxp 0.2 hp ) )

        [ra ga ba] color_a
        [rb gb bb] color_b

        dra (if (> ra 0.2) (- ra 0.2) ra)
        dga (if (> ga 0.2) (- ga 0.2) ga)
        dba (if (> ba 0.2) (- ba 0.2) ba)

        drb (if (> rb 0.2) (- rb 0.2) rb)
        dgb (if (> gb 0.2) (- gb 0.2) gb)
        dbb (if (> bb 0.2) (- bb 0.2) bb)
        ; TODO bodyw needed?
        result {:headl headl :bodyl bodyl :arml arml :legl legl ; lengths
                :headw headw :neckw neckw :armw armw :bodyw headw :hipw hipw :legw legw ; widths
                :walks walks :runs runs   :punchs punchs :kicks kicks ; speed
                :maxp maxp :hitp hitp  :kickp kickp :maxh maxh ; power and health
                :cola color_a :colb [dra dga dba 1.0] :colc color_b :cold [drb dgb dbb 1.0]}] ; colors
    result))
  

(defn init [x y]
  {:next nil
   :speed 0.0
   :power 100.0
   :health 100.0
   :facing 1.0
   :update-fn update-jump
   :idle-angle 0
   ; walk state
   :base-order {:active :base_l :passive :base_r}
   :base-target nil
   :base-surfaces {:active nil :passive nil}
   :jump-state 0
   :step-length 0
   ; command collector
   :commands []
   ; masses
   :masses {:head (phys2/mass2 x y 4.0 1.0 1.0)
            :neck (phys2/mass2 x y 4.0 1.0 1.0)
            :hip (phys2/mass2 x y 4.0 1.0 1.0)
            :hand_l (phys2/mass2 x y 4.0 1.0 1.0)
            :hand_r (phys2/mass2 x y 4.0 1.0 1.0)
            :elbow_l (phys2/mass2 x y 4.0 1.0 1.0)
            :elbow_r (phys2/mass2 x y 4.0 1.0 1.0)
            :knee_l (phys2/mass2 x y 4.0 1.0 1.0)
            :knee_r (phys2/mass2 x y 4.0 1.0 1.0)
            :foot_l (phys2/mass2 (+ x 20.0) y 4.0 10.0 0.0)
            :foot_r (phys2/mass2 (+ x 20.0) y 4.0 10.0 0.0)
            :base_l (phys2/mass2 (+ x 20.0) y 4.0 10.0 0.0)
            :base_r (phys2/mass2 (- x 20.0) y 4.0 10.0 0.0)}
   ; debug
   :step-zone [x y]
   ; body metrics
   :metrics (generate-metrics (basemetrics-random))})

        
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


(defn hit [{{:keys [head neck hip hand_l hand_r elbow_l elbow_r knee_l knee_r foot_l foot_r]} :masses} hitt hitd]
  (let [headv (math2/sub-v2 neck head)
        bodyv (math2/sub-v2 hip neck)
        footav (math2/sub-v2 knee_l hip)
        footbv (math2/sub-v2 knee_r hip)
        headisp (math2/isp-v2-v2 hitt hitd head headv 0.0)
        bodyisp (math2/isp-v2-v2 hitt hitd neck bodyv 0.0)
        footaisp (math2/isp-v2-v2 hitt hitd hip footav 0.0)
        footbisp (math2/isp-v2-v2 hitt hitd hip footbv 0.0)
        result [(if headisp "head" nil)
                (if bodyisp "body" nil)
                (if footaisp "foota" nil)
                (if footbisp "footb" nil)]]
    result))


(defn move-hand-walk
  "move head point"
  [{:keys [facing] {{[hx hy] :p} :hip {[ax ay] :p} :base_l {[bx by] :p} :base_r {[nx ny :as neck] :p} :neck } :masses { arml :arml } :metrics angle :idle-angle :as state}
   {:keys [down up left right punch]}]
  (let [nax (+ (* facing (+ (* arml 0.4 ) (/ (Math/abs (- bx ax )) 8.0 ))) (* (Math/sin angle ) 5.0))
        nbx (- (* facing (- (* arml 0.4 ) (/ (Math/abs (- bx ax )) 8.0 ))) (* (Math/sin angle ) 5.0))
        nay (+ (* arml -0.1 ) (* (Math/cos angle ) 5.0))
        nby (- (* arml -0.14 )(* (Math/cos angle ) 5.0))
        hand_l [(+ nx nax) (+ ny nay)]
        hand_r [(+ nx nbx) (+ ny nby)]
        elbow_l (triangle_with_bases neck hand_l (if punch 20.0 30.0) facing)
        elbow_r (triangle_with_bases neck hand_r 30.0 facing)]
    (-> state
        (assoc-in [:masses :hand_l :p] hand_l)
        (assoc-in [:masses :hand_r :p] hand_r)
        (assoc-in [:masses :elbow_l :p] elbow_l)
        (assoc-in [:masses :elbow_r :p] elbow_r))))


(defn move-head-walk
  "move head point"
  [{:keys [facing] {{[hx hy] :p} :hip {[ax ay] :p} :base_l {[bx by] :p} :base_r } :masses { legl :legl } :metrics :as state}
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
  [{:keys [next jump-state] {{[hx hy] :p} :hip {[ax ay] :p} :base_l {[bx by] :p} :base_r } :masses :as state}
   {:keys [down up]}]
  (let [x (+ ax (/ (- bx ax) 2))
        y (+ ay (/ (- by ay) 2))]
    (assoc-in state [:masses :hip :p] [x (- y 50)])))


(defn move-hip-walk
  "move hip points, handle jumping"
  [{:keys [next jump-state idle-angle facing speed] {{[hx hy] :p} :hip {[ax ay] :p} :base_l {[bx by] :p} :base_r } :masses { legl :legl } :metrics :as state}
   {:keys [down up left right run]}]
  (let [cx (+ ax (/ (- bx ax) 2)) ; x center of bases
        cy (+ ay (/ (- by ay) 2)) ; y center of bases
        ; standing y pos
        sty (- cy (+ (* legl 0.85) ; starting position is 0.85 leglength
                     (/ (Math/abs (- bx ax)) 10.0) ; if legs are closer hip is higher
                     (* (Math/sin idle-angle) 2.0) ; breathing movement
                     (if (< (Math/abs speed) 3.0) (- (* (- 3.0 (Math/abs speed)) 2.0) (/ (Math/abs (- bx ax)) 5.0)) 0))) ; if stangind stand up more with straight back
        ; squatting y pos
        sqy (- cy (* legl 0.5))
        ; final x
        fx (cond
             run (+ cx (* facing 10.0)) ; head is in front of body when running
             :else (+ cx (* facing 2.0))) ; when waling
        ; final y
        dy (cond
             (or down (and up (= jump-state 0)))
             (/ (- sqy hy) 3) ; move to standing pos 
             (or (not down) (and up (= jump-state 1)))
             (/ (- sty hy) 3)) ; move to squatting pos

        fy (+ hy dy)
        
        newstate (cond
                (and up (= jump-state 0) (< (Math/abs dy) 0.5)) 1
                (and up (= jump-state 1) (< (Math/abs dy) 1.0)) 2
                :else jump-state)

        newnext (if (= newstate 2) "jump" next)]
    (-> state
        (assoc-in [:masses :hip :p] [fx fy])
        (assoc :next newnext)
        (assoc :jump-state newstate))))


(defn get-step-zone
  "gets base collision triangle"
  [[x y] speed]
  (let [size (cond
               (and (> speed -1.0) (<  speed 0.0)) -20.0
               (and (< speed  1.0) (>= speed 0.0))  20.0
               :else (+ (* (/ speed (Math/abs speed ) 40.0 ) ) (* speed 8.0)))
        A [(+ x size) y]
        B [(- size) (/ (Math/abs size) 2.0)]
        C [(- size) (-(/ (Math/abs size) 2.0))]]
    {:A A :B B :C C}))


(defn get-base-order
  "based on direction decides active and passive base"
  [masses speed]
  (let [{[bax bay] :p} (masses :base_l)
        {[bbx bby] :p} (masses :base_r)]
    (if (or (and (< bax bbx) (>= speed 0.0)) (and (> bax bbx) (< speed 0.0)))
      {:active :base_l :passive :base_r}
      {:active :base_r :passive :base_l})))


(defn step-feet-walk
  "puts a triangle from the passive base on the surfaces, collision ponit is the new base target for the active base"
  [{ :keys [masses speed base-surfaces] :as state} surfaces]
  ; speed must not be 0
  (let [base-order (get-base-order masses speed)
        step-zone (get-step-zone (:p (masses (:passive base-order))) speed)
        collided (sort-by first < (concat
                                   (phys2/get-colliding-surfaces (:A step-zone) (:B step-zone) 10.0 surfaces)
                                   (phys2/get-colliding-surfaces (:A step-zone) (:C step-zone) 10.0 surfaces)))
        surf (first collided)
        base-target (if surf (nth surf 1) (:A step-zone))
        newpassivesurf (:active base-surfaces)
        newactivesurf (if surf (nth surf 2) nil)]
    (-> state
        (assoc :step-zone {:A (:A step-zone)
                           :B (math2/add-v2 (:A step-zone)(:B step-zone))
                           :C (math2/add-v2 (:A step-zone)(:C step-zone))})
        (assoc :step-length (math2/length-v2 (math2/sub-v2 base-target (:p (masses (:active base-order))))))
        (assoc :dostep! false)
        (assoc :base-order base-order)
        (assoc :base-target base-target)
        (assoc :base-surfaces {:active newactivesurf :passive newpassivesurf}))))


(defn move-knee-walk
 [{:keys [masses speed base-order base-target step-length facing] {legl :legl} :metrics :as state}
   time]
  (let [knee_l (triangle_with_bases (get-in masses [:foot_l :p]) (get-in masses [:hip :p]) (/ legl 1.95) facing)
        knee_r (triangle_with_bases (get-in masses [:foot_r :p]) (get-in masses [:hip :p]) (/ legl 1.95) facing)]
    (-> state
        (assoc-in [:masses :knee_l :p] knee_l) 
        (assoc-in [:masses :knee_r :p] knee_r))))


(defn move-feet-walk
  "move active base towards target point"
  [{:keys [masses speed base-order base-target step-length facing] {legl :legl} :metrics :as state}
   surfaces
   time]
  (if (> (Math/abs speed) 0.1)
    (if base-target
      (let [akw (:active base-order)
            apt (:p (masses akw))
            stepv (math2/sub-v2 base-target apt)
            stepvl (math2/length-v2 stepv)
            currl (* (Math/abs speed) time)
            currv (math2/resize-v2 stepv currl)
            currp (math2/add-v2 apt currv)
            curr-delta (math2/length-v2 (math2/sub-v2 base-target currp))
            step-ratio (if (< (/ step-length 2) curr-delta) (/ (- step-length curr-delta) step-length) (/ curr-delta step-length))
            act_dy (Math/abs (* speed 6.0 step-ratio))
            nmasses (assoc-in masses [akw :p] currp)
            step? (if (< stepvl currl) true false)
            foot_l (cond
                     (= :base_l (:active base-order)) [(first currp) (- (second currp) act_dy)]
                     (= :base_l (:passive base-order)) (:p (:base_l nmasses)))
            foot_r (cond
                     (= :base_r (:active base-order)) [(first currp) (- (second currp) act_dy)]
                     (= :base_r (:passive base-order)) (:p (:base_r nmasses)))]
        (cond-> state
          true (assoc :masses nmasses)
          true (assoc-in [:masses :foot_l :p] foot_l) 
          true (assoc-in [:masses :foot_r :p] foot_r)
          step? (step-feet-walk surfaces))) ; step if base target is close
      (step-feet-walk state surfaces)) ; step if no base target
    (assoc state :base-target nil))) ; stay still if no speed


(defn update-speed
  "update speed based on pressed buttons"
  [{:keys [speed facing metrics] :as state}
   {:keys [left right up down run]}
   time]
  (let [max (if (or run down) (:runs metrics) (:walks metrics))
        nsx (cond
              right (if (> speed max)
                      (- speed (* 0.3 time))
                      (+ speed (* 0.3 time)))
              left (if (< speed (- max))
                      (+ speed (* 0.3 time))
                      (- speed (* 0.3 time)))
              :else (* speed (- 1.0 (* 0.08 time))))
        dir (cond
              (and (> nsx 0.0 ) right) 1
              (and (< nsx 0.0 ) left ) -1
              :else facing)]
    (-> state
        (assoc :speed nsx)
        (assoc :facing dir)))) ; TODO replace facing with dir


(defn update-mode
  "if next mode is set, switch to that mode"
  [{:keys [next speed] {ba :base_l bb :base_r} :masses :as state}]
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
          (assoc :base-target nil) ; reset stepping
          (assoc :update-fn update-walk)))
    (= next "jump")
    (do
      ; reset jump state
      (println "switching to jump mode")
      (-> state
          (assoc-in [:masses :base_l :p] (math2/add-v2 (:p ba) [0 -5]))
          (assoc-in [:masses :base_r :p] (math2/add-v2 (:p bb) [0 -5]))
          (assoc-in [:masses :base_l :d] [(/ speed 2) -10])
          (assoc-in [:masses :base_r :d] [(/ speed 2) -10])          
          (assoc :next nil)
          (assoc :update-fn update-jump)))))


(defn update-jump
  "jump state update, update base masses in the world, check if they reached ground" 
  [{:keys [masses speed] :as state}
   {:keys [left right up down] :as control}
   surfaces
   time]
  (let [bases (select-keys masses [:base_l :base_r])
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
                 true (assoc-in [:masses :base_l] (:base_l newbases))
                 true (assoc-in [:masses :base_r] (:base_r newbases))
                 true (move-hip-jump control)
                 true (move-knee-walk control)
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
      (move-knee-walk control)
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
      (update-mode)))
