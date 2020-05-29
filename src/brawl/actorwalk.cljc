(ns brawl.actorwalk
  (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))


(defn triangle_with_bases
  "calculates third point of triangle based on the two base points, side length and direction, used for knee and elbow"
  [a b size dir]
  (let [[x y :as ab2] (math2/scale-v2 (math2/sub-v2 b a) 0.5)
        ab2l (math2/length-v2 ab2)]
    (if-not (< ab2l size)
      (math2/add-v2 a ab2)
      (let [needed (Math/sqrt (- (* size size) (* ab2l ab2l)))
            normal (math2/resize-v2 [(* dir (- y)) (* dir x)] needed)]
        (math2/add-v2 (math2/add-v2 a ab2) normal)))))


(defn send-commands
  "send commands if needed"
  [{:keys [id commands dragged-gun dragged-body pickup-sent] {:keys [down]} :control :as state}]
  (if (and down (or (= nil dragged-gun) (= nil dragged-body)) (not pickup-sent))
      (-> state
      (update :commands into [{:text "pickup" :id id}])
      (assoc :pickup-sent true))
    state))


(defn move-head-walk
  "move head point"
  [{:keys [facing squat-size punch-pressed speed] {{[hx hy] :p} :hip} :masses
    {{[ax ay] :p} :base_l {[bx by] :p} :base_r} :bases
    {legl :legl bodyl :bodyl headl :headl} :metrics
    {:keys [down up left right punch]} :control :as state}
   time]
  (let [nx (* facing (+ (* (Math/abs speed) 1.0) (/ (Math/abs (- bx ax )) 15.0 ))) ; head move forward and backwards when stepping
        nnx (* facing squat-size 0.5) ; head move even forward when squatting
        ny (* squat-size 0.25) ; head should move lower when squatting
        pnx (if punch (* facing 10.0) 0.0)
        neck [(+ hx nx nnx pnx) (- (+ hy ny) bodyl)]
        head [(+ hx nx nnx pnx) (- (+ hy ny) (+ bodyl headl))]]
    (-> state
        (assoc-in [:masses :neck :p] neck)
        (assoc-in [:masses :head :p] head))))


(defn move-hand-walk
  "move head point"
  [{:keys [id facing punch-hand action-sent commands speed]
    {{[hx hy] :p} :hip {[nx ny :as neck] :p} :neck } :masses
    {{[ax ay] :p} :base_l {[bx by] :p} :base_r} :bases
    {arml :arml} :metrics angle :idle-angle
    {:keys [down up left right punch block]} :control
    :as state}
   surfaces
   time]
  (let [nlx (+ (* facing (+ (* arml 0.4 ) (/ (Math/abs (- bx ax )) 8.0 ))) (* (Math/sin angle ) 5.0))
        nrx (- (* facing (- (* arml 0.4 ) (/ (Math/abs (- bx ax )) 8.0 ))) (* (Math/sin angle ) 5.0))
        nly (+ (* arml 0.1 ) (* (Math/cos angle ) 5.0))
        nry (- (* arml 0.14 )(* (Math/cos angle ) 5.0))
        hand_l (cond
                 block [(+ nx (* facing arml 0.3)) (- ny (* arml 0.4))]
                 (and punch (= punch-hand :hand_l) (not left) (not right)) [(+ nx (* facing arml 0.99)) ny]
                 :else [(+ nx nlx) (+ ny nly)])
        hand_r (cond
                 block [(+ nx (* facing arml 0.3)) (- ny (* arml 0.4))]
                 (and punch (= punch-hand :hand_r) (not left) (not right)) [(+ nx (* facing arml 0.99)) ny]                 
                 :else [(+ nx nrx) (+ ny nry)])
        elbow_l (triangle_with_bases neck hand_l (* arml 0.5) facing)
        elbow_r (triangle_with_bases neck hand_r (* arml 0.5) facing)
        newcommands (if-not (and punch (not action-sent) (not left) (not right))
                      commands
                      (into commands
                            [{:id id
                              :text "attack"
                              :base neck
                              :target (if (= punch-hand :hand_l) hand_l hand_r)
                              :time time
                              :power 10}]))]
    (-> state
        (assoc :commands newcommands)
        (assoc :action-sent (if  (and punch (not action-sent)) true action-sent))
        (assoc-in [:masses :hand_l :p] hand_l)
        (assoc-in [:masses :hand_r :p] hand_r)
        (assoc-in [:masses :elbow_l :p] elbow_l)
        (assoc-in [:masses :elbow_r :p] elbow_r))))


(defn move-hip-walk
  "move hip points, handle jumping"
  [ {:keys [next jump-state idle-angle facing speed base-order squat-size]
     {:keys [down up left right run kick]} :control
     {{[hx hy] :p} :hip } :masses
     {{[lx ly] :p} :base_l {[rx ry] :p} :base_r} :bases
     {legl :legl } :metrics :as state}
   time]
  (let [cx (if (or (< speed -0.5) (> speed 0.5) (not kick)) (+ lx (/ (- rx lx) 2)) ; x center of bases when walking
               (if (= :base_l (:active base-order)) rx lx)) ; passive foot when kic
                 
        cy (+ ly (/ (- ry ly) 2)) ; y center of bases
        sty (- cy (+ (* legl 0.85) ; standing y pos, starting position is 0.85 leglength
                     (/ (Math/abs (- rx lx)) 20.0) ; if legs are closer hip is higher
                     (* (Math/sin idle-angle) 2.0) ; breathing movement
                     (if (< (Math/abs speed) 3.0) (- (* (- 3.0 (Math/abs speed)) 2.0) (/ (Math/abs (- rx lx)) 5.0)) 0))) ; if stangind stand up more with straight back

        squat-size (cond
                     (or down (and up (= jump-state 0)))
                     (+ squat-size (/ (- (* legl 0.5) squat-size) 3)) ; move to standing pos 
                     (or (not down) (and up (= jump-state 1)))
                     (+ squat-size (/ (- squat-size) 3))) ; move to squatting pos

        fx (cond ; final x
             run (+ cx (* facing 10.0)) ; head is in front of body when running
             :else (+ cx (* facing 2.0))) ; when waling

        fy (+ sty squat-size) ; final y

        newstate (cond
                (and up (= jump-state 0) (> squat-size (* legl 0.4))) 1
                (and up (= jump-state 1) (< squat-size 1.0)) 2
                :else jump-state)
        newnext (if (= newstate 2) "jump" next)]
    (-> state
        (assoc-in [:masses :hip :p] [fx fy])
        (assoc :next newnext)
        (assoc :squat-size squat-size)
        (assoc :jump-state newstate))))


(defn move-knee-walk
 [{:keys [masses speed base-order base-target step-length facing] {legl :legl} :metrics :as state}
   time]
  (let [knee_l (triangle_with_bases (get-in masses [:foot_l :p]) (get-in masses [:hip :p]) (/ legl 1.95) facing)
        knee_r (triangle_with_bases (get-in masses [:foot_r :p]) (get-in masses [:hip :p]) (/ legl 1.95) facing)]
    (-> state
        (assoc-in [:masses :knee_l :p] knee_l) 
        (assoc-in [:masses :knee_r :p] knee_r))))


(defn move-feet-walk-still 
  "do kick if needed"
  [{:keys [id bases masses speed base-order base-target step-length facing action-sent commands] {legl :legl runs :runs walks :walks} :metrics {kick :kick} :control :as state}
   surfaces
   time]
  (let [active-base (:active base-order)
        passive-base (:passive base-order)
        [apx apy :as act] (:p (bases active-base)) ; active position
        [ppx ppy :as pas] (:p (bases passive-base)) ; passive position
        kick-point [(+ ppx (* legl facing 1.2)) (- ppy (* legl 1.5))]
        foot_l (if (= :base_l (:active base-order))
                 (if kick kick-point act)
                 pas) ; final position
        foot_r (if (= :base_r (:active base-order))
                 (if kick kick-point act)
                 pas)
        newcommands (if-not (and kick (not action-sent))
                      commands
                      (into commands [{:id id :text "attack" :base (:p (:hip masses)) :target kick-point :time time :power 40}]))]
    (-> state
        (assoc :commands newcommands)
        (assoc :action-sent (if (and kick (not action-sent)) true action-sent))
        (assoc-in [:masses :foot_l :p] foot_l) 
        (assoc-in [:masses :foot_r :p] foot_r)
        (assoc :base-target nil))))


(defn get-step-zone
  "gets base collision triangle"
  [[x y] speed]
  (let [size (cond
               (and (> speed -1.0) (<  speed 0.0)) -20.0
               (and (< speed  1.0) (>= speed 0.0))  20.0
               :else (+ (* (/ speed (Math/abs speed)) 40.0) (* speed 6.0)))
        A [(+ x size) y]
        B [(- size) (/ (Math/abs size) 2.0)]
        C [(- size) (-(/ (Math/abs size) 2.0))]]
    {:A A :B B :C C :T (math2/add-v2 A C) :M (math2/add-v2 A B)}))


(defn get-base-order
  "based on direction decides active and passive base"
  [bases speed]
  (let [{[bax bay] :p} (bases :base_l)
        {[bbx bby] :p} (bases :base_r)]
    (if (or (and (< bax bbx) (>= speed 0.0)) (and (> bax bbx) (< speed 0.0)))
      {:active :base_l :passive :base_r}
      {:active :base_r :passive :base_l})))


(defn step-feet-walk
  "puts a triangle from the passive base on the surfaces, collision ponit is the new base target for the active base"
  [{ :keys [bases masses speed base-surfaces vert-direction] :as state} surfaces]
  ; speed must not be 0
  (let [base-order (get-base-order bases speed)
        base-point (:p (bases (:passive base-order)))
        step-zone (get-step-zone base-point speed)
        collided-top (sort-by first < (phys2/get-colliding-surfaces-by-distance (:T step-zone) (math2/sub-v2 (:A step-zone) (:T step-zone)) 4.0 surfaces base-point))
        collided-bot (sort-by first < (phys2/get-colliding-surfaces-by-distance (:A step-zone) (math2/sub-v2 (:M step-zone) (:A step-zone)) 4.0 surfaces base-point))
        collided (cond
                   (and (= vert-direction  1) (not (empty? collided-top))) collided-top
                   (and (= vert-direction -1) (not (empty? collided-bot))) collided-bot
                   :else (concat collided-top collided-bot))
        surf (first collided)
        base-target (if surf (nth surf 1) (:A step-zone))
        newpassivesurf (:active base-surfaces)
        newactivesurf (if surf (nth surf 2) nil)]

    (-> state
        (assoc :step-zone {:A (:A step-zone)
                           :B (math2/add-v2 (:A step-zone)(:B step-zone))
                           :C (math2/add-v2 (:A step-zone)(:C step-zone))})
        (assoc :step-length (math2/length-v2 (math2/sub-v2 base-target (:p (bases (:active base-order))))))
        (assoc :dostep! false)
        (assoc :base-order base-order)
        (assoc :base-target base-target)
        (assoc :base-surfaces {:active newactivesurf :passive newpassivesurf}))))


(defn move-feet-walk
  "move active base towards target point"
  [{:keys [bases masses speed base-order base-target step-length facing kick-pressed] {legl :legl runs :runs walks :walks} :metrics {kick :kick} :control :as state}
   surfaces
   time]
  (if (> (Math/abs speed) 0.5)
    (if base-target
      (let [active-base (:active base-order)
            passive-base (:passive base-order)
            actual-pos (:p (bases active-base))
            actual-vec (math2/sub-v2 base-target actual-pos)
            actual-size (math2/length-v2 actual-vec)
            current-size (* (Math/abs speed) time)
            current-vec (math2/resize-v2 actual-vec current-size)
            remaining-size (- actual-size current-size)
            [cpx cpy :as current-pos] (math2/add-v2 actual-pos current-vec) ; current position
            [ppx ppy] (:p (bases passive-base)) ; passive position
            walk-lift-ratio (if (< (/ step-length 2) remaining-size); when walking, highest foot position is center TODO simplify 
                              (/ (- step-length remaining-size) step-length) 
                              (/ remaining-size step-length))
            run-lift-ratio-passive  (if (< remaining-size (/ step-length 3)) ; when running. highest foot position is third for passive
                                  (/ (- (/  step-length 3.0) remaining-size ) ( / step-length 3.0 ))
                                  0.0)
            run-lift-ratio-active (/ remaining-size step-length) ; active foot falling is linear to target point
            walk-lift-active (Math/abs (* speed 6.0 walk-lift-ratio))
            walk-lift-passive 0.0
            run-lift-active (* legl 0.5 run-lift-ratio-active)
            run-lift-passive (* legl 0.5 run-lift-ratio-passive)
            speed-ratio (if (> (Math/abs speed) walks) ; walk / run speed ratio in actual state
                          (let [speed-diff (- runs (Math/abs speed))
                                walkr (/ speed-diff (- runs walks))]
                            walkr)
                          1.0)
            lift-active (+ (* speed-ratio walk-lift-active) (* (- 1.0 speed-ratio) run-lift-active)) ; merge walk and run states
            lift-passive (+ (* speed-ratio walk-lift-passive) (* (- 1.0 speed-ratio) run-lift-passive))
            foot_l (if (= :base_l (:active base-order))
                     [cpx (- cpy lift-active)]
                     [ppx (- ppy lift-passive)]) ; final position
            foot_r (if (= :base_r (:active base-order))
                     [cpx (- cpy lift-active)]
                     [ppx (- ppy lift-passive)])
            step? (if (< actual-size current-size) true false)] ; do we need step
        (cond-> state
          true (assoc-in [:bases active-base :p] current-pos)
          true (assoc-in [:masses :foot_l :p] foot_l) 
          true (assoc-in [:masses :foot_r :p] foot_r)
          step? (step-feet-walk surfaces))) ; step if base target is close
      (step-feet-walk state surfaces)) ; step if no base target
    (move-feet-walk-still state surfaces time))) ; stay still or do kick if no speed


(defn update-speed
  "update speed based on pressed buttons"
  [{:keys [speed facing metrics]  {:keys [left right up down run]} :control :as state}
   time]
  (let [max (if (or (not run) down) (:walks metrics) (:runs metrics))
        nsx (cond
              right (if (> speed max)
                      (+ max 0.1)
                      (+ speed (* 0.8 time)))
              left (if (< speed (- max))
                      (- (- max) 0.1)
                      (- speed (* 0.8 time)))
              :else (* speed (- 1.0 (* 0.08 time))))
        dir (cond
              (and (> nsx 0.0 ) right) 1
              (and (< nsx 0.0 ) left ) -1
              :else facing)]
    (-> state
        (assoc :speed nsx)
        (assoc :facing dir)))) ; TODO replace facing with dir


(defn update-walk
  "update walk state"
  [state surfaces time]
  (let [result (-> state
                  (update-speed time)
                  (move-feet-walk surfaces time)
                  (move-hip-walk time)
                  (move-knee-walk time)
                  (move-head-walk time)
                  (move-hand-walk surfaces time)
                  (send-commands))]
    result))
