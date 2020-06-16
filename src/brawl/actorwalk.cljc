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

;; TODO too long
(defn send-commands
  "send commands if needed"
  [actor]
  (let [{:keys [id commands color facing]} actor
        {:keys [gun body]} (:drag actor)
        {:keys [order]} (:walk actor)
        {:keys [neck hip hand_l hand_r foot_l foot_r]} (:masses actor)
        {:keys [bullets punch-hand pickup-sent action-sent]} (:attack actor) 
        {:keys [left right down punch shoot kick]} (:control actor)]
    (cond
      ;; pick up gun or body
      (and down (or (= nil gun) (= nil body)) (not pickup-sent))
      (-> actor
          (update :commands into [{:text "pickup" :id id}])
          (assoc-in [:attack :pickup-sent] true))
      ;; send punch
      (and punch (not action-sent) (not left) (not right))
      (-> actor
          (update :commands into [{:id id
                                   :text "attack"
                                   :base (:p neck)
                                   :target (if (= punch-hand :hand_l) (:p hand_l) (:p hand_r))
                                   :radius 100.0
                                   :facing facing
                                   :color color
                                   :power 20}])
          (assoc-in [:attack :action-sent] true))
      ;; send shoot
      (and shoot (not action-sent) (> bullets 0))
      (-> actor
          (update :commands into [{:id id
                                   :text "attack"
                                   :base (:p neck)
                                   :target (math2/add-v2 (:p neck) [(* facing 500.0) 0.0])
                                   :radius 500.0
                                   :facing facing
                                   :color color
                                   :power 110}])
          (assoc-in [:attack :action-sent] true)
          (assoc-in [:attack :bullets] (dec bullets)))
      ;; send kick
      (and kick (not action-sent))
      (-> actor
          (update :commands into [{:id id
                                   :text "attack"
                                   :base (:p hip)
                                   :target (if (= :base_l (:active order)) (:p foot_l) (:p foot_r))
                                   :facing facing
                                   :radius 100.0
                                   :color color
                                   :power 40}])
          (assoc-in [:attack :action-sent] true))
      ;; return untouched
      :else actor)))


(defn move-head-walk
  "move head point"
  [actor]
  (let [{:keys [facing speed]} actor
        {:keys [punch]} (:control actor)
        {:keys [squat-size]} (:walk actor)
        {:keys [bodyl headl]} (:metrics actor)
        {{[hx hy] :p} :hip} (:masses actor)
        {{[ax ay] :p} :base_l {[bx by] :p} :base_r} (:bases actor)
        nx (* facing (+ (* (Math/abs speed) 1.0) (/ (Math/abs (- bx ax )) 15.0 ))) ; head move forward and backwards when stepping
        nnx (* facing squat-size 0.5) ; head move even forward when squatting
        ny (* squat-size 0.25) ; head should move lower when squatting
        pnx (if punch (* facing 10.0) 0.0)
        neck [(+ hx nx nnx pnx) (- (+ hy ny) bodyl)]
        head [(+ hx nx nnx pnx) (- (+ hy ny) (+ bodyl headl))]]
    (-> actor
        (assoc-in [:masses :neck :p] neck)
        (assoc-in [:masses :head :p] head))))


(defn move-hand-walk
  "move head point"
  [actor surfaces]
  (let [{:keys [facing]} actor
        {:keys [punch-hand punch-y]} (:attack actor)
        {:keys [left right punch shoot block]} (:control actor)
        {{[hx hy] :p} :hip {[nx ny :as neck] :p} :neck } (:masses actor)
        {{[ax ay] :p} :base_l {[bx by] :p} :base_r} (:bases actor)
        {arml :arml} (:metrics actor)
        {angle :idle-angle} (:walk actor)

        nlx (+ (* facing (+ (* arml 0.4 ) (/ (Math/abs (- bx ax )) 8.0 ))) (* (Math/sin angle ) 5.0))
        nrx (- (* facing (- (* arml 0.4 ) (/ (Math/abs (- bx ax )) 8.0 ))) (* (Math/sin angle ) 5.0))
        nly (+ (* arml 0.1 ) (* (Math/cos angle ) 5.0))
        nry (- (* arml 0.14 )(* (Math/cos angle ) 5.0))
        
        hand_l (cond
                 block [(+ nx (* facing arml 0.3)) (- ny (* arml 0.4))]
                 (or shoot (and punch (= punch-hand :hand_l) (not left) (not right))) [(+ nx (* facing arml 0.99)) (+ ny punch-y)]
                 :else [(+ nx nlx) (+ ny nly)])
        hand_r (cond
                 block [(+ nx (* facing arml 0.3)) (- ny (* arml 0.4))]
                 (and punch (= punch-hand :hand_r) (not left) (not right)) [(+ nx (* facing arml 0.99)) (+ ny punch-y)]
                 :else [(+ nx nrx) (+ ny nry)])

        elbow_l (triangle_with_bases neck hand_l (* arml 0.5) facing)
        elbow_r (triangle_with_bases neck hand_r (* arml 0.5) facing)]

    (-> actor
        (assoc-in [:masses :hand_l :p] hand_l)
        (assoc-in [:masses :hand_r :p] hand_r)
        (assoc-in [:masses :elbow_l :p] elbow_l)
        (assoc-in [:masses :elbow_r :p] elbow_r))))


(defn move-hip-walk
  "move hip points, handle jumping"
  [actor]
  (let [{:keys [next-mode facing speed]} actor
        {:keys [order squat-size jump-state idle-angle]} (:walk actor)
        {:keys [up down run kick]} (:control actor)
        {{[hx hy] :p} :hip } (:masses actor)
        {{[lx ly] :p} :base_l {[rx ry] :p} :base_r} (:bases actor)
        {legl :legl } (:metrics actor)
        cx (if (or (< speed -0.5) (> speed 0.5) (not kick)) (+ lx (/ (- rx lx) 2)) ; x center of bases when walking
               (if (= :base_l (:active order)) rx lx)) ; passive foot when kic
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
        fy (+ sty squat-size) ; final 
        newstate (cond
                (and up (= jump-state 0) (> squat-size (* legl 0.4))) 1
                (and up (= jump-state 1) (< squat-size 1.0)) 2
                :else jump-state)
        newnext (if (= newstate 2) :jump next-mode)]
    (-> actor
        (assoc :next-mode newnext)
        (assoc-in [:masses :hip :p] [fx fy])
        (assoc-in [:walk :squat-size] squat-size)
        (assoc-in [:walk :jump-state] newstate))))


(defn move-knee-walk
  [actor]
  (let [{:keys [masses speed facing]} actor
        {legl :legl} (:metrics actor)
        {base-order :order base-target :target} (:walk actor)
        knee_l (triangle_with_bases (get-in masses [:foot_l :p]) (get-in masses [:hip :p]) (/ legl 1.95) facing)
        knee_r (triangle_with_bases (get-in masses [:foot_r :p]) (get-in masses [:hip :p]) (/ legl 1.95) facing)]
    (-> actor
        (assoc-in [:masses :knee_l :p] knee_l) 
        (assoc-in [:masses :knee_r :p] knee_r))))


(defn move-feet-walk-still 
  "do kick if needed"
  [actor surfaces]
  (let [{:keys [bases facing]} actor
        {:keys [order]} (:walk actor)
        {legl :legl runs :runs walks :walks} (:metrics actor)
        {:keys [kick-y action-sent]} (:attack actor)
        {kick :kick} (:control actor)

        active-base (:active order)
        passive-base (:passive order)
        
        [apx apy :as act] (:p (active-base bases)) ; active position
        [ppx ppy :as pas] (:p (passive-base bases)) ; passive position
        kick-point [(+ ppx (* legl facing 1.2)) (+ (- ppy (* legl 1.5)) kick-y)]
        foot_l (if (= :base_l active-base)
                 (if kick kick-point act)
                 pas) ; final position
        foot_r (if (= :base_r active-base)
                 (if kick kick-point act)
                 pas)]
    (-> actor
        (assoc-in [:masses :foot_l :p] foot_l) 
        (assoc-in [:masses :foot_r :p] foot_r)
        (assoc-in [:walk :target] nil))))


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

;; TODO too long
(defn step-feet-walk
  "puts a triangle from the passive base on the surfaces, collision ponit is the new base target for the active base"
  [actor surfaces time]
  ; speed must not be 0
  (let [{:keys [bases masses speed]} actor
        {:keys [vert-direction] base-surfaces :surfaces} (:walk actor)

        base-order (get-base-order bases speed)
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
        newactivesurf (if surf (nth surf 2) nil)
        newslope (if (= nil newactivesurf)
                   0
                   (math2/rad-to-degree (math2/angle-x-v2 (:b (nth surf 2)))))]

    (if (and (< newslope 50) (> newslope -50))
      ;; slope is okay
      (if (and (= nil newpassivesurf) (empty? collided))
        ;; we have no surface under feet, fall
        (-> actor
            (assoc :next-mode :rag)
            (assoc-in [:attack :timeout] time)
            (assoc :health -1))
        ;; normal step
        (-> actor
            (assoc-in [:walk :zone] {:A (:A step-zone)
                                     :B (math2/add-v2 (:A step-zone)(:B step-zone))
                                     :C (math2/add-v2 (:A step-zone)(:C step-zone))})
            (assoc-in [:walk :step-size] (math2/length-v2 (math2/sub-v2 base-target (:p (bases (:active base-order))))))
            (assoc-in [:walk :order] base-order)
            (assoc-in [:walk :target] base-target)
            (assoc-in [:walk :surfaces] {:active newactivesurf :passive (or newpassivesurf newactivesurf)})))
      ;; too steep slope, stop stepping
      (-> actor
          (assoc :speed 0.0)))))

;; TODO too long
(defn move-feet-walk
  "move active base towards target point"
  [actor surfaces time delta]
  (let [{:keys [bases masses speed facing kick-pressed]} actor
        {:keys [step-size order target]} (:walk actor)
        {:keys [legl runs walks]} (:metrics actor)
        {:keys [kick]} (:control actor)]
    (if (> (Math/abs speed) 1.0)
      (if target
        (let [active-base (:active order)
              passive-base (:passive order)
              actual-pos (:p (bases active-base))
              actual-vec (math2/sub-v2 target actual-pos)
              actual-size (math2/length-v2 actual-vec)
              current-size (* (Math/abs speed) delta)
              current-vec (math2/resize-v2 actual-vec current-size)
              remaining-size (- actual-size current-size)
              [cpx cpy :as current-pos] (math2/add-v2 actual-pos current-vec) ; current position
              [ppx ppy] (:p (bases passive-base)) ; passive position
              walk-lift-ratio (if (< (/ step-size 2) remaining-size); when walking, highest foot position is center TODO simplify 
                                (/ (- step-size remaining-size) step-size) 
                                (/ remaining-size step-size))
              run-lift-ratio-passive  (if (< remaining-size (/ step-size 3)) ; when running. highest foot position is third for passive
                                        (/ (- (/  step-size 3.0) remaining-size ) ( / step-size 3.0 ))
                                        0.0)
              run-lift-ratio-active (/ remaining-size step-size) ; active foot falling is linear to target point
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
              foot_l (if (= :base_l (:active order))
                       [cpx (- cpy lift-active)]
                       [ppx (- ppy lift-passive)]) ; final position
              foot_r (if (= :base_r (:active order))
                       [cpx (- cpy lift-active)]
                       [ppx (- ppy lift-passive)])
              step? (if (< actual-size current-size) true false)] ; do we need step
          (cond-> actor
            true (assoc-in [:bases active-base :p] current-pos)
            true (assoc-in [:masses :foot_l :p] foot_l) 
            true (assoc-in [:masses :foot_r :p] foot_r)
            step? (step-feet-walk surfaces time))) ; step if base target is close
        (step-feet-walk actor surfaces time)) ; step if no base target
      (move-feet-walk-still actor surfaces)))) ; stay still or do kick if no speed


(defn update-speed
  "update speed based on pressed buttons"
  [actor delta]
  (let [{:keys [speed facing metrics]} actor
        {:keys [left right up down run block]} (:control actor)
        max (if (or (not run) down) (:walks metrics) (:runs metrics))
        nsx (cond
              right (if (> speed max)
                      (+ max 0.1)
                      (+ speed (* 0.8 delta)))
              left (if (< speed (- max))
                      (- (- max) 0.1)
                      (- speed (* 0.8 delta)))
              :else (* speed (- 1.0 (* 0.15 delta))))
        dir (cond
              (and (> nsx 0.0) right (not block)) 1
              (and (< nsx 0.0) left (not block)) -1
              :else facing)]
    (-> actor
        (assoc :speed nsx)
        (assoc :facing dir)))) ; TODO replace facing with dir


(defn update-walk
  "update walk state"
  [state surfaces time delta]
  (let [result (-> state
                  (update-speed delta)
                  (move-feet-walk surfaces time delta)
                  (move-hip-walk)
                  (move-knee-walk)
                  (move-head-walk)
                  (move-hand-walk surfaces)
                  (send-commands))]
    result))
