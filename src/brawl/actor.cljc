(ns brawl.actor
  (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]
            [clojure.spec.alpha :as spec]))

(def MPI2 (* Math/PI 2))


(declare update-jump)
(declare update-walk)
(declare update-idle)
(declare update-rag)
(declare step-feet)


(defn basemetrics-normalize [ {:keys [hitpower hitrate stamina speed height color_a color_b] :as base} mkey]
  (let [half (- 1.25 (* height 0.5)) 
        hp (if (or (= mkey :hitrate) (= mkey :height)) (- half hitrate) hitpower) 
        hr (if (or (= mkey :hitpower) (= mkey :height)) (- half hitpower) hitrate)
        st (if (or (= mkey :speed) (= mkey :height)) (- half speed) stamina)
        sp (if (or (= mkey :stamina) (= mkey :height)) (- half stamina) speed)
        nhp (cond (< hr 0) (- hp hr) (> hr 1.0) (+ hp (- hr 1.0)) :else hp) ; check overflow
        nhr (cond (< hp 0) (- hr hp) (> hp 1.0) (+ hr (- hp 1.0)) :else hr)
        nst (cond (< sp 0) (- st sp) :else st)
        nsp (cond (< st 0) (- sp st) :else sp)]
    (assoc base :hitpower nhp :hitrate nhr :stamina nst :speed nsp)))


(defn basemetrics-default []
  (basemetrics-normalize
   {:height 0.5
    :hitpower 0.5
    :hitrate 0.5
    :stamina 0.5
    :speed 0.5
    :color_a [1.0 0.0 0.0 1.0]
    :color_b [0.0 0.0 1.0 1.0]} :height))


(defn basemetrics-random []
  (basemetrics-normalize
   {:height (/ (rand 10) 10)
    :hitpower (/ (rand 10) 10)
    :hitrate (/ (rand 10) 10)
    :stamina (/ (rand 10) 10)
    :speed (/ (rand 10) 10)
    :color_a [1.0 (rand) (rand) 1.0]
    :color_b [1.0 (rand) (rand) 1.0]} :height))


(defn generate-metrics [{:keys [hitpower hitrate stamina speed height color_a color_b] :as base}]
  (let [hp (cond (> hitpower 1.0) 1.0 (< hitpower 0.0) 0.0 :else hitpower)
        hr (cond (> hitrate 1.0) 1.0 (< hitrate 0.0) 0.0 :else hitrate)
        st (cond (> stamina 1.0) 1.0 (< stamina 0.0) 0.0 :else stamina)
        sp (cond (> speed 1.0) 1.0 (< speed 0.0) 0.0 :else speed)

        size (cond (> height 1.0) 1.0
                   (< height 0.0) 0.0
                   :else height)

        headl (+ 16.0 (* size 8.0))
        bodyl (+ 50.0 (* size 20.0)) 
        arml (+ 50.0 (* size 20.0)) 
        legl (+ 60.0 (* size 20.0)) 

        headw (+ 36.0 (* size 8.0)) 
        neckw (+ 4.0 (* hp 5.0)) 
        armw (+ 4.0 (* hp 7.0)) 
        hipw (+ 6.0 (* st 20.0)) 
        legw (+ 6.0 (* st 5.0)) 
        
        runs (+ 5.0 (* sp 4.0) height)
        walks (* runs 0.5)
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
                :cola color_a :colb [dra dga dba 1.0] :colc color_b :cold [drb dgb dbb 1.0]
                :base base}] ; colors
    result))
  

(defn init [x y id]
  (let [bases {:base_l (phys2/mass2 (+ x 20.0) y 2.0 1.0 0.0 0.0)
               :base_r (phys2/mass2 (- x 20.0) y 2.0 1.0 0.0 0.0)}    
        masses {:head (phys2/mass2 x y 4.0 1.0 0.2 0.7)
                :neck (phys2/mass2 x y 4.0 1.0 0.2 0.7)
                :hip (phys2/mass2 x y 4.0 1.0 0.2 0.7)
                :hand_l (phys2/mass2 x y 4.0 1.0 0.2 0.7)
                :hand_r (phys2/mass2 x y 4.0 1.0 0.2 0.7)
                :elbow_l (phys2/mass2 x y 4.0 1.0 0.2 0.7)
                :elbow_r (phys2/mass2 x y 4.0 1.0 0.2 0.7)
                :knee_l (phys2/mass2 x y 4.0 1.0 0.2 0.7)
                :knee_r (phys2/mass2 x y 4.0 1.0 0.2 0.7)
                :foot_l (phys2/mass2 (+ x 20.0) y 4.0 1.0 0.2 0.7)
                :foot_r (phys2/mass2 (+ x 20.0) y 4.0 1.0 0.2 0.7)}
        metrics (generate-metrics (basemetrics-random))]
    
  {:id id
   :next nil ; next mode walk / jump / idle
   :speed 0.0
   :power 100.0
   :health 100.0
   :facing 1.0
   :update-fn update-jump
   :idle-angle 0
   :action-sent false
   :commands []
   ; walk state
   :bases bases
   :squat-size 0
   :base-order {:active :base_l :passive :base_r}
   :base-target nil
   :base-surfaces {:active nil :passive nil}
   :punch-hand :hand_l
   :punch-press false
   :vert-direction 1
   :kick-press false
   :jump-state 0
   :step-length 0
   ; masses
   :masses masses
   :metrics metrics
   :dguards [
             (phys2/dguard2 masses :head :neck (:headl metrics) 0.0)
             (phys2/dguard2 masses :neck :hip (:bodyl metrics) 0.0)
             (phys2/dguard2 masses :hip :knee_l (* 0.5 (:legl metrics)) 0.0)
             (phys2/dguard2 masses :hip :knee_r (* 0.5 (:legl metrics)) 0.0)
             (phys2/dguard2 masses :knee_l :foot_l (* 0.5 (:legl metrics)) 0.0)
             (phys2/dguard2 masses :knee_r :foot_r (* 0.5 (:legl metrics)) 0.0)
             (phys2/dguard2 masses :neck :elbow_l (* 0.5 (:arml metrics)) 0.0)
             (phys2/dguard2 masses :neck :elbow_r (* 0.5 (:arml metrics)) 0.0)
             (phys2/dguard2 masses :elbow_l :hand_l (* 0.5 (:arml metrics)) 0.0)
             (phys2/dguard2 masses :elbow_r :hand_r (* 0.5 (:arml metrics)) 0.0)
             ]
   :aguards [
             (phys2/aguard2 masses :head :neck :hip 0 Math/PI 0.5)
             (phys2/aguard2 masses :hip :knee_l :foot_l (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
             (phys2/aguard2 masses :hip :knee_r :foot_r (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
             (phys2/aguard2 masses :neck :elbow_r :hand_r (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
             (phys2/aguard2 masses :neck :elbow_l :hand_l (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
             ]
       
   ; debug
   :step-zone [x y]
   ; body metrics
   :randoms (vec (repeatedly 40 #(+ -1.5 (rand 3))))})); random sizes for skin phasing



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


(defn hitpoint [{{:keys [head neck hip hand_l hand_r elbow_l elbow_r knee_l knee_r foot_l foot_r]} :masses health :health :as actor} {:keys [id base target time]}]
  ;; check distance from nect first
  (let [dist (math2/length-v2 (math2/sub-v2 target (:p hip)))]
    (if (and (not= id (:id actor)) (< dist 80.0))
      (let [[hvx hvy :as hitv] (math2/sub-v2 target base)
            hitsm (math2/resize-v2 hitv (if (< health 20) 10 2))
            hitbg (math2/resize-v2 hitv (if (< health 20) 20 4))
            
            headv (math2/sub-v2 (:p neck) (:p head))
            bodyv (math2/sub-v2 (:p hip) (:p neck))
            footlv (math2/sub-v2 (:p knee_l) (:p hip))
            footrv (math2/sub-v2 (:p knee_r) (:p hip))

            headisp (math2/intersect-v2-v2 base hitv (:p head) headv)
            bodyisp (math2/intersect-v2-v2 base hitv (:p neck) bodyv)
            footlisp (math2/intersect-v2-v2 base hitv (:p hip) footlv)
            footrisp (math2/intersect-v2-v2 base hitv (:p hip) footrv)]

            (first (remove nil? [headisp bodyisp footlisp footrisp]))))))
            

(defn hit [{{:keys [head neck hip hand_l hand_r elbow_l elbow_r knee_l knee_r foot_l foot_r] :as masses} :masses health :health metrics :metrics update-fn :update-fn :as actor} {:keys [id base target time] :as command}]
  ;; check distance from nect first
  (let [ [dx dy] (math2/sub-v2 target (:p hip))]
    (if (and (not= id (:id actor)) (< dx 80.0) (< dy 80.0) (not= update-fn update-rag))
      (let [[hvx hvy :as hitv] (math2/sub-v2 target base)
            hitsm (math2/resize-v2 hitv (if (< health 20) 10 2))
            hitbg (math2/resize-v2 hitv (if (< health 20) 20 4))
            
            headv (math2/sub-v2 (:p neck) (:p head))
            bodyv (math2/sub-v2 (:p hip) (:p neck))
            footlv (math2/sub-v2 (:p knee_l) (:p hip))
            footrv (math2/sub-v2 (:p knee_r) (:p hip))

            headisp (math2/intersect-v2-v2 base hitv (:p head) headv)
            bodyisp (math2/intersect-v2-v2 base hitv (:p neck) bodyv)
            footlisp (math2/intersect-v2-v2 base hitv (:p hip) footlv)
            footrisp (math2/intersect-v2-v2 base hitv (:p hip) footrv)

            has-isp (or headisp bodyisp footlisp footrisp)
            neck-part (if bodyisp (math2/length-v2 (math2/sub-v2 bodyisp (:p neck))))
            hip-part  (if bodyisp (math2/length-v2 (math2/sub-v2 bodyisp (:p hip))))
            neck-ratio (if bodyisp (/ hip-part (:bodyl metrics)))
            hip-ratio (if bodyisp (/ neck-part (:bodyl metrics)))

            ;;(let [newmasses (reduce (fn [oldmasses [id mass]] (assoc oldmasses id (assoc mass :d [0 0]))) {} masses)] 

            result (if-not has-isp
                     actor
                     (cond-> actor
                       true (assoc :hittime time)
                       true (assoc :next "rag")
                       true (update :health - 15) 
                       true (update :speed  + (* (/ hvx (Math/abs hvx)) 5.0))
                       ;; true (assoc :masses (reduce (fn [oldmasses [id mass]] (assoc oldmasses id (assoc mass :d [0 0]))) {} masses))
                       headisp (assoc-in [:masses :head :d] (math2/add-v2 (:d head) hitbg))
                       headisp (assoc-in [:masses :neck :d] (math2/add-v2 (:d neck) hitsm))
                       bodyisp (assoc-in [:masses :neck :d] (math2/add-v2 (:d neck) (math2/scale-v2 hitbg (+ 0.4 neck-ratio))))
                       bodyisp (assoc-in [:masses :hip :d] (math2/add-v2 (:d hip) (math2/scale-v2 hitbg (+ 0.4 hip-ratio))))
                       footlisp (assoc-in [:masses :hip :d] (math2/add-v2 (:d hip) hitbg))
                       footlisp (assoc-in [:masses :knee_l :d] (math2/add-v2 (:d knee_l) hitsm))
                       footrisp (assoc-in [:masses :hip :d] (math2/add-v2 (:d hip) hitbg))
                       footrisp (assoc-in [:masses :knee_r :d] (math2/add-v2 (:d knee_r) hitsm))
                       ))]

        result)
      actor)))


(defn move-hand-walk
  "move head point"
  [{:keys [id facing punch-pressed punch-hand action-sent commands speed] {{[hx hy] :p} :hip {[nx ny :as neck] :p} :neck } :masses {{[ax ay] :p} :base_l {[bx by] :p} :base_r} :bases  { arml :arml } :metrics angle :idle-angle :as state}
   {:keys [down up left right punch block]}
   surface
   time]
  (let [nlx (+ (* facing (+ (* arml 0.4 ) (/ (Math/abs (- bx ax )) 8.0 ))) (* (Math/sin angle ) 5.0))
        nrx (- (* facing (- (* arml 0.4 ) (/ (Math/abs (- bx ax )) 8.0 ))) (* (Math/sin angle ) 5.0))
        nly (+ (* arml 0.1 ) (* (Math/cos angle ) 5.0))
        nry (- (* arml 0.14 )(* (Math/cos angle ) 5.0))
        hand_l (cond
                 block [(+ nx (* facing arml 0.3)) (- ny (* arml 0.4))]
                 (and punch-pressed (= punch-hand :hand_l) (not left) (not right)) [(+ nx (* facing arml 0.99)) ny]
                 :else [(+ nx nlx) (+ ny nly)])
        hand_r (cond
                 block [(+ nx (* facing arml 0.3)) (- ny (* arml 0.4))]
                 (and punch-pressed (= punch-hand :hand_r) (not left) (not right)) [(+ nx (* facing arml 0.99)) ny]                 
                 :else [(+ nx nrx) (+ ny nry)])
        elbow_l (triangle_with_bases neck hand_l (* arml 0.5) facing)
        elbow_r (triangle_with_bases neck hand_r (* arml 0.5) facing)
        command (if (and punch-pressed (not action-sent) (not left) (not right)) {:id id :text "attack" :base neck :target (if (= punch-hand :hand_l) hand_l hand_r) :time time}) ]
    (-> state
        (assoc :commands (if command (conj command command) commands))
        (assoc-in [:masses :hand_l :p] hand_l)
        (assoc-in [:masses :hand_r :p] hand_r)
        (assoc-in [:masses :elbow_l :p] elbow_l)
        (assoc-in [:masses :elbow_r :p] elbow_r))))


(defn move-head-jump
  "move head point"
  [{:keys [facing squat-size kick-pressed] {{[hx hy] :p} :hip} :masses { {[ax ay] :p} :base_l {[bx by] :p} :base_r} :bases { legl :legl bodyl :bodyl headl :headl } :metrics :as state}
   {:keys [down up left right]}]
  (let [nx (* facing (/ (Math/abs (- bx ax )) 8.0 )) ; head move forward and backwards when stepping
        nnx (* facing squat-size 0.5) ; head move even forward when squatting
        ny (* squat-size 0.25) ; head should move lower when squatting
        jnx (if kick-pressed (* facing legl -0.3) 0.0) ; kick delta x
        neck [(+ hx nx nnx jnx) (- (+ hy ny) bodyl)]
        head [(+ hx nx nnx jnx) (- (+ hy ny) (+ bodyl headl))]]
    (-> state
        (assoc-in [:masses :neck :p] neck)
        (assoc-in [:masses :head :p] head))))


(defn move-head-walk
  "move head point"
  [{:keys [facing squat-size punch-pressed speed] {{[hx hy] :p} :hip} :masses  {{[ax ay] :p} :base_l {[bx by] :p} :base_r} :bases { legl :legl bodyl :bodyl headl :headl } :metrics :as state}
   {:keys [down up left right]}]
  (let [nx (* facing (+ (* (Math/abs speed) 1.0) (/ (Math/abs (- bx ax )) 15.0 ))) ; head move forward and backwards when stepping
        nnx (* facing squat-size 0.5) ; head move even forward when squatting
        ny (* squat-size 0.25) ; head should move lower when squatting
        pnx (if punch-pressed (* facing 10.0) 0.0)
        neck [(+ hx nx nnx pnx) (- (+ hy ny) bodyl)]
        head [(+ hx nx nnx pnx) (- (+ hy ny) (+ bodyl headl))]]
    (-> state
        (assoc-in [:masses :neck :p] neck)
        (assoc-in [:masses :head :p] head))))


(defn move-hip-jump
  "move hip points, handle jumping"
  [{:keys [next jump-state] {{[hx hy] :p} :hip} :masses  {{[ax ay] :p} :base_l {[bx by] :p} :base_r} :bases { legl :legl } :metrics :as state}
   {:keys [down up]}]
  (let [x (+ ax (/ (- bx ax) 2))
        y (+ ay (/ (- by ay) 2))]
    (assoc-in state [:masses :hip :p] [x (- y (* legl 0.8))])))


(defn move-hip-walk
  "move hip points, handle jumping"
  [{:keys [next jump-state idle-angle facing speed base-order kick-pressed squat-size] {{[hx hy] :p} :hip } :masses  {{[lx ly] :p} :base_l {[rx ry] :p} :base_r} :bases { legl :legl } :metrics :as state}
   {:keys [down up left right run]}]
  (let [cx (if (or (< speed -0.5) (> speed 0.5) (not kick-pressed)) (+ lx (/ (- rx lx) 2)) ; x center of bases when walking
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


(defn get-step-zone
  "gets base collision triangle"
  [[x y] speed]
  (let [size (cond
               (and (> speed -1.0) (<  speed 0.0)) -20.0
               (and (< speed  1.0) (>= speed 0.0))  20.0
               :else (+ (* (/ speed (Math/abs speed)) 40.0) (* speed 8.0)))
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
        collided-top (sort-by first < (phys2/get-colliding-surfaces (:T step-zone) (math2/sub-v2 (:A step-zone) (:T step-zone)) 4.0 surfaces))
        collided-bot (sort-by first < (phys2/get-colliding-surfaces (:A step-zone) (math2/sub-v2 (:M step-zone) (:A step-zone)) 4.0 surfaces))
        collided (cond
                   (and (= vert-direction 1) (not (empty? collided-top))) collided-top
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


(defn move-knee-walk
 [{:keys [masses speed base-order base-target step-length facing] {legl :legl} :metrics :as state}
   time]
  (let [knee_l (triangle_with_bases (get-in masses [:foot_l :p]) (get-in masses [:hip :p]) (/ legl 1.95) facing)
        knee_r (triangle_with_bases (get-in masses [:foot_r :p]) (get-in masses [:hip :p]) (/ legl 1.95) facing)]
    (-> state
        (assoc-in [:masses :knee_l :p] knee_l) 
        (assoc-in [:masses :knee_r :p] knee_r))))


(defn move-feet-jump
  "move active base towards target point"
  [{:keys [id speed base-order base-target step-length facing kick-pressed action-sent commands] {{[hx hy] :p} :hip :as masses } :masses {base_l :base_l base_r :base_r} :bases {legl :legl runs :runs walks :walks} :metrics :as state}
   {:keys [left right up down run]}
   surfaces
   time]
  (let [foot_l (if kick-pressed [(+ hx (* legl facing) -10.0 ) (+ hy (* legl 0.5))] (:p base_l))
        foot_r (if kick-pressed [(+ hx (* legl facing)) (+ hy (* legl 0.5) -10.0)] (:p base_r))
        command (if (and kick-pressed (not action-sent)) {:id id :text "attack" :base [hx hy] :target foot_l :time time})]
    (-> state
        (assoc :commands (if command (conj command command) commands))
        (assoc-in [:masses :foot_l :p] foot_l) 
        (assoc-in [:masses :foot_r :p] foot_r))))


(defn move-feet-walk-still 
  "do kick if needed"
  [{:keys [id bases masses speed base-order base-target step-length facing kick-pressed action-sent commands] {legl :legl runs :runs walks :walks} :metrics :as state}
   {:keys [left right up down run] :as control} 
   surfaces
   time]
  (let [active-base (:active base-order)
        passive-base (:passive base-order)
        [apx apy :as act] (:p (bases active-base)) ; active position
        [ppx ppy :as pas] (:p (bases passive-base)) ; passive position
        kick-point [(+ ppx (* legl facing 1.2)) (- ppy (* legl 1.5))]
        foot_l (if (= :base_l (:active base-order))
                 (if kick-pressed kick-point act)
                 pas) ; final position
        foot_r (if (= :base_r (:active base-order))
                 (if kick-pressed kick-point act)
                 pas)
        command (if (and kick-pressed (not action-sent)) {:id id :text "attack" :base (:p (:hip masses)) :target kick-point :time time})]
    (-> state
        (assoc :commands (if command (conj command command) commands))
        (assoc-in [:masses :foot_l :p] foot_l) 
        (assoc-in [:masses :foot_r :p] foot_r)
        (assoc :base-target nil))))


(defn move-feet-walk
  "move active base towards target point"
  [{:keys [bases masses speed base-order base-target step-length facing kick-pressed] {legl :legl runs :runs walks :walks} :metrics :as state}
   {:keys [left right up down run] :as control} 
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
    (move-feet-walk-still state control surfaces time))) ; stay still or do kick if no speed


(defn update-speed
  "update speed based on pressed buttons"
  [{:keys [speed facing metrics] :as state}
   {:keys [left right up down run]}
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


(defn update-mode
  "if next mode is set, switch to that mode"
  [{:keys [masses next speed update-fn] {hip :hip fl :foot_l fr :foot_r :as masses} :masses  {ba :base_l bb :base_r} :bases :as state}]
  (let [result
        (cond
          (= next nil) state
          (= next "walk")
          (do
            (let [newmasses (reduce (fn [res [id mass]] ; reset mass directions for next rag
                                      (assoc res id (assoc mass :d [0 0])))
                                    masses
                                    masses)]
                                        ; reset walk state
              (cond-> state
                (= update-fn update-jump) (assoc :squat-size (* 0.5 (get-in state [:metrics :legl]))) ; squat when reaching ground
                true (assoc :jump-state 0) ; reset jump state
                true (assoc :next nil)
                true (assoc :base-target nil) ; reset stepping
                true (assoc :update-fn update-walk)
                true (assoc :masses newmasses))))
          (= next "jump")
          (let [[hx hy] (:p hip)
                [lx ly] (:p ba)
                [rx ry] (:p bb)]
            ;; reset jump state
            (cond-> state
              (= update-fn update-walk) (assoc-in [:bases :base_l :d] [(/ speed 2) -10])
              (= update-fn update-walk) (assoc-in [:bases :base_r :d] [(/ speed 2) -10])         
              true (assoc :next nil)
              true (assoc :update-fn update-jump)))

          (= next "rag")
          ;; reset jump state
          (-> state
              (assoc :next nil)
              (assoc :update-fn update-rag))

          (= next "idle")
          ;; reset jump state
          (-> state
              (assoc :next nil)
              (assoc :update-fn update-idle))

          :else
          (do
            (println "bad mode" next)
            state)
          )]
    (if (= result nil) println "UPDATEMODE ERROR!!!")
    result
    ))


(defn update-jump
  "jump state update, update base masses in the world, check if they reached ground" 
  [{:keys [bases masses speed] :as state}
   {:keys [left right up down] :as control}
   surfaces
   time]
  (let [newbases (-> bases 
                     (phys2/add-gravity [0.0 0.5])
                     (phys2/move-masses surfaces 0.5))
        ground (and (:q (:base_l newbases) (:base_r newbases))) ;; check quisence of bases
        next (cond
               ground "walk"
               ;(< speed -15) "idle"
               :else nil)
        result (cond-> state
                 next (assoc :next next)
                 true (assoc :bases newbases)
                 true (move-hip-jump control)
                 true (move-feet-jump control surfaces time)
                 true (move-knee-walk control)
                 true (move-head-jump control)
                 true (move-hand-walk control surfaces time))]
    (if (= result nil) println "UPDATEJUMP ERROR!!!")
    result))


(defn update-walk
  "update walk state"
  [state control surfaces time]
  (let [result (-> state
                  (update-speed control time)
                  (move-feet-walk control surfaces time)
                  (move-hip-walk control)
                  (move-knee-walk control)
                  (move-head-walk control)
                  (move-hand-walk control surfaces time))]
    (if (= result nil) println "UPDATEWALK ERROR!!!")
    result))


(defn update-idle [state] state)


(defn update-rag [{:keys [masses dguards hittime next health] :as state } control surfaces time]
  (if (> health 0)
    (let [newmasses (-> masses
                        (phys2/add-gravity [0.0 0.4])
                        (phys2/keep-angles (:aguards state))
                        (phys2/keep-distances (:dguards state))
                        (phys2/move-masses surfaces 0.4))
          newnext (if (= hittime 20) "jump" next) 
          result (-> state
                     (assoc :next newnext)
                     (assoc :masses newmasses)
                     (update :hittime inc))]

      (if (= result nil) println "UPDATERAG ERROR!!!")
      result)
    (let [newmasses (-> masses
                        (phys2/add-gravity [0.0 0.4])
                        (phys2/keep-angles (:aguards state))
                        (phys2/keep-distances (:dguards state))
                        (phys2/move-masses surfaces 0.4))
          newnext (if (= hittime 150) "idle" next) 
          result (-> state
                     (assoc :next newnext)
                     (assoc :masses newmasses)
                     (update :hittime inc))]
      (if (= result nil) println "UPDATERAG ERROR!!!")
      result)))
  

(defn update-attack [{:keys [punch-pressed punch-hand kick-pressed action-sent vert-direction] :as state} {:keys [left right up down punch kick block] :as control}]
  (let [p-pressed (cond
                  (and (not punch-pressed) punch) true
                  (and punch-pressed (not punch)) false
                  :else punch-pressed)
        k-pressed (cond
                  (and (not kick-pressed) kick) true
                  (and kick-pressed (not kick)) false
                  :else kick-pressed)
        p-hand (if (and (not punch-pressed) punch)
               (if (= punch-hand :hand_l) :hand_r :hand_l)
               punch-hand)]
    (-> state
        (assoc :action-sent (if (and (not p-pressed) (not k-pressed)) false action-sent))
        (assoc :punch-pressed p-pressed)
        (assoc :punch-hand p-hand)
        (assoc :kick-pressed k-pressed)
        (assoc :vert-direction (cond
                                 down -1
                                 up 1
                                 :else vert-direction))
        )))


(defn update-angle
  [{angle :idle-angle :as state}]
  (if (> angle MPI2)
    (assoc state :idle-angle (- angle MPI2))
    (update state :idle-angle + 0.05)))


(defn update-actor [{mode :mode update-fn :update-fn :as state} control surfaces time]
  "update actor state"
  ;;(if (= (:id state) :enemy) (println "BEFORE UPDATE" (:version state) (get-in state [:masses :knee_l :d] )))
  (let [result (-> state
                   (update-angle)
                   (update-attack control)
                   (update-fn control surfaces time)
                   (update-mode)
                   )]
    ;;(if (= (:id state) :enemy) (println "AFTER UPDATE" (:version result) (get-in result [:masses :knee_l :d] )))
    result))
