(ns brawl.actor
  (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]
            [brawl.metrics :as metrics]
            [brawl.actorai :as ai]
            [brawl.actorjump :as jump]
            [brawl.actorwalk :as walk]
            [clojure.spec.alpha :as spec]))

(def MPI2 (* Math/PI 2))
(declare update-rag)


(defn int-to-rgba [color]
  (let [r (/ (float (bit-and (bit-shift-right color 24) 0xFF)) 255.0)
        g (/ (float (bit-and (bit-shift-right color 16) 0xFF)) 255.0)
        b (/ (float (bit-and (bit-shift-right color 8) 0xFF)) 255.0)
        a (/ (float (bit-and color 0xFF)) 255.0)]
    [r g b a]))


(defn default-bases [x y]
  {:base_l (phys2/mass2 (+ x 20.0) y 2.0 1.0 0.0 0.0)
   :base_r (phys2/mass2 (- x 20.0) y 2.0 1.0 0.0 0.0)})


(defn default-masses [x y]
  {:head    (phys2/mass2 x y 4.0 1.0 0.5 0.6)
   :neck    (phys2/mass2 x y 4.0 1.0 0.5 0.6)
   :hip     (phys2/mass2 x y 4.0 1.0 0.5 0.6)
   :hand_l  (phys2/mass2 x y 4.0 1.0 0.5 0.6)
   :hand_r  (phys2/mass2 x y 4.0 1.0 0.5 0.6)
   :elbow_l (phys2/mass2 x y 4.0 1.0 0.5 0.6)
   :elbow_r (phys2/mass2 x y 4.0 1.0 0.5 0.6)
   :knee_l  (phys2/mass2 x y 4.0 1.0 0.5 0.6)
   :knee_r  (phys2/mass2 x y 4.0 1.0 0.5 0.6)
   :foot_l  (phys2/mass2 (+ x 20.0) y 4.0 1.0 0.5 0.6)
   :foot_r  (phys2/mass2 (+ x 20.0) y 4.0 1.0 0.5 0.6)})


(defn default-distance-guards [masses metrics]
  [(phys2/dguard2 masses :head :neck (:headl metrics) 0.0)
   (phys2/dguard2 masses :neck :hip (:bodyl metrics) 0.0)
   (phys2/dguard2 masses :neck :elbow_l (* 0.5 (:arml metrics)) 0.0)
   (phys2/dguard2 masses :neck :elbow_r (* 0.5 (:arml metrics)) 0.0)
   (phys2/dguard2 masses :elbow_l :hand_l (* 0.5 (:arml metrics)) 0.0)
   (phys2/dguard2 masses :elbow_r :hand_r (* 0.5 (:arml metrics)) 0.0)
   (phys2/dguard2 masses :hip :knee_l (* 0.5 (:legl metrics)) 0.0)
   (phys2/dguard2 masses :hip :knee_r (* 0.5 (:legl metrics)) 0.0)
   (phys2/dguard2 masses :knee_l :foot_l (* 0.5 (:legl metrics)) 0.0)
   (phys2/dguard2 masses :knee_r :foot_r (* 0.5 (:legl metrics)) 0.0)])


(defn default-angle-guards [masses]
  [(phys2/aguard2 masses :head :neck :hip 0 Math/PI 0.5)
   (phys2/aguard2 masses :hip :knee_l :foot_l (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
   (phys2/aguard2 masses :hip :knee_r :foot_r (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
   (phys2/aguard2 masses :neck :elbow_r :hand_r (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
   (phys2/aguard2 masses :neck :elbow_l :hand_l (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)])


(defn default-control []
  {:left false
   :right false
   :up false
   :down false
   :punch false
   :kick false
   :block false
   :run false })


(defn init
  "create new actor data"
  [x y id color basemetrics level]
  (let [metrics (metrics/generate-metrics basemetrics)
        bases (default-bases x y)
        masses (default-masses x y)
        dguards (default-distance-guards masses metrics)
        aguards (default-angle-guards masses)]
    {;; base properties
     :id id
     :color color
     :level level ;; used for health level/hitpower calculation
     :metrics metrics
     ;; physics
     :bases bases
     :masses masses
     :dguards dguards
     :aguards aguards
     ;; internal state
     :curr-mode :jump
     :next-mode nil
     :health (+ 100.0 (* level 50.0))
     :bullets 0
     :commands [] ;; command collector
     :control (default-control)
     :pickup-sent false
     :action-sent false
     :is-dragged false
     :injure-when-dropped false
     ;; ai state
     :ai-state :idle
     :ai-target nil
     :ai-timeout 0
     ;; movement
     :speed -2.0
     :facing 1.0
     :idle-angle 0
     :vert-direction 1
     :squat-size 0
     :base-order {:active :base_l :passive :base_r}
     :base-target nil
     :base-surfaces {:active nil :passive nil}
     :punch-hand :hand_l
     :punch-y 0
     :kick-y 0
     :jump-state 0
     :step-length 0
     ;; dragged objects
     :dragged-gun nil
     :dragged-body nil
     ;; skin drawing related
     :colorf (int-to-rgba color)
     :randoms (vec (repeatedly 40 #(+ -1.5 (rand 3))))
     :step-zone [x y]}))


(defn update-dragged [dragged
                      {{:keys [head hip neck]} :masses :as dragger}
                      time]
  (-> dragged
    (assoc :hittimeout (+ time 5000))
    (assoc-in [:masses :hip :d] [0 0])
    (assoc-in [:masses :hip :p] (math2/add-v2 (:p head) [-20 0] ))
    (assoc-in [:masses :neck :d] [0 0])
    (assoc-in [:masses :neck :p] (math2/add-v2 [-20 0] (math2/add-v2 (:p head)(math2/rotate-90-ccw (math2/sub-v2 (:p neck) (:p hip))))))))


(defn update-gun [{s :s :as gun}
                  {{hand_l :hand_l elbow_l :elbow_l} :masses
                   :keys [bullets facing commands action-sent]
                   {:keys [shoot]} :control :as actor}]
  (assoc gun
         :p (:p hand_l)
         :f (- facing)
         :d (if shoot [(* facing 100.0) 0.0] (math2/sub-v2 (:p hand_l) (:p elbow_l)))
         :s (if (and shoot (> bullets 0)) (inc s) 0)))


(defn check-death [{:keys [health] :as actor}]
  (if (<= health 0) (assoc actor :next-mode :rag) actor))


(defn calc-isps [{:keys [head neck hip knee_l knee_r foot_l foot_r]} base target]
  (let [[hvx hvy :as hitv] (math2/sub-v2 target base)
        
        headv (math2/sub-v2 (:p neck) (:p head))
        bodyv (math2/sub-v2 (:p hip) (:p neck))
        footlv (math2/sub-v2 (:p knee_l) (:p hip))
        footrv (math2/sub-v2 (:p knee_r) (:p hip))
        
        headisp (math2/intersect-v2-v2 base hitv (:p head) headv)
        bodyisp (math2/intersect-v2-v2 base hitv (:p neck) bodyv)
        footlisp (math2/intersect-v2-v2 base hitv (:p hip) footlv)
        footrisp (math2/intersect-v2-v2 base hitv (:p hip) footrv)]
    [headisp bodyisp footlisp footrisp]))


(defn hitpoint
  "get hitpoint"
  [{:keys [id color curr-mode]
    {:keys [hip] :as masses} :masses}
   {:keys [base target radius] :as enemy}]
  
  (let [[dx dy] (math2/sub-v2 base (:p hip))
        nearby? (and (< dx radius) (< dy radius))
        not-rag? (not= curr-mode :rag)
        diff-actor? (not= id (:id enemy))
        diff-color? (not= color (:color enemy))]

    (if (and diff-actor? diff-color? nearby? not-rag?)
      (let [isps (calc-isps masses base target)]        
        (first (remove nil? isps))))))


(defn hit
  "hit actor"
  [{:keys [id health color metrics facing curr-mode]
    {:keys [head neck hip hand_l hand_r elbow_l elbow_r knee_l knee_r foot_l foot_r] :as masses} :masses
    {:keys [block] :as control} :control col :color :as actor}
   {:keys [base target power radius facing] :as command}
   time]

  (let [[dx dy] (math2/sub-v2 base (:p hip))
        alive? (> health 0)
        nearby? (and (< dx radius) (< dy radius))
        not-rag? (not= curr-mode :rag)
        diff-actor? (not= id (:id command))
        diff-color? (not= color (:color command))
        diff-facing? (not= facing (:facing command))]

    (if-not (and diff-actor? diff-color? nearby? not-rag? alive?)
      actor
      (let [[headisp bodyisp footlisp footrisp] (calc-isps masses base target)
            [hvx hvy :as hitv] (math2/sub-v2 target base)
            hitsm (math2/resize-v2 hitv 10)
            hitbg (math2/resize-v2 hitv 20)
            has-isp (or headisp bodyisp footlisp footrisp)]

        (if-not has-isp
          ;; no intersection, leave actor untouched
          actor
          ;; intersection, check facing and blocking
          (if (and block diff-facing?)
            ;; move actor slightly when blocking and facing the sender
            (-> actor
                (assoc :hittimeout (+ time 1000))
                (update :health - (/ power 2.0)) 
                (update :speed + (* facing (/ power 2.0)))
                (check-death))
            ;; hit actor
            (let [neck-part (if bodyisp (math2/length-v2 (math2/sub-v2 bodyisp (:p neck))))
                  hip-part  (if bodyisp (math2/length-v2 (math2/sub-v2 bodyisp (:p hip))))
                  neck-ratio (if bodyisp (/ hip-part (:bodyl metrics)))
                  hip-ratio (if bodyisp (/ neck-part (:bodyl metrics)))

                  hitpower (cond
                             headisp power
                             bodyisp (* power 0.6)
                             footlisp (* power 0.4)
                             footrisp (* power 0.4))]

              (cond-> actor
                true (assoc :hittimeout (cond
                                          (= curr-mode :jump) (+ time 2000)
                                          (and headisp (> hitpower 39)) (+ time 1000)
                                          (< health hitpower) (+ time 2000)
                                          :else (+ time 200)))
                true (assoc :next-mode :rag)
                true (update :health - hitpower) 
                true (update :speed + (* (/ hvx (Math/abs hvx)) 5.0))
                ;; TODO REFACTOR
                (< (- health hitpower) 0) (assoc :dragged-body nil)
                (and (= (:id actor) :hero ) (< (- health hitpower) 0 )) (update :commands conj {:text "show-wasted"})
                ;; true (assoc :masses (reduce (fn [oldmasses [id mass]] (assoc oldmasses id (assoc mass :d [0 0]))) {} masses))
                headisp (assoc-in [:masses :head :d] (math2/add-v2 (:d head) hitbg))
                headisp (assoc-in [:masses :neck :d] (math2/add-v2 (:d neck) hitsm))
                bodyisp (assoc-in [:masses :neck :d] (math2/add-v2 (:d neck) (math2/scale-v2 hitbg (+ 0.4 neck-ratio))))
                bodyisp (assoc-in [:masses :hip :d] (math2/add-v2 (:d hip) (math2/scale-v2 hitbg (+ 0.4 hip-ratio))))
                footlisp (assoc-in [:masses :hip :d] (math2/add-v2 (:d hip) hitbg))
                footlisp (assoc-in [:masses :knee_l :d] (math2/add-v2 (:d knee_l) hitsm))
                footrisp (assoc-in [:masses :hip :d] (math2/add-v2 (:d hip) hitbg))
                footrisp (assoc-in [:masses :knee_r :d] (math2/add-v2 (:d knee_r) hitsm))))))))))
  

(defn update-idle [state] state)


(defn update-rag [{:keys [masses dguards hittimeout next-mode health injure-when-dropped is-dragged commands id] :as state } surfaces time delta]
  (if (> health 0)
    (let [newmasses (-> masses
                        (phys2/add-gravity [0.0 0.4])
                        ;;(phys2/keep-angles (:aguards state))
                        (phys2/keep-distances (:dguards state))
                        (phys2/move-masses surfaces 0.4))
          newnext (if (> time hittimeout) :walk next-mode) 
          result (-> state
                     (assoc :next-mode newnext)
                     (assoc :masses newmasses))]
      (if (= result nil) println "UPDATERAG ERROR!!!")
      result)
    
    (let [newmasses (-> masses
                        (phys2/add-gravity [0.0 0.4])
                        ;;(phys2/keep-angles (:aguards state))
                        (phys2/keep-distances (:dguards state))
                        (phys2/move-masses (if is-dragged [] surfaces) 0.4))
          finished (or (> time hittimeout) (and (:q (:neck newmasses)) (:q (:hip newmasses))))
          newnext (if finished :idle next-mode) 
          newcommands (if-not (and injure-when-dropped (= 0 (mod (int (* time 10.0)) 2)))
                        commands
                        (into commands [{:id id
                                         :text "attack"
                                         :base (get-in masses [:neck :p])
                                         :target (get-in masses (math2/resize-v2 [:neck :d] 10.0))
                                         :facing 0
                                         :radius 50.0
                                         :power 50}]))
          result (-> state
                     (assoc :commands newcommands)
                     (assoc :next-mode newnext)
                     (assoc :masses newmasses))]

      (if (= result nil) println "UPDATERAG ERROR!!!")
      result)))


(defn update-mode
  "if next mode is set, switch to that mode"
  [{:keys [masses next-mode speed curr-mode] {hip :hip fl :foot_l fr :foot_r :as masses} :masses  {ba :base_l bb :base_r} :bases :as state} surfaces]
  (let [result
        (cond
          
          (= next-mode nil) state

          (= next-mode :walk)
          (let [newmasses (reduce (fn [res [id mass]] ; reset mass directions for next rag
                                    (assoc res id (assoc mass :d [0 0])))
                                  masses
                                  masses)
                newfeetpoint (sort-by first (phys2/get-intersecting-surfaces (:p hip) [0 400] surfaces))
                finalpoint (if-not (empty? newfeetpoint)
                             (second (first newfeetpoint))
                             (:p ba))]
                                        ; reset walk state
            (cond-> state
              (= curr-mode :rag) (assoc-in [:bases :base_l :p] finalpoint)
              (= curr-mode :rag) (assoc-in [:bases :base_r :p] finalpoint)
              (= curr-mode :rag) (assoc :squat-size (* 1.0 (get-in state [:metrics :legl]))) ; squat when reaching ground
              (= curr-mode :jump) (assoc :squat-size (* 0.5 (get-in state [:metrics :legl]))) ; squat when reaching ground
              true (assoc :jump-state 0) ; reset jump state
              true (assoc :next-mode nil)
              true (assoc :base-target nil) ; reset stepping
              true (assoc :curr-mode :walk)
              true (assoc :masses newmasses)))

          (= next-mode :jump)
          (let [[hx hy] (:p hip)
                [lx ly] (:p ba)
                [rx ry] (:p bb)]
            ;; reset jump state
            (cond-> state
              (= curr-mode :walk) (assoc-in [:bases :base_l :p] [(- hx 10.0) (- (min ly ry) 10.0)])
              (= curr-mode :walk) (assoc-in [:bases :base_r :p] [(+ hx 10.0) (- (min ly ry) 10.0)])
              (= curr-mode :walk) (assoc-in [:bases :base_l :d] [(/ speed 2) -10])
              (= curr-mode :walk) (assoc-in [:bases :base_r :d] [(/ speed 2) -10])         
              true (assoc :next-mode nil)
              true (assoc :curr-mode :jump)))

          (= next-mode :rag)
          ;; reset jump state
          (-> state
              (assoc :next-mode nil)
              (assoc :curr-mode :rag))
          
          (= next-mode :idle)
          ;; reset jump state
          (-> state
              (assoc :is-dragged false)
              (assoc :injure-when-dropped false)
              (assoc :next-mode nil)
              (assoc :curr-mode :idle))

          :else
          (do
            (println "bad mode" next-mode)
            state))]
    (if (= result nil) println "UPDATEMODE ERROR!!!")
    result))


(defn update-fn [{mode :curr-mode :as state} surfaces time delta]
  (case mode
    :rag (update-rag state surfaces time delta)
    :idle (update-idle state)
    :walk (walk/update-walk state surfaces time delta)
    :jump (jump/update-jump state surfaces time delta)
    state))


(defn update-controls [{:keys [punch-hand action-sent pickup-sent vert-direction] self-control :control :as state} {:keys [left right up down punch kick shoot block run] :as control}]
  (if-not control
    state
    (let [p-hand (if (and (not (:punch self-control)) punch)
                   (if (= punch-hand :hand_l) :hand_r :hand_l)
                   punch-hand)
          punch-y (if (and (not (:punch self-control)) punch) (rand 15) (:punch-y state))
          kick-y (if (and (not (:kick self-control)) kick) (rand 30) (:kick-y state))]
      (-> state
          (assoc :action-sent (if (and (not punch) (not kick) (not shoot)) false action-sent))
          (assoc :pickup-sent (if (not down) false pickup-sent))
          (assoc :punch-hand p-hand)
          (assoc :vert-direction (cond
                                   down -1
                                   up 1
                                   :else vert-direction))
          (assoc :punch-y punch-y)
          (assoc :kick-y kick-y)
          (assoc-in [:control :punch] punch)
          (assoc-in [:control :kick] kick)
          (assoc-in [:control :shoot] shoot)
          (assoc-in [:control :left] left)
          (assoc-in [:control :right] right)
          (assoc-in [:control :up] up)
          (assoc-in [:control :down] down)
          (assoc-in [:control :block] block)
          (assoc-in [:control :run] run)))))


(defn update-angle
  [{angle :idle-angle health :health :as state} delta]
  (cond-> state
    (and (< health 100.0) (> health 0.0)) (update :health + (* 0.05 delta)) 
    (> angle MPI2) (assoc :idle-angle (- angle MPI2))
    (< angle MPI2) (update :idle-angle + (* 0.05 delta))))


(defn update-actor [state control surfaces actors guns time delta]
  "update actor state"
  ;;(if (= (:id state) :enemy) (println "BEFORE UPDATE" (:version state) (get-in state [:masses :knee_l :d] )))
  (let [result (-> state
                   (update-angle delta)
                   (update-controls control) ;; manual controls for hero
                   (ai/update-ai control surfaces actors time delta) ;; ai controls for others
                   (update-fn surfaces time delta)
                   (update-mode surfaces))]
    ;;(if (= (:id state) :enemy) (println "AFTER UPDATE" (:version result) (get-in result [:masses :knee_l :d] )))
    result))
