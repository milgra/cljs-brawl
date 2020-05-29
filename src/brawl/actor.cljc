(ns brawl.actor
  (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]
            [brawl.metrics :as metrics]
            [brawl.actorai :as ai]
            [brawl.actorjump :as jump]
            [brawl.actorwalk :as walk]
            [clojure.spec.alpha :as spec]))

(def MPI2 (* Math/PI 2))

(declare update-idle)
(declare update-rag)


(defn update-dragged [{update-fn :update-fn :as dragged} {{head :head hip :hip neck :neck} :masses :as dragger}]
  (cond-> dragged
    (not= update-fn update-rag) (assoc :update-fn update-rag)
    true (assoc :hittime 0)
    true (assoc-in [:masses :hip :d] [0 0])
    true (assoc-in [:masses :hip :p] (math2/add-v2 (:p head) [-20 0] ))
    true (assoc-in [:masses :neck :d] [0 0])
    true (assoc-in [:masses :neck :p] (math2/add-v2 [-20 0] (math2/add-v2 (:p head)(math2/rotate-90-ccw (math2/sub-v2 (:p neck) (:p hip))))))))


(defn update-gun [gun {{hand_l :hand_l elbow_l :elbow_l} :masses facing :facing commands :commands {:keys [punch] } :control :as actor}]
  (assoc gun
         :p (:p hand_l)
         :f (- facing)
         :d (if punch [(* facing 100.0) 0.0] (math2/sub-v2 (:p hand_l) (:p elbow_l)))
         :s punch))


(defn int-to-rgba [color]
  (let [r (/ (float (bit-and (bit-shift-right color 24) 0xFF)) 255.0)
        g (/ (float (bit-and (bit-shift-right color 16) 0xFF)) 255.0)
        b (/ (float (bit-and (bit-shift-right color 8) 0xFF)) 255.0)
        a (/ (float (bit-and color 0xFF)) 255.0)]
    [r g b a]))
  

(defn init [x y id color basemetrics]
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
        metrics (metrics/generate-metrics basemetrics)]
    
    {:id id
     :color color
     :metrics metrics
     ;; base states
     :next nil
     :speed 0.0
     :power 100.0
     :health 100.0
     :facing 1.0
     :update-fn jump/update-jump
     :commands []
     :idle-angle 0
     :dragged-gun nil
     :dragged-body nil
     :injure-when-dropped false
     ;; ai state
     :ai-state :idle
     :ai-enemy nil
     :ai-duration 0
     ;; control state
     :control {:left false :right false :up false :down false :punch false :kick false :block false :run false }
     :pickup-sent false
     :action-sent false
     ;; walk state
     :bases bases
     :squat-size 0
     :base-order {:active :base_l :passive :base_r}
     :base-target nil
     :base-surfaces {:active nil :passive nil}
     :punch-hand :hand_l
     :vert-direction 1
     :jump-state 0
     :step-length 0
     ;; masses
     :masses masses
     :dguards [(phys2/dguard2 masses :head :neck (:headl metrics) 0.0)
               (phys2/dguard2 masses :neck :hip (:bodyl metrics) 0.0)
               (phys2/dguard2 masses :hip :knee_l (* 0.5 (:legl metrics)) 0.0)
               (phys2/dguard2 masses :hip :knee_r (* 0.5 (:legl metrics)) 0.0)
               (phys2/dguard2 masses :knee_l :foot_l (* 0.5 (:legl metrics)) 0.0)
               (phys2/dguard2 masses :knee_r :foot_r (* 0.5 (:legl metrics)) 0.0)
               (phys2/dguard2 masses :neck :elbow_l (* 0.5 (:arml metrics)) 0.0)
               (phys2/dguard2 masses :neck :elbow_r (* 0.5 (:arml metrics)) 0.0)
               (phys2/dguard2 masses :elbow_l :hand_l (* 0.5 (:arml metrics)) 0.0)
               (phys2/dguard2 masses :elbow_r :hand_r (* 0.5 (:arml metrics)) 0.0)]
     :aguards [(phys2/aguard2 masses :head :neck :hip 0 Math/PI 0.5)
               (phys2/aguard2 masses :hip :knee_l :foot_l (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
               (phys2/aguard2 masses :hip :knee_r :foot_r (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
               (phys2/aguard2 masses :neck :elbow_r :hand_r (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
               (phys2/aguard2 masses :neck :elbow_l :hand_l (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)]
     ;; skin drawing related
     :colorf (int-to-rgba color)
     :randoms (vec (repeatedly 40 #(+ -1.5 (rand 3))))
     :step-zone [x y]})); random sizes for skin phasing


(defn hitpoint
  "get hitpoint"
  [{{:keys [head neck hip hand_l hand_r elbow_l elbow_r knee_l knee_r foot_l foot_r]} :masses health :health :as actor} {:keys [id base target time]}]
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
            

(defn hit
  "hit actor"
  [{{:keys [head neck hip hand_l hand_r elbow_l elbow_r knee_l knee_r foot_l foot_r] :as masses} :masses health :health metrics :metrics update-fn :update-fn :as actor} {:keys [id base target time power] :as command}]
  (let [ [dx dy] (math2/sub-v2 base (:p hip))]
    (if-not (and (not= id (:id actor)) (< dx 80.0) (< dy 80.0) (not= update-fn update-rag))
      actor
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
                       true (assoc :hitduration (if (and headisp (> power 39)) 100 20))
                       true (assoc :next "rag")
                       true (update :health - power) 
                       true (update :speed  + (* (/ hvx (Math/abs hvx)) 5.0))
                       ;; true (assoc :masses (reduce (fn [oldmasses [id mass]] (assoc oldmasses id (assoc mass :d [0 0]))) {} masses))
                       headisp (assoc-in [:masses :head :d] (math2/add-v2 (:d head) hitbg))
                       headisp (assoc-in [:masses :neck :d] (math2/add-v2 (:d neck) hitsm))
                       bodyisp (assoc-in [:masses :neck :d] (math2/add-v2 (:d neck) (math2/scale-v2 hitbg (+ 0.4 neck-ratio))))
                       bodyisp (assoc-in [:masses :hip :d] (math2/add-v2 (:d hip) (math2/scale-v2 hitbg (+ 0.4 hip-ratio))))
                       footlisp (assoc-in [:masses :hip :d] (math2/add-v2 (:d hip) hitbg))
                       footlisp (assoc-in [:masses :knee_l :d] (math2/add-v2 (:d knee_l) hitsm))
                       footrisp (assoc-in [:masses :hip :d] (math2/add-v2 (:d hip) hitbg))
                       footrisp (assoc-in [:masses :knee_r :d] (math2/add-v2 (:d knee_r) hitsm))))]
        result))))


(defn update-idle [state] state)


(defn update-rag [{:keys [masses dguards hittime hitduration next health injure-when-dropped commands id] :as state } surfaces time]
  (if (> health 0)
    (let [newmasses (-> masses
                        (phys2/add-gravity [0.0 0.4])
                        ;;(phys2/keep-angles (:aguards state))
                        (phys2/keep-distances (:dguards state))
                        (phys2/move-masses surfaces 0.4))
          newnext (if (= hittime hitduration) "jump" next) 
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
          newcommands (if-not (and injure-when-dropped (= 0 (mod hittime 5)) (< hittime 100) (> hittime 0))
                        commands
                        (into commands [{:id id
                                         :text "attack"
                                         :base (get-in masses [:hip :p])
                                         :target (get-in masses (math2/resize-v2 [:hip :d] 10.0))
                                         :time time
                                         :power 50}]))
          result (-> state
                     (assoc :commands newcommands)
                     (assoc :next newnext)
                     (assoc :masses newmasses)
                     (update :hittime inc))]
      (if (= result nil) println "UPDATERAG ERROR!!!")
      result)))


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
                (= update-fn jump/update-jump) (assoc :squat-size (* 0.5 (get-in state [:metrics :legl]))) ; squat when reaching ground
                true (assoc :jump-state 0) ; reset jump state
                true (assoc :next nil)
                true (assoc :base-target nil) ; reset stepping
                true (assoc :update-fn walk/update-walk)
                true (assoc :masses newmasses))))

          (= next "jump")
          (let [[hx hy] (:p hip)
                [lx ly] (:p ba)
                [rx ry] (:p bb)]
            ;; reset jump state
            (cond-> state
              (= update-fn walk/update-walk) (assoc-in [:bases :base_l :p] [(- hx 10.0) (- (min ly ry) 10.0)])
              (= update-fn walk/update-walk) (assoc-in [:bases :base_r :p] [(+ hx 10.0) (- (min ly ry) 10.0)])
              (= update-fn walk/update-walk) (assoc-in [:bases :base_l :d] [(/ speed 2) -10])
              (= update-fn walk/update-walk) (assoc-in [:bases :base_r :d] [(/ speed 2) -10])         
              true (assoc :next nil)
              true (assoc :update-fn jump/update-jump)))

          (= next "rag")
          ;; reset jump state
          (-> state
              (assoc :next nil)
              (assoc :update-fn update-rag))

          (= next "idle")
          ;; reset jump state
          (-> state
              (assoc :injure-when-dropped false)
              (assoc :next nil)
              (assoc :update-fn update-idle))

          :else
          (do
            (println "bad mode" next)
            state)
          )]
    (if (= result nil) println "UPDATEMODE ERROR!!!")
    result))


(defn update-controls [{:keys [punch-hand action-sent pickup-sent vert-direction] :as state} {:keys [left right up down punch kick block run] :as control}]
  (if-not control
    state
    (let [p-pressed (cond
                      (and (not (:punch control)) punch) true
                      (and (:punch control) (not punch)) false
                      :else (:punch control))
          k-pressed (cond
                      (and (not (:kick control)) kick) true
                      (and (:kick control) (not kick)) false
                      :else (:kick control))
          p-hand (if (and (not (:punch control)) punch)
                   (if (= punch-hand :hand_l) :hand_r :hand_l)
                   punch-hand)]
      (-> state
          (assoc :action-sent (if (and (not p-pressed) (not k-pressed)) false action-sent))
          (assoc :pickup-sent (if (not down) false pickup-sent))
          (assoc :punch-hand p-hand)
          (assoc :vert-direction (cond
                                   down -1
                                   up 1
                                   :else vert-direction))
          (assoc-in [:control :punch] p-pressed)
          (assoc-in [:control :kick] k-pressed)
          (assoc-in [:control :left] left)
          (assoc-in [:control :right] right)
          (assoc-in [:control :up] up)
          (assoc-in [:control :down] down)
          (assoc-in [:control :block] block)
          (assoc-in [:control :run] run)))))


(defn update-angle
  [{angle :idle-angle health :health :as state}]
  (cond-> state
    (and (< health 100.0) (> health 0.0)) (update :health + 0.05) 
    (> angle MPI2) (assoc :idle-angle (- angle MPI2))
    (< angle MPI2) (update :idle-angle + 0.05)))


(defn update-actor [{mode :mode update-fn :update-fn :as state} control surfaces actors guns time]
  "update actor state"
  ;;(if (= (:id state) :enemy) (println "BEFORE UPDATE" (:version state) (get-in state [:masses :knee_l :d] )))
  (let [result (-> state
                   (update-angle)
                   (update-controls control) ;; manual controls for hero
                   ;;(ai/update-ai control surfaces actors time) ;; ai controls for others
                   (update-fn surfaces time)
                   (update-mode))]
    ;;(if (= (:id state) :enemy) (println "AFTER UPDATE" (:version result) (get-in result [:masses :knee_l :d] )))
    result))
