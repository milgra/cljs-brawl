(ns brawl.actor
  (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]
            [brawl.math :as math]
            [brawl.metrics :as metrics]
            [brawl.actorai :as ai]
            [brawl.actorjump :as jump]
            [brawl.actorwalk :as walk]))


(defn default-bases
  [x y]
  {:base_l (phys2/mass2 (+ x 20.0) y 2.0 1.0 0.0 0.0)
   :base_r (phys2/mass2 (- x 20.0) y 2.0 1.0 0.0 0.0)})


(defn default-masses
  [x y]
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


(defn default-distance-guards
  [masses metrics]
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


(defn default-angle-guards
  [masses]
  [(phys2/aguard2 masses :head :neck :hip 0 Math/PI 0.5)
   (phys2/aguard2 masses :hip :knee_l :foot_l (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
   (phys2/aguard2 masses :hip :knee_r :foot_r (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
   (phys2/aguard2 masses :neck :elbow_r :hand_r (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
   (phys2/aguard2 masses :neck :elbow_l :hand_l (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)])


(defn default-control
  []
  {:left false
   :right false
   :up false
   :down false
   :punch false
   :kick false
   :block false
   :run false })


(defn init
  "create new actor dataset"
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
     :speed -2.0
     :facing 1.0
     :health (+ 100.0 (* level 50.0))
     :control (default-control)
     :commands [] ;; command collector
     ;; step related
     :step {:order {:active :base_l :passive :base_r}
            :target nil
            :surfaces {:active nil :passive nil}
            :length 0
            :zone []
            :vert-direction 1
            :squat-size 0
            :jump-state 0
            :idle-angle 0}
     ;; skin related
     :skin {:color (math/int-to-rgba color)
            :randoms (vec (repeatedly 40 #(+ -1.5 (rand 3))))}
     ;; ai related
     :ai {:state :idle
          :target nil
          :timeout 0}
     ;; drag related
     :drag {:dragged-gun nil
            :dragged-body nil
            :is-dragged false
            :injure-when-dropped false}
     ;; attack related
     :attack {:bullets 0
              :punch-hand :hand_l
              :punch-y 0
              :kick-y 0
              :action-sent false}
     
     :pickup-sent false
    }))


(defn check-death
  "check health and change state to dead if needed"
  [{:keys [id health] :as actor} time]
  (if (<= health 0)
    (cond-> actor
      true (assoc :next-mode :rag)
      true (assoc :dragged-body nil)
      true (assoc :hittimeout (+ time 2000))
      (= id :hero ) (update :commands conj {:text "show-wasted"}))
    actor))


(defn hitpoints
  "returns hitpoints of actor bones and attack vector"
  [{:keys [id color health curr-mode]
    {:keys [head neck hip knee_l knee_r] :as masses} :masses}
   {:keys [base target radius] :as command}]
  
  (let [[dx dy] (math2/sub-v2 base (:p hip))
        alive? (> health 0)
        nearby? (and (< (Math/abs dx) radius) (< (Math/abs dy) radius))
        not-rag? (not= curr-mode :rag)
        diff-color? (not= color (:color command))]

    (if-not (and diff-color? nearby? not-rag? alive?)
      [] ;; no hitpoints, return empty vector
      (let [[hvx hvy :as hitv] (math2/sub-v2 target base)
        
            headv (math2/sub-v2 (:p neck) (:p head))
            bodyv (math2/sub-v2 (:p hip) (:p neck))
            footlv (math2/sub-v2 (:p knee_l) (:p hip))
            footrv (math2/sub-v2 (:p knee_r) (:p hip))
            
            headisp (math2/intersect-v2-v2 base hitv (:p head) headv)
            bodyisp (math2/intersect-v2-v2 base hitv (:p neck) bodyv)
            footlisp (math2/intersect-v2-v2 base hitv (:p hip) footlv)
            footrisp (math2/intersect-v2-v2 base hitv (:p hip) footrv)]

        [headisp bodyisp footlisp footrisp]))))


(defn hit-masses
  "push masses towards hit direction"
  [{:keys [metrics] {:keys [head neck hip knee_l knee_r foot_l foot_r] :as masses} :masses :as actor}
   [headisp bodyisp footlisp footrisp]
   base
   target]
  (let [[hvx hvy :as hitv] (math2/sub-v2 target base)
        hitsm (math2/resize-v2 hitv 10)
        hitbg (math2/resize-v2 hitv 20)        
        neck-part (if bodyisp (math2/length-v2 (math2/sub-v2 bodyisp (:p neck))))
        hip-part  (if bodyisp (math2/length-v2 (math2/sub-v2 bodyisp (:p hip))))
        neck-ratio (if bodyisp (/ hip-part (:bodyl metrics)))
        hip-ratio (if bodyisp (/ neck-part (:bodyl metrics)))]

    (cond-> actor
      headisp (assoc-in [:masses :head :d] (math2/add-v2 (:d head) hitbg))
      headisp (assoc-in [:masses :neck :d] (math2/add-v2 (:d neck) hitsm))
      
      bodyisp (assoc-in [:masses :neck :d] (math2/add-v2 (:d neck) (math2/scale-v2 hitbg (+ 0.4 neck-ratio))))
      bodyisp (assoc-in [:masses :hip :d] (math2/add-v2 (:d hip) (math2/scale-v2 hitbg (+ 0.4 hip-ratio))))

      footlisp (assoc-in [:masses :hip :d] (math2/add-v2 (:d hip) hitbg))
      footlisp (assoc-in [:masses :knee_l :d] (math2/add-v2 (:d knee_l) hitsm))
      footrisp (assoc-in [:masses :hip :d] (math2/add-v2 (:d hip) hitbg))
      footrisp (assoc-in [:masses :knee_r :d] (math2/add-v2 (:d knee_r) hitsm)))))


(defn hit
  "hit actor"
  [{:keys [id health facing curr-mode]
    {:keys [block]} :control :as actor}
   {:keys [base target power facing] :as command}
   time]

  (let [[headisp bodyisp footlisp footrisp :as hitpoints] (hitpoints actor command)
        diff-facing? (not= facing (:facing command))
        hitpower (cond
                   headisp power ;; head kick/punch is full power
                   bodyisp (* power 0.6)
                   footlisp (* power 0.4)
                   footrisp (* power 0.4))
        timeout (cond
                  (= curr-mode :jump) (+ time 2000) ;; if hit during jump, stay on the ground a bit
                  (and headisp (> hitpower 39)) (+ time 1000) ;; if hit on the head hard, stay on the gorund a little
                  :else (+ time 200))] ;; just stagger a little

    (if-not (or headisp bodyisp footlisp footrisp)
      actor ;; no intersection, leave actor untouched
      (if (and block diff-facing?)
        ;; move actor slightly back when blocking and facing the sender
        (-> actor
            (assoc :hittimeout (+ time 1000))
            (update :health - (/ power 2.0)) 
            (update :speed + (* facing (/ power 2.0)))
            (check-death time))
        ;; hit actor
        (-> actor
            (assoc :hittimeout timeout )
            (assoc :next-mode :rag)
            (update :health - hitpower) 
            (update :speed + (* facing (/ power 2.0)))
            (hit-masses hitpoints base target)
            (check-death time))))))


(defn update-dragged
  "update body mass points of actor based on dragger"
  [dragged
   {{:keys [head hip neck]} :masses :as dragger}
   time]
  (-> dragged
      (assoc :hittimeout (+ time 5000))
      (assoc-in [:masses :hip :d] [0 0])
      (assoc-in [:masses :hip :p] (math2/add-v2 (:p head) [-20 0] ))
      (assoc-in [:masses :neck :d] [0 0])
      (assoc-in [:masses :neck :p] (math2/add-v2 [-20 0] (math2/add-v2 (:p head)(math2/rotate-90-ccw (math2/sub-v2 (:p neck) (:p hip))))))))


(defn update-gun
  "update points of gun based on dragged"
  [{s :s :as gun}
   {{hand_l :hand_l elbow_l :elbow_l} :masses
    :keys [facing commands action-sent]
    {bullets :bullets} :attack
    {:keys [shoot]} :control :as actor}]
  (assoc gun
         :p (:p hand_l)
         :f (- facing)
         :d (if shoot [(* facing 100.0) 0.0] (math2/sub-v2 (:p hand_l) (:p elbow_l)))
         :s (if (and shoot (> bullets 0)) (inc s) 0)))


(defn change-mode-idle
  "change mode to idle"
  [state surfaces]
  (-> state
      (assoc-in [:drag :is-dragged] false)
      (assoc-in [:drag :injure-when-dropped] false)
      (assoc :next-mode nil)
      (assoc :curr-mode :idle)))


(defn change-mode-rag
  "change mode to rag"
  [state surfaces]
  (-> state
      (assoc :next-mode nil)
      (assoc :curr-mode :rag)))


(defn change-mode-jump
  "change mode to jump"
  [{:keys [curr-mode speed]
    {hip :hip :as masses} :masses
    {bl :base_l br :base_r} :bases :as state}
   surfaces]
    (let [[hx hy] (:p hip)
          [lx ly] (:p bl)
          [rx ry] (:p br)]
      (cond-> state
        (= curr-mode :walk) (assoc-in [:bases :base_l :p] [(- hx 10.0) (- (min ly ry) 10.0)])
        (= curr-mode :walk) (assoc-in [:bases :base_r :p] [(+ hx 10.0) (- (min ly ry) 10.0)])
        (= curr-mode :walk) (assoc-in [:bases :base_l :d] [(/ speed 2) -10])
        (= curr-mode :walk) (assoc-in [:bases :base_r :d] [(/ speed 2) -10])         
        true (assoc :next-mode nil)
        true (assoc :curr-mode :jump))))


(defn change-mode-walk
  "change mode to walk"
  [{:keys [curr-mode]
    {hip :hip :as masses} :masses
    {bl :base_l} :bases :as state}
   surfaces]
  (let [masses-new (reduce (fn [res [id mass]] (assoc res id (assoc mass :d [0 0]))) masses masses) ; reset mass directions for next rag
        feet-new (sort-by first (phys2/get-intersecting-surfaces (:p hip) [0 400] surfaces))
        feet-final (if-not (empty? feet-new)
                     (second (first feet-new))
                     (:p bl))
        state-new (-> state
                      (assoc-in [:step :jump-state] 0) ; reset jump state
                      (assoc :next-mode nil)
                      (assoc-in [:base :target] nil) ; reset stepping
                      (assoc :curr-mode :walk)
                      (assoc :masses masses-new))]
    (cond-> state-new
      (= curr-mode :rag) (assoc-in [:bases :base_l :p] feet-final)
      (= curr-mode :rag) (assoc-in [:bases :base_r :p] feet-final)
      (= curr-mode :rag) (assoc-in [:step :squat-size] (* 1.0 (get-in state [:metrics :legl]))) ; squat when reaching ground
      (= curr-mode :jump) (assoc-in [:step :squat-size] (* 0.5 (get-in state [:metrics :legl])))))) ; squat when reaching ground


(defn change-mode
  "if next mode is set, switch to that mode"
  [{:keys [masses next-mode speed curr-mode] {hip :hip fl :foot_l fr :foot_r :as masses} :masses
    {ba :base_l bb :base_r} :bases :as state}
   surfaces]
  (cond
    (= next-mode nil) state
    (= next-mode :walk) (change-mode-walk state surfaces)
    (= next-mode :jump) (change-mode-jump state surfaces)
    (= next-mode :rag) (change-mode-rag state surfaces)    
    (= next-mode :idle) (change-mode-idle state surfaces)
    :else state))


(defn update-rag
  "update ragdoll state"
  [{:keys [masses dguards hittimeout next-mode health injure-when-dropped is-dragged commands id] :as state }
   surfaces
   time
   delta]
  (let [masses-new (-> masses
                       (phys2/add-gravity [0.0 0.4])
                       ;;(phys2/keep-angles (:aguards state))
                       (phys2/keep-distances (:dguards state))
                       (phys2/move-masses (if is-dragged [] surfaces) 0.4)) ;; if dragged be able to drag down the body at a fork
        mode-new (cond
                   (and (>  health 0) (> time hittimeout)) :walk
                   (and (<= health 0) (> time hittimeout) (:q (:neck masses-new)) (:q (:hip masses-new))) :idle
                   :else next-mode)

        commands-new (if-not (and injure-when-dropped (= 0 (mod (int (* time 10.0)) 2)))
                       commands
                       (into commands [{:id id
                                        :text "attack"
                                        :base (get-in masses [:neck :p])
                                        :target (get-in masses (math2/resize-v2 [:neck :d] 10.0))
                                        :facing 0
                                        :radius 50.0
                                        :power 50}]))]
    (-> state
        (assoc :commands commands-new)
        (assoc :next-mode mode-new)
        (assoc :masses masses-new))))


(defn update-mode
  "update actor based on actual mode"
  [{mode :curr-mode :as state} surfaces time delta]
  (case mode
    :rag (update-rag state surfaces time delta)
    :walk (walk/update-walk state surfaces time delta)
    :jump (jump/update-jump state surfaces time delta)
    state))


(defn update-controls
  "update controls if controlled by player"
  [{:keys [pickup-sent] self-control :control
    {vert-direction :vert-direction} :step
    {punch-hand :punch-hand kick-y :kick-y punch-y :punch-y action-sent :action-sent} :attack
    :as state}
   {:keys [left right up down punch kick shoot block run] :as control}]
  (if-not control
    state
    (let [p-hand (if (and (not (:punch self-control)) punch) ;; change only when punch state changes
                   (if (= punch-hand :hand_l) :hand_r :hand_l)
                 punch-hand) ;; else leave it as it was
          punch-y (if (and (not (:punch self-control)) punch) (rand 15) punch-y) ;; punch height
          kick-y (if (and (not (:kick self-control)) kick) (rand 30) kick-y)] ;; kick height
      (-> state
          
          (assoc-in [:attack :action-sent] (if (and (not punch) (not kick) (not shoot)) false action-sent)) ;; set action sent to false if no action is happening
          (assoc :pickup-sent (if (not down) false pickup-sent)) ;; set it to false if no pickup is happening
          (assoc-in [:attack :punch-hand] p-hand)
          (assoc-in [:step :vert-direction] (cond ;; change vertical direction in case of up/down
                                              down -1
                                              up 1
                                              :else vert-direction))
          (assoc-in [:attack :punch-y] punch-y)
          (assoc-in [:attack :kick-y] kick-y)
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
  "update breathing angle of actor"
  [{{angle :idle-angle} :step health :health :as state} delta]
  (cond-> state
    (and (< health 100.0) (> health 0.0)) (update :health + (* 0.05 delta)) 
    (> angle math/MPI2) (assoc-in [:step :idle-angle] (- angle math/MPI2))
    (< angle math/MPI2) (update-in [:step :idle-angle] + (* 0.05 delta))))


(defn update-actor
  "update actor state"
  [state control surfaces actors guns time delta]
  (-> state
      (ai/update-ai control surfaces actors time delta) ;; ai controls for others
      (update-controls control) ;; manual controls for hero
      (update-angle delta)
      (update-mode surfaces time delta)
      (change-mode surfaces)))
