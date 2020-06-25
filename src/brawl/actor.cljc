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
  {:head    (phys2/mass2 x y 4.0 1.0 0.5 0.2)
   :neck    (phys2/mass2 x y 4.0 1.0 0.5 0.2)
   :hip     (phys2/mass2 x y 4.0 1.0 0.5 0.2)
   :hand_l  (phys2/mass2 x y 4.0 1.0 0.5 0.2)
   :hand_r  (phys2/mass2 x y 4.0 1.0 0.5 0.2)
   :elbow_l (phys2/mass2 x y 4.0 1.0 0.5 0.2)
   :elbow_r (phys2/mass2 x y 4.0 1.0 0.5 0.2)
   :knee_l  (phys2/mass2 x y 4.0 1.0 0.5 0.2)
   :knee_r  (phys2/mass2 x y 4.0 1.0 0.5 0.2)
   :foot_l  (phys2/mass2 (+ x 20.0) y 4.0 1.0 0.5 0.2)
   :foot_r  (phys2/mass2 (+ x 20.0) y 4.0 1.0 0.5 0.2)})


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


(def default-control
  {:left false
   :right false
   :up false
   :down false
   :punch false
   :kick false
   :block false
   :run false })


(def default-walk
  {:zone [] ;; store step zone for debug mode in skin
   :order {:active :base_l :passive :base_r} ;; base stepping order
   :target nil ;; target of active base
   :surfaces {:active nil :passive nil} ;; stepped surfaces for foot angles in skin
   :direction 1 ;; vertical direction
   :step-size 0 ;; current step size
   :squat-size 0 ;; current squat size
   :jump-state 0 ;; jump state, 0 (squat phase) 1 (jumping phase)
   :jump-lethal false ;; fall speed is too high
   :jump-speed 0
   :idle-angle 0})


(def default-drag
  {:gun nil ;; dragged gun
   :body nil ;; dragged body
   :injure? false ;; should our dragged body injure others (when dropped?)
   :dragged? false}) ;; are we dragged?


(def default-ai
  {:state :idle
   :target nil ;; enemy to chase/hit
   :timeout 0}) ;; decision timeout


(def default-attack
  {:bullets 0 ;; bullets in our gun
   :punch-hand :hand_l ;; current punch hand
   :punch-y 0
   :kick-y 0
   :action-sent false ;; hit/kick sent
   :pickup-sent false ;; pickup sent
   :timeout 0}) ;; decision timeout


(defn default-skin
  [color]
  {:color (math/int-to-rgba color)
   :randoms (vec (repeatedly 40 #(+ -1.5 (rand 3))))})


(defn init
  "create new actor dataset"
  [x y id color basemetrics level]
  (let [metrics (metrics/generate-metrics basemetrics)
        bases (default-bases x y)
        masses (default-masses x y)
        dguards (default-distance-guards masses metrics)
        aguards (default-angle-guards masses)]
    {;; fix properties
     :id id
     :color color
     :level level
     :metrics metrics
     ;; physics
     :bases bases
     :masses masses
     :dguards dguards
     :aguards aguards
     ;; internal state
     :mode :jump
     :next-mode nil
     :speed -2.0
     :facing 1.0
     :health (+ (if (= id :hero) 150.0 100.0) (* level 10.0))
     :control default-control
     :commands []
     ;; substates
     :ai default-ai
     :walk default-walk
     :drag default-drag
     :skin (default-skin color)
     :attack default-attack}))


(defn check-death
  "check health and change state to dead if needed"
  [actor time]
  (let [{:keys [id health]} actor]
    (if-not (<= health 0)
      actor
      (let [actor-new (-> actor
                          (assoc :next-mode :rag)
                          (assoc-in [:attack :timeout] (+ time 2000))
                          (update :commands conj {:text "drop" :id id}))]
        (if (= id :hero ) (update actor-new :commands conj {:text "show-wasted"}) actor-new)))))


(defn play-hit-or-death
  [actor]
  (let [{:keys [health]} actor]
    (if (<= health 0)
      (update actor :commands conj {:text "play-death"})
      (update actor :commands conj {:text "play-hit"}))))


(defn hitpoints
  "returns hitpoints of actor bones and attack vector"
  [actor command]  
  (let [{:keys [id color health mode]} actor
        {:keys [head neck hip knee_l knee_r]} (:masses actor)
        {:keys [base target radius]} command
        [dx dy] (math2/sub-v2 base (:p hip))
        alive? (> health 0)
        nearby? (and (< (Math/abs dx) radius) (< (Math/abs dy) radius))
        not-rag? (not= mode :rag)
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
  [actor hitpoints base target]
  (let [{:keys [metrics]} actor
        {:keys [head neck hip knee_l knee_r foot_l foot_r]} (:masses actor)
        [headisp bodyisp footlisp footrisp] hitpoints
        [hvx hvy :as hitv] (math2/sub-v2 target base)

        hitsm (math2/resize-v2 hitv 5)
        hitbg (math2/resize-v2 hitv 10)

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
  [actor command time]
  (let [{:keys [id health facing mode]} actor
        {:keys [block]} (:control actor)
        {:keys [base target power]} command
        [headisp bodyisp footlisp footrisp :as hitpoints] (hitpoints actor command)
        diff-facing? (not= facing (:facing command))
        hitpower (cond
                   headisp power ;; head kick/punch is full power
                   bodyisp (* power 0.6)
                   footlisp (* power 0.4)
                   footrisp (* power 0.4))
        timeout (cond
                  (= mode :jump) (+ time 2000) ;; if hit during jump, stay on the ground a bit
                  (and headisp (> hitpower 39)) (+ time 1000) ;; if hit on the head hard, stay on the gorund a little
                  :else (+ time 200))] ;; just stagger a little

    (if-not (or headisp bodyisp footlisp footrisp)
      actor ;; no intersection, leave actor untouched
      (if (and block diff-facing?)
        ;; move actor slightly back when blocking and facing the sender
        (-> actor
            (assoc-in [:attack :timeout] (+ time 1000))
            (update :health - (/ power 5.0)) 
            (update :speed + (* -2.5 facing))
            (check-death time))
        ;; hit actor
        (-> actor
            (assoc-in [:attack :timeout] timeout )
            (assoc :next-mode :rag)
            (update :health - hitpower) 
            (update :speed + (* -2.5 facing))
            (play-hit-or-death)
            (hit-masses hitpoints base target)
            (check-death time))))))


(defn update-dragged
  "update body mass points of actor based on dragger"
  [dragged dragger time]
  (let [{:keys [head hip neck]} (:masses dragger)
        hip-new (math2/add-v2 (:p head) [-20 0])
        neck-new (math2/add-v2 [-20 0] (math2/add-v2 (:p head) (math2/rotate-90-ccw (math2/sub-v2 (:p neck) (:p hip)))))]
    (-> dragged
        (assoc-in [:attack :timeout] (+ time 1000))
        (assoc-in [:masses :hip :d] [0 0])
        (assoc-in [:masses :hip :p] hip-new)
        (assoc-in [:masses :neck :d] [0 0])
        (assoc-in [:masses :neck :p] neck-new))))
  

(defn update-gun
  "update points of gun based on dragged"
  [gun actor]
  (let [{:keys [s]} gun
        {:keys [facing commands]} actor
        {:keys [hand_l elbow_l]} (:masses actor)
        {:keys [bullets]} (:attack actor)
        {:keys [shoot]} (:control actor)]
    (assoc gun
           :p (:p hand_l)
           :f (- facing)
           :d (if shoot [(* facing 100.0) 0.0] (math2/sub-v2 (:p hand_l) (:p elbow_l)))
           :s (if (and shoot (> bullets 0)) (inc s) 0))))


(defn change-mode-idle
  "change mode to idle"
  [actor surfaces]
  (-> actor
      (assoc-in [:drag :dragged?] false)
      (assoc-in [:drag :injure?] false)
      (assoc :next-mode nil)
      (assoc :mode :idle)))


(defn change-mode-rag
  "change mode to rag"
  [actor surfaces]
  (-> actor
      (assoc :next-mode nil)
      (assoc :mode :rag)))


(defn change-mode-jump
  "change mode to jump"
  [actor surfaces]
  (let [{:keys [mode speed]} actor
        {hip :hip :as masses} (:masses actor)
        {bl :base_l br :base_r} (:bases actor)
        [hx hy] (:p hip)
        [lx ly] (:p bl)
        [rx ry] (:p br)]
    (cond-> actor
      (= mode :walk) (assoc-in [:bases :base_l :p] [(- hx 10.0) (- (min ly ry) 10.0)])
      (= mode :walk) (assoc-in [:bases :base_r :p] [(+ hx 10.0) (- (min ly ry) 10.0)])
      (= mode :walk) (assoc-in [:bases :base_l :d] [(/ speed 2) -10])
      (= mode :walk) (assoc-in [:bases :base_r :d] [(/ speed 2) -10])         
      true (assoc :next-mode nil)
      true (assoc :mode :jump))))


(defn change-mode-walk
  "change mode to walk"
  [actor surfaces]
  (let [{:keys [mode]} actor
        {hip :hip :as masses} (:masses actor)
        {bl :base_l} (:bases actor)
        masses-new (reduce (fn [res [id mass]] (assoc res id (assoc mass :d [0 0]))) masses masses) ; reset mass directions for next rag
        feet-new (sort-by first (phys2/get-intersecting-surfaces (:p hip) [0 400] surfaces))
        feet-final (if-not (empty? feet-new)
                     (second (first feet-new))
                     (:p bl))
        actor-new (-> actor
                      (assoc-in [:walk :jump-state] 0) ; reset jump state
                      (assoc-in [:walk :target] nil) ; reset stepping
                      (assoc :next-mode nil)
                      (assoc :mode :walk)
                      (assoc :masses masses-new))]
    (cond-> actor-new
      (= mode :rag) (assoc-in [:bases :base_l :p] feet-final)
      (= mode :rag) (assoc-in [:bases :base_r :p] feet-final)
      (= mode :rag) (assoc-in [:walk :squat-size] (* 1.0 (get-in actor [:metrics :legl]))) ; squat when reaching ground
      (= mode :jump) (assoc-in [:walk :squat-size] (* 0.5 (get-in actor [:metrics :legl])))))) ; squat when reaching ground


(defn change-mode
  "if next mode is set, switch to that mode"
  [actor surfaces]
  (case (:next-mode actor)
    :walk (change-mode-walk actor surfaces)
    :jump (change-mode-jump actor surfaces)
    :rag (change-mode-rag actor surfaces)    
    :idle (change-mode-idle actor surfaces)
    actor))


(defn update-rag
  "update ragdoll state"
  [actor surfaces time delta]
  (let [{:keys [masses dguards next-mode health commands id]} actor
        {:keys [injure? dragged?]} (:drag actor)
        {:keys [timeout]} (:attack actor)

        masses-new (-> masses
                       (phys2/add-gravity [0.0 0.4])
                       ;;(phys2/keep-angles (:aguards state))
                       (phys2/keep-distances (:dguards actor))
                       (phys2/move-masses (if dragged? [] surfaces) 0.4)) ;; if dragged be able to drag down the body at a fork
        mode-new (cond
                   (and (>  health 0) (> time timeout)) :walk
                   (and (<= health 0) (:q (:neck masses-new)) (:q (:hip masses-new))) :idle
                   :else next-mode)

        injure?-new (if (and (<= health 0) (> time timeout)) false injure?) 

        commands-new (if-not (and injure? (= 0 (mod (int (* time 10.0)) 2)))
                       commands
                       (into commands [{:id id
                                        :text "attack"
                                        :base (get-in masses [:neck :p])
                                        :target (get-in masses (math2/resize-v2 [:neck :d] 10.0))
                                        :facing 0
                                        :radius 50.0
                                        :power 50}]))]
    (-> actor
        (assoc :injure? injure?-new)
        (assoc :commands commands-new)
        (assoc :next-mode mode-new)
        (assoc :masses masses-new))))


(defn update-mode
  "update actor based on actual mode"
  [actor surfaces time delta]
  (case (:mode actor)
    :rag (update-rag actor surfaces time delta)
    :walk (walk/update-walk actor surfaces time delta)
    :jump (jump/update-jump actor surfaces time delta)
    actor))


(defn update-controls
  "update controls if controlled by player"
  [actor control]
  (let [self-control (:control actor)
        {direction :direction} (:walk actor)
        {:keys [punch-hand kick-y punch-y action-sent pickup-sent]} (:attack actor)
        {:keys [left right up down punch kick shoot block run]} control]
    
    (if-not control
      actor
      (let [p-hand (if (and (not (:punch self-control)) punch) ;; change only when punch state changes
                     (if (= punch-hand :hand_l) :hand_r :hand_l)
                     punch-hand) ;; else leave it as it was
            punch-y (if (and (not (:punch self-control)) punch) (rand 15) punch-y) ;; punch height
            kick-y (if (and (not (:kick self-control)) kick) (rand 30) kick-y)] ;; kick height
        (-> actor
            
            (assoc-in [:attack :action-sent] (if (and (not punch) (not kick) (not shoot)) false action-sent)) ;; set action sent to false if no action is happening
            (assoc-in [:attack :pickup-sent] (if (not down) false pickup-sent)) ;; set it to false if no pickup is happening
            (assoc-in [:attack :punch-hand] p-hand)
            (assoc-in [:walk :direction] (cond ;; change vertical direction in case of up/down
                                                down -1
                                                up 1
                                                :else direction))
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
            (assoc-in [:control :run] run))))))


(defn update-angle
  "update breathing angle of actor"
  [actor delta]
  (let [{angle :idle-angle} (:walk actor)
        health (:health actor)]
    (cond-> actor
      (and (< health 100.0) (> health 0.0)) (update :health + (* 0.05 delta)) 
      (> angle math/MPI2) (assoc-in [:walk :idle-angle] (- angle math/MPI2))
      (< angle math/MPI2) (update-in [:walk :idle-angle] + (* 0.05 delta)))))


(defn update-actor
  "update actor state"
  [actor control surfaces actors guns time delta]
  (-> actor
      (ai/update-ai control surfaces actors time delta) ;; ai controls for others
      (update-controls control) ;; manual controls for hero
      (update-angle delta)
      (update-mode surfaces time delta)
      (change-mode surfaces)))
