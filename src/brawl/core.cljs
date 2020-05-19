(ns ^:figwheel-hooks brawl.core
  (:require [goog.dom :as gdom]
            [goog.events :as events]
            [tubax.core :refer [xml->clj]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! chan put! take! poll!]]
            [cljs.core.async :refer-macros [go]]
            [brawl.svg :as svg]
            [gui.webgl :as uiwebgl]
            [gui.ui :as ui]
            [gui.math4 :as math4]
            [brawl.shape :as shape]
            [brawl.webgl :as webgl]
            [brawl.actor :as actor]
            [brawl.actorskin :as actorskin]
            [mpd.phys2 :as phys2]
            [mpd.math2 :as math2]
            [brawl.particle :as particle]
            [brawl.layouts :as layouts]
            [brawl.floatbuffer :as floatbuf])
  (:import [goog.events EventType]))
  

(defn load-level! [channel curr-level]
  "load level svg's"
  (go
    (let [response (<! (http/get (str "level" curr-level ".svg")
                                 {:with-credentials? false}))
          xmlstr (xml->clj (:body response) {:strict false})
          shapes (svg/psvg xmlstr "")]
      (put! channel shapes))))


(defn load-image! [channel name]
  "load touchscreen button images"
  (let [img (js/Image.)]
    (set! (.-onload img)
          (fn [a]
            (put! channel img)))
    (set! (.-src img) name)))


(defn resize-context! [ ]
  "resize canvas on window resize"
  (let [canvas (. js/document getElementById "main")]
        (set! (. canvas -width) (. js/window -innerWidth))
        (set! (. canvas -height) (. js/window -innerHeight))))


(def keycodes (atom {}))
(def mousedown (atom false))

(defn init-events! [keych tchch]
  "start event listening"
 
  (events/listen
   js/document
   EventType.KEYDOWN
   (fn [event]
     (let [code (.-keyCode event)
           prev (get @keycodes code)]
       (swap! keycodes assoc code true)
       (if (not prev) (put! keych {:code (.-keyCode event) :value true})))))

  (events/listen
   js/document
   EventType.KEYUP
   (fn [event]
     (let [code (.-keyCode event)
           prev (get @keycodes code)]
       (swap! keycodes assoc code false)
       (if prev (put! keych {:code (.-keyCode event) :value false})))))

  (events/listen
   js/document
   EventType.TOUCHSTART
   (fn [event]
     (println "touchstart" event)))

  (events/listen
   js/document
   EventType.MOUSEDOWN
   (fn [event]
     (swap! mousedown not)
     (put! tchch {:type "down" :point [(.-clientX event) (.-clientY event)]})))

  (events/listen
   js/document
   EventType.MOUSEUP
   (fn [event]
     (swap! mousedown not)
     (put! tchch {:type "up" :point [(.-clientX event) (.-clientY event)]})))

  (events/listen
   js/document
   EventType.MOUSEMOVE
   (fn [event]
     (if @mousedown (put! tchch {:type "down" :point [(.-clientX event) (.-clientY event)]}))))

  (events/listen
   js/window
   EventType.RESIZE
   (fn [event] (resize-context!))))
    

(defn create-actors
  "add actors, infos, guns and enpoint to scene based on pivot points in svg"
  [state pivots]
  (reduce (fn [{:keys [actors guns infos] :as oldstate} {id :id path :path}]         
            (let [pos (nth path 3)
                  toks (clojure.string/split id #"_")
                  type (first (second toks))]
              (cond (= type "l")
                    (do
                      (println "create actor" id type pos)
                      (-> oldstate
                      (assoc :actors (conj
                                      actors
                                      (actor/init
                                       (first pos)
                                       (second pos)
                                       (if (= id "Pivot_l0_t0") :hero (keyword (str (rand)))))

                                      ;; (actor/init
                                      ;;  (first pos)
                                      ;;  (second pos)
                                      ;;  (if (= id "Pivot_l0_t0") :enemy (keyword (str (rand)))))
                                      ))))
                    (= type "g") (assoc oldstate :guns (conj guns {:pos pos}))
                    (= type "e") (assoc oldstate :endpos pos)
                    (= type "i") (assoc oldstate :infos (conj infos {:pos pos :index (js/parseInt (second type))})))     
              )) state pivots))


(defn update-gen-sliders [{:keys [views world] :as state}]
  (let [{:keys [height hitpower hitrate stamina speed]} (get-in world [:actors 0 :metrics :base])
        hpsl (:Hitpower views)
        hrsl (:Hitrate views)
        hesl (:Height views)
        spsl (:Speed views)
        stsl (:Stamina views)
        newv (-> views
                 (ui/set-slider-value hpsl hitpower)
                 (ui/set-slider-value hrsl hitrate)
                 (ui/set-slider-value hesl height)
                 (ui/set-slider-value spsl speed)
                 (ui/set-slider-value stsl stamina))]
    (assoc state :views newv)))


(defn execute-attack [{ {actors :actors particles :particles} :world :as state} command]
  ;; hittest other actors
  (let [[dx dy] (math2/resize-v2 (math2/sub-v2 (:target command) (:base command)) 4.0)
        contacts (remove nil? (map (fn [actor] (actor/hitpoint actor command)) actors))
        newparticles (reduce (fn [ res [x y] ]
                               (concat res
                                       (repeatedly 10 #(particle/init x y [1.0 1.0 1.0 1.0] (math2/resize-v2 [(+ (- 1.0) (rand 2.0)) (+ (- 1.0) (rand 2.0))] (+ 1.0 (rand 2.0)))  :dust))
                                       (repeatedly 5 #(particle/init x y [1.0 0.0 0.0 1.0]  [ (+ dx -2.0 (rand 2.0)) (+ dy -2.0 (rand 2.0)) ]  :blood))))
                             [] contacts)
        newactors (vec (map (fn [actor] (actor/hit actor command)) actors))]
    (-> state
        (assoc-in [:world :particles] (concat particles newparticles))
        (assoc-in [:world :actors] newactors ))))


(defn load-next-level [{:keys [curr-level] :as state}]
  (let [next-level (min (inc curr-level) 6)
        views (ui/gen-from-desc {} layouts/hud)
        baseviews (ui/get-base-ids layouts/hud)
        viewids (ui/collect-visible-ids views baseviews "")]
    (println "next-level" next-level)
    (load-level! (:svgch state) next-level)
    (-> state
        (assoc :views views :baseviews baseviews :viewids viewids)
        (assoc :world {:inited false
                       :actors [] ; (actor/init 580.0 300.0)]
                       :guns []
                       :infos []
                       :endpos [0 0]
                       :surfaces []
                       :surfacelines []})
        (assoc :curr-level next-level))))


(defn load-ui [state description]
  (let [views (ui/gen-from-desc {} description)
        baseviews (ui/get-base-ids description)
        viewids (ui/collect-visible-ids views baseviews "")]
    (assoc state :views views :baseviews baseviews :viewids viewids)))


(defn execute-commands [{commands :commands :as state}]
  (reduce (fn [oldstate {text :text :as command}]
            (cond

              (= text "attack")
              (execute-attack oldstate command)

              ;; ui
              
              (= text "set-hitpower") ; update base metrics and generate new metrics for actor
              (let [actor (get-in oldstate [:world :actors 0])
                    nbase (-> (get-in actor [:metrics :base])
                              (assoc :hitpower (:ratio command))
                              (actor/basemetrics-normalize :hitpower))
                    nmetrics (actor/generate-metrics nbase)]
                (-> oldstate (assoc-in [:world :actors 0 :metrics] nmetrics) (update-gen-sliders)))
              (= text "set-hitrate") ; update base metrics and generate new metrics for actor
              (let [actor (get-in oldstate [:world :actors 0])
                    nbase (-> (get-in actor [:metrics :base])
                              (assoc :hitrate (:ratio command))
                              (actor/basemetrics-normalize :hitrate))
                    nmetrics (actor/generate-metrics nbase)]
                (-> oldstate (assoc-in [:world :actors 0 :metrics] nmetrics) (update-gen-sliders)))
              (= text "set-height") ; update base metrics and generate new metrics for actor
              (let [actor (get-in oldstate [:world :actors 0])
                    nbase (-> (get-in actor [:metrics :base])
                              (assoc :height (:ratio command))
                              (actor/basemetrics-normalize :height))
                    nmetrics (actor/generate-metrics nbase)]
                (-> oldstate (assoc-in [:world :actors 0 :metrics] nmetrics) (update-gen-sliders)))
              (= text "set-speed") ; update base metrics and generate new metrics for actor
              (let [actor (get-in oldstate [:world :actors 0])
                    nbase (-> (get-in actor [:metrics :base])
                              (assoc :speed (:ratio command))
                              (actor/basemetrics-normalize :speed))
                    nmetrics (actor/generate-metrics nbase)]
                (-> oldstate (assoc-in [:world :actors 0 :metrics] nmetrics) (update-gen-sliders)))
              (= text "set-stamina") ; update base metrics and generate new metrics for actor
              (let [actor (get-in oldstate [:world :actors 0])
                    nbase (-> (get-in actor [:metrics :base])
                              (assoc :stamina (:ratio command))
                              (actor/basemetrics-normalize :stamina))
                    nmetrics (actor/generate-metrics nbase)]
                (-> oldstate (assoc-in [:world :actors 0 :metrics] nmetrics) (update-gen-sliders)))
              (= text "randomize")
              (let [actor (get-in oldstate [:world :actors 0])
                    nbase (-> (actor/basemetrics-random)
                              (actor/basemetrics-normalize :height))
                    nmetrics (actor/generate-metrics nbase)]
                (-> oldstate (assoc-in [:world :actors 0 :metrics] nmetrics) (update-gen-sliders)))
              
              (= text "show-menu") (load-ui oldstate layouts/menu)
              (= text "continue") (load-ui oldstate (if (= (:curr-level oldstate) 0) layouts/generator layouts/hud))
              (= text "options") (load-ui oldstate layouts/options)
              (= text "options back") (load-ui oldstate layouts/menu)

              (= text "left") (assoc-in oldstate [:keycodes 37] true)
              (= text "right") (assoc-in oldstate [:keycodes 39] true)
              (= text "jump") (assoc-in oldstate [:keycodes 38] true)
              (= text "down") (assoc-in oldstate [:keycodes 40] true)
              (= text "run") (assoc-in oldstate [:keycodes 32] true)
              (= text "punch") (assoc-in oldstate [:keycodes 70] true)
              (= text "kick") (assoc-in oldstate [:keycodes 83] true)
              (= text "block") (assoc-in oldstate [:keycodes 68] true)
              
              (= text "start-game") (load-next-level oldstate)
              (= text "next-level") (load-next-level oldstate)

              :else oldstate))
          state
          commands))


(defn draw-ui [{:keys [gui views viewids] :as state} frame]
  (let [projection (math4/proj_ortho 0 (.-innerWidth js/window) (.-innerHeight js/window) 0 -10.0 10.0)
        newgui (uiwebgl/draw! gui projection (map views viewids))]
    (assoc state :gui newgui)))

  
(defn update-ui [{:keys [views baseviews commands] :as state} touchevent]
  (let [results (if touchevent
                  (let [touched-views (ui/collect-pressed-views views (:point touchevent))]
                    (reduce
                     (fn [result {:keys [class] :as view}]
                       (cond
                         (= class "Slider") (conj result (ui/touch-slider view views touchevent))
                         (= class "Button") (conj result (ui/touch-button view views touchevent))
                         :else result))
                     []
                     (map views touched-views)))
                  [])
        newviews (reduce (fn [oldviews {newviews :views}]
                           (reduce #(assoc oldviews (:id %2) %2) oldviews newviews))
                         views
                         results)
        newcommands (map :command results)]
    (-> state
        (assoc :views (ui/align newviews baseviews 0 0 (. js/window -innerWidth) (. js/window -innerHeight)))
        (assoc :commands (concat commands newcommands)))))


(defn draw-world [{:keys [gfx trans floatbuffer] {:keys [actors particles surfacelines] :as world} :world :as state} frame svglevel]
  "draws background, actors, masses with projection"
  (if (:inited world)
    (let [actor (first actors)
          [fax fay] (:p (get-in actor [:masses :base_l]))
          [fbx fby] (:p (get-in actor [:masses :base_r]))
          [tx ty] [ (+ fax (/ (- fbx fax ) 2)) (+ fay (/ (- fby fay) 2))  ]
          ratio 1.0
          r (/ (.-innerWidth js/window) (.-innerHeight js/window) )
          h (* 350.0 ratio)
          w (* h r)
          [l r b t :as vis-rect] [(- tx w) (+ tx w) (+ ty h) (- ty h)]
          projection (math4/proj_ortho (+ l 50) (- r 50) (- b 50) (+ t 50) -1.0 1.0)    
          variation (Math/floor (mod (/ frame 10.0) 3.0 ))
          newgfx (if svglevel (webgl/loadshapes gfx (filter #(if (:id %) (not (clojure.string/includes? (:id %) "Pivot")) true) svglevel)) gfx)

          newbuf (floatbuf/empty! floatbuffer)
          newbuf1 (reduce (fn [oldbuf actor] (actorskin/get-skin-triangles actor oldbuf variation vis-rect)) newbuf (rseq actors))]
      ;; draw triangles
      (webgl/clear! newgfx)
      (webgl/drawshapes! newgfx projection trans variation)
      (webgl/drawtriangles! newgfx projection newbuf1)

      (let [newbuf2 (floatbuf/empty! newbuf1)
            newbuf3 (reduce (fn [oldbuf particle] (particle/get-point particle oldbuf)) newbuf2 particles) ;; particles
            newbuf4 (reduce (fn [oldbuf actor] (actorskin/getpoints actor oldbuf vis-rect)) newbuf3 actors)]

        ;; draw points
        (webgl/drawpoints! newgfx projection newbuf4)

        (let [newbuf5 (floatbuf/empty! newbuf4)
              newbuf6 (reduce (fn [oldbuf actor] (actorskin/getlines actor oldbuf vis-rect)) newbuf5 actors)
              newbuf7 (floatbuf/append! newbuf6 surfacelines)]
          ;; draw lines
          (webgl/drawlines! newgfx projection newbuf7)
      (-> state
          (assoc :gfx newgfx)
          (assoc-in [:world :vis-rect] vis-rect)
          (assoc :floatbuffer newbuf7)))))
    state))


(defn update-world
  "updates phyisics and actors"
  [{{:keys [actors surfaces particles inited endpos vis-rect] :as world} :world keycodes :keycodes commands :commands :as state} svglevel]

  (cond
    
    inited ; create new state
    (let [currcodes {:left (keycodes 37)
                     :right (keycodes 39)
                     :up (keycodes 38)
                     :down (keycodes 40)
                     :punch (keycodes 70)
                     :run (keycodes 32)
                     :kick (keycodes 83)
                     :block (keycodes 68)}
          newhero (actor/update-actor (first actors) currcodes surfaces 1.0)
          newactors (vec (concat [ newhero ] (map (fn [ actor ] (actor/update-actor actor {} surfaces 1.0)) (rest actors))))

          ;; extract commands
          newcommands (reduce (fn [result {comms :commands :as actor}] (if (empty? comms) result (conj result comms))) commands newactors)

          ;; remove commands if needed
          newnewactors (if (empty? newcommands)
                         newactors
                         (mapv (fn [{comms :commands :as actor}] (if (empty? comms) actor (-> actor (assoc :commands []) (assoc :action-sent true)))) newactors)) 

          ;; check finish sign
          newnewcommands (let [[ex ey] endpos
                          [bx by] (get-in newhero [:masses :base_l :p])
                          dx (- bx ex)
                          dy (- by ey)]
                           (if (and (< (Math/abs dx) 50.0) (< (Math/abs dy) 50))
                             (conj newcommands {:text "next-level"})
                             newcommands))

          newparticles (map (fn [particle] (particle/upd particle vis-rect)) particles)
          
          newworld (assoc world :actors newnewactors :particles newparticles)]

      (-> state
          (assoc :commands newnewcommands)
          (assoc :world newworld)))
    
    svglevel ; load new level
    (let [[ l r b t] vis-rect
          points (map :path (filter #(and (= (% :id) "Surfaces") (not (contains? % :color))) svglevel ))
          pivots (filter #(if (:id %) (clojure.string/includes? (:id %) "Pivot") false) svglevel)
          surfaces (phys2/surfaces-from-pointlist points)
          lines (clj->js (reduce (fn [result {[tx ty] :t [bx by] :b}] (concat result [tx ty 1.0 1.0 1.0 1.0 (+ tx bx) (+ ty by) 1.0 1.0 1.0 1.0])) [] surfaces))
          seeds (map #(particle/init (rand 500.0) (rand 500.0) [1.0 1.0 1.0 0.2] [(+ 0.1 (rand 0.6)) (+ 0.05 (rand 0.3))]  :seed) (take 20 (cycle "a")))
          newworld (-> world
                       (create-actors pivots)
                       (assoc :inited true)
                       (assoc :particles seeds)
                       (assoc :surfaces surfaces)
                       (assoc :surfacelines lines))]
      (-> state
          (assoc :world newworld)
          (update-gen-sliders)))
      
    :else ; return unchanged
    state))


(defn update-keycodes [state keyevent]           
  (let [keycodes (if keyevent
                   (assoc (:keycodes state) (:code keyevent) (:value keyevent))
                   (:keycodes state))
        [tx ty] (:trans state)
        [sx sy] (:speed state)
        nsx (cond
              (keycodes 37) (- sx 0.4)
              (keycodes 39) (+ sx 0.4)
              "default" (* sx 0.9))
        nsy (cond
              (keycodes 38) (- sy 0.4)
              (keycodes 40) (+ sy 0.4)
              "default" (* sy 0.9))
        ntx (+ tx sx)
        nty (+ ty sy)]  
    (-> state
        (assoc :keycodes keycodes)
        (assoc :speed [nsx nsy])
        (assoc :trans [ntx nty]))))


(defn animate [state draw-fn]
  "main runloop, syncs animation to display refresh rate"
  (letfn [(loop [prestate frame]
            (fn [time]
              (let [newstate (if (> time 0)
                               (draw-fn prestate frame time)
                               prestate)]
                (.requestAnimationFrame
                 js/window
                 (loop newstate (inc frame))))))]
    ((loop state 0) 0 )))


(defn main []
  "entering point"
  (let [gfx (webgl/init)
        gui (uiwebgl/init)
        svgch (chan) ;; level svg channel
        imgch (chan) ;; texture image channel
        tchch (chan) ;; touch channel
        keych (chan) ;; key press channel
        views (ui/gen-from-desc {} layouts/generator) ; viewmap contains all visible views as id-desc pairs
        baseviews (ui/get-base-ids layouts/generator)
        viewids (ui/collect-visible-ids views baseviews  "")
        world {:inited false
               :actors [] ; (actor/init 580.0 300.0)]
               :guns []
               :infos []
               :endpos [0 0]
               :surfaces []
               :surfacelines []
               :particles []
               :vis-rect [0 0 0 0]} ; visible rect in world
        state {:gfx gfx
               :gui gui
               :world world
               :views views
               :baseviews baseviews
               :viewids viewids
               :curr-level 1
               :texfile "font.png"
               :floatbuffer (floatbuf/create!)
               :keycodes {}
               :commands []
               :trans [500.0 300.0]
               :speed [0.0 0.0]
               :svgch svgch}]
  
    (resize-context!)
    (init-events! keych tchch)
    (load-image! imgch (:texfile state))
    (load-level! svgch (:curr-level state))
    
    (animate state
             (fn [prestate frame time]
               (if (= (mod frame 1) 0 ) ; frame skipping for development
                 (let [svglevel (poll! svgch)
                       teximage (poll! imgch)
                       keyevent (poll! keych)
                       tchevent (poll! tchch)
                       
                       newstate (-> prestate
                                    (assoc :commands [])
                                    ;; world
                                    (update-keycodes keyevent)
                                    (update-world svglevel)
                                    (draw-world frame svglevel)
                                    ;; ui
                                    (update-ui tchevent)
                                    (draw-ui frame)
                                    (execute-commands)
                                    )]
                   newstate)
                 prestate)))))

(main)
