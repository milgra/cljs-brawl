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
            [brawl.names :as names]
            [brawl.particle :as particle]
            [brawl.layouts :as layouts]
            [brawl.floatbuffer :as floatbuf])
  (:import [goog.events EventType]))


(defn resize-context!
  "resize canvas on window resize"
  []
  (let [canvas (. js/document getElementById "main")]
        (set! (. canvas -width) (. js/window -innerWidth))
        (set! (. canvas -height) (. js/window -innerHeight))))


(defn load-font!
  "start loading external font"
  [state name url]
  (let [font (js/FontFace. name (str "url(" url ")"))]
    (.then (.load font) #(
                          do
                          (.add (.-fonts js/document) font)
                          (put! (:msgch state) {:id "redraw-ui"})))))


(defn load-level!
  "load level svg's"
  [state level]
  (go
    (let [channel (:msgch state)
          response (<! (http/get (str "levels/level" level ".svg")
                                 {:with-credentials? false}))
          xmlstr (xml->clj (:body response) {:strict false})
          shapes (svg/psvg xmlstr "")]
      (put! channel {:id "level" :shapes shapes}))))


(defn load-ui
  "load new ui stack"
  [state description]
  (let [views (ui/gen-from-desc {} description)
        baseid (keyword (:id description))
        viewids (ui/collect-visible-ids views [baseid] "")
        alignedviews (ui/align views [baseid] 0 0 (. js/window -innerWidth) (. js/window -innerHeight))]
    (assoc state
           :baseid baseid
           :views alignedviews
           :viewids viewids)))


(defn local-storage-supported?
  []
  (let [item "workshub-test-ls"]
    (try
      (.setItem js/localStorage item item)
      (.removeItem js/localStorage item)
      true
      (catch :default _
        false))))


(defn load-defaults! [state]
  (println "load-defaults saved" (.getItem js/localStorage "state-saved?") "state" (.getItem js/localStorage "state"))
  (if (and (local-storage-supported?) (= "true" (.getItem js/localStorage "state-saved?")))
    (let [state-js (cljs.reader/read-string (.getItem js/localStorage "state"))]
      (println "state-js" (:curr-level state-js))
      ;; todo validate with specs
      (merge state state-js))
    state))


(defn save-defaults! [state]
  (println "save-defaults")
  (if (local-storage-supported?)
    (let [state-js {:curr-level (:curr-level state)
                    :volumes {:music (get-in state [:volumes :music])
                              :effects (get-in state [:volumes :effects])}
                    }]
      (.setItem js/localStorage "state-saved?" true)
      (.setItem js/localStorage "state" state-js))
  state))


(def keycodes (atom {}))
(def mousedown (atom false))

(defn init-events!
  "start event listening"
  [state]
  (events/listen
   js/document
   EventType.KEYDOWN
   (fn [event]
     (let [code (.-keyCode event)
           prev (get @keycodes code)]
       (swap! keycodes assoc code true)
       (if (not prev) (put! (:msgch state) {:id "key" :code (.-keyCode event) :value true})))))

  (events/listen
   js/document
   EventType.KEYUP
   (fn [event]
     (let [code (.-keyCode event)
           prev (get @keycodes code)]
       (swap! keycodes assoc code false)
       (if prev (put! (:msgch state) {:id "key" :code (.-keyCode event) :value false})))))

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
     (put! (:msgch state) {:id "mouse" :type "down" :point [(.-clientX event) (.-clientY event)]})))

  (events/listen
   js/document
   EventType.MOUSEUP
   (fn [event]
     (swap! mousedown not)
     (put! (:msgch state) {:id "mouse" :type "up" :point [(.-clientX event) (.-clientY event)]})))

  (events/listen
   js/document
   EventType.MOUSEMOVE
   (fn [event]
     (if @mousedown (put! (:msgch state) {:id "mouse" :type "down" :point [(.-clientX event) (.-clientY event)]}))))

  (events/listen
   js/window
   EventType.RESIZE
   (fn [event] (resize-context!))))

(def colors [0xFF0000FF 0x00FF00FF 0x0000FFFF 0xFF00FFFF 0x00FFFFFFF 0xFFFF00FF])

(defn create-actors
  "add actors, infos, guns and enpoint to scene based on pivot points in svg"
  [state pivots]
  (reduce (fn [{:keys [actors guns infos] :as oldstate} {id :id path :path}]         
            (let [pos (nth path 3)
                  toks (clojure.string/split id #"_")
                  type (first (second toks))]
              (cond
                (= type "l") (let [team (js/parseInt (second (nth toks 2)))
                                   level (js/parseInt (second (second toks)))
                                   name (if (= level 0) :hero (keyword (names/getname)))
                                   color (nth colors team)]
                               (update oldstate :actors assoc name (actor/init (first pos) (second pos) name color)))
                (= type "g") (assoc oldstate :guns (conj guns {:pos pos}))
                (= type "e") (assoc oldstate :endpos pos)
                (= type "i") (assoc oldstate :infos (conj infos {:pos pos :index (js/parseInt (second type))})))     
              )) state pivots))


(defn update-gen-sliders [{:keys [views world] :as state}]
  (let [{:keys [height hitpower hitrate stamina speed]} (get-in world [:actors :hero :metrics :base])
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


(defn execute-attack [{ {actors :actors particles :particles} :world sounds :sounds :as state} command]
  ;; hittest other actors
  (let [[dx dy] (math2/resize-v2 (math2/sub-v2 (:target command) (:base command)) 4.0)
        contacts (remove nil? (map (fn [[id actor]] (actor/hitpoint actor command)) actors))
        newparticles (reduce (fn [res [x y]]
                               (concat res
                                       (repeatedly 10 #(particle/init x y [1.0 1.0 1.0 0.5] (math2/resize-v2 [(+ (- 1.0) (rand 2.0)) (+ (- 1.0) (rand 2.0))] (+ 1.0 (rand 2.0))) :dust))
                                       (repeatedly 5 #(particle/init x y [1.0 0.0 0.0 0.5]  [ (+ dx -2.0 (rand 2.0)) (+ dy -2.0 (rand 2.0))] :blood))))
                             [] contacts)
        newactors (reduce (fn [result [id actor]]
                            (let [newactor (actor/hit actor command)]
                              (if (and (> (:health actor) 0 ) (< (:health newactor) 0)) (.play ((keyword (str "death" (rand-int 2))) sounds)))
                              (assoc result id newactor)))
                          {}
                          actors)]
    (when-not (empty? contacts)
      (.play ((keyword (str "punch" (rand-int 3))) sounds))
      (set! (.-loop (:theme sounds)) true)
      (.play (:theme sounds)))
  
    (-> state
        (assoc-in [:world :particles] (concat particles newparticles))
        (assoc-in [:world :actors] newactors ))))


(defn load-next-level [{:keys [curr-level sounds] :as state}]
  (let [next-level (min (inc curr-level) 6)]
    (load-level! state next-level)
    (-> state
        (assoc-in [:world :loaded] false)
        (assoc :curr-level next-level))))


(defn execute-commands [{commands :commands :as state}]
  (reduce
   (fn [oldstate {text :text :as command}]
     (let [hero (get-in oldstate [:worlds :actors :hero])
           path-metrics [:world :actors :hero :metrics]]
       (cond
         (= text "attack")
         (execute-attack oldstate command)
         ;; ui
         (= text "set-hitpower") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :hitpower (:ratio command))
                         (actor/basemetrics-normalize :hitpower))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
         (= text "set-hitrate") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :hitrate (:ratio command))
                         (actor/basemetrics-normalize :hitrate))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
         (= text "set-height") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :height (:ratio command))
                         (actor/basemetrics-normalize :height))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
         (= text "set-speed") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :speed (:ratio command))
                         (actor/basemetrics-normalize :speed))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
         (= text "set-stamina") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :stamina (:ratio command))
                         (actor/basemetrics-normalize :stamina))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
         (= text "randomize")
         (let [nbase (-> (actor/basemetrics-random)
                         (actor/basemetrics-normalize :height))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
         
         (= text "show-menu") (load-ui oldstate layouts/menu)
         (= text "continue") (load-ui oldstate (if (= (:curr-level oldstate) 0) layouts/generator layouts/hud))
         (= text "options")
         (let [newstate (load-ui oldstate layouts/options)]
           (println "EFFECTS" (get-in newstate [:volumes :music]))
           (assoc newstate :views (-> (:views newstate)
                                      (ui/set-slider-value (:Music (:views newstate)) (get-in newstate [:volumes :music]))
                                      (ui/set-slider-value (:Effects (:views newstate)) (get-in newstate [:volumes :effects])))))

         (= text "donate") (save-defaults! oldstate)
         (= text "options back") (load-ui oldstate layouts/menu)

         (= text "left") (assoc-in oldstate [:keycodes 37] true)
         (= text "right") (assoc-in oldstate [:keycodes 39] true)
         (= text "jump") (assoc-in oldstate [:keycodes 38] true)
         (= text "down") (assoc-in oldstate [:keycodes 40] true)
         (= text "run") (assoc-in oldstate [:keycodes 32] true)
         (= text "punch") (assoc-in oldstate [:keycodes 70] true)
         (= text "kick") (assoc-in oldstate [:keycodes 83] true)
         (= text "block") (assoc-in oldstate [:keycodes 68] true)
         
         (= text "start-game")
         (-> oldstate
             (load-ui layouts/info)
             (update :commands conj {:text "load-level"}))

         (= text "next-level")
         (-> oldstate
             (load-ui layouts/info)
             (update :commands conj {:text "load-level"}))

         (= text "load-level") (load-next-level oldstate)

         :else oldstate)))
   (assoc state :commands [])
   commands))


(defn draw-ui [{:keys [gui views viewids] :as state} frame]
  (let [projection (math4/proj_ortho 0 (.-innerWidth js/window) (.-innerHeight js/window) 0 -10.0 10.0)
        newgui (uiwebgl/draw! gui projection (map views viewids))]
    (assoc state :gui newgui)))

  
(defn update-ui [{:keys [baseid views commands viewdesc gui] :as state} msg]
  (let [results (if (and msg (= (:id msg) "mouse"))
                  (let [touched-views (ui/collect-pressed-views views (:point msg))]
                    (reduce
                     (fn [result {:keys [class] :as view}]
                       (cond
                         (= class "Slider") (conj result (ui/touch-slider view views msg))
                         (= class "Button") (conj result (ui/touch-button view views msg))
                         :else result))
                     []
                     (map views touched-views)))
                  [])
        newviews (reduce (fn [oldviews {newviews :views}]
                           (reduce #(assoc oldviews (:id %2) %2) oldviews newviews))
                         views
                         results)
        newcommands (map :command results)]
    (cond-> state
        true (assoc :views (ui/align newviews [baseid] 0 0 (. js/window -innerWidth) (. js/window -innerHeight)))
        true (assoc :commands (concat commands newcommands))
        (and msg (= (:id msg) "redraw-ui")) (assoc :gui (uiwebgl/reset gui)))))


(defn draw-world [{:keys [gfx trans floatbuffer] {:keys [actors particles surfacelines] :as world} :world curr-level :curr-level :as state} frame msg]
  "draws background, actors, masses with projection"
  (if (:inited world)
    (let [hero (:hero actors)
          [fax fay] (:p (get-in hero [:bases :base_l]))
          [fbx fby] (:p (get-in hero [:bases :base_r]))
          [tx ty] [ (+ fax (/ (- fbx fax ) 2)) (+ fay (/ (- fby fay) 2))  ]
          ratio 1.0
          r (/ (.-innerWidth js/window) (.-innerHeight js/window) )
          h (* 350.0 ratio)
          w (* h r)
          [l r b t :as vis-rect] [(- tx w) (+ tx w) (+ ty h) (- ty h)]
          projection (math4/proj_ortho (+ l 50) (- r 50) (- b 50) (+ t 50) -1.0 1.0)    
          variation (Math/floor (mod (/ frame 10.0) 3.0 ))
          newgfx (if (and msg (= (:id msg) "level")) (webgl/loadshapes gfx (filter #(if (:id %) (not (clojure.string/includes? (:id %) "Pivot")) true) (:shapes msg))) gfx)

          newbuf (floatbuf/empty! floatbuffer)
          newbuf1 (reduce (fn [oldbuf [id actor]]
                            (actorskin/get-skin-triangles actor oldbuf variation vis-rect))
                          newbuf
                          actors)]
      ;; draw triangles
      (webgl/clear! newgfx)
      (webgl/drawshapes! newgfx projection trans variation)
      (webgl/drawtriangles! newgfx projection newbuf1)

      (let [newbuf2 (floatbuf/empty! newbuf1)
            newbuf3 (reduce (fn [oldbuf particle] (particle/get-point particle oldbuf)) newbuf2 particles) ;; particles
            newbuf4 (reduce (fn [oldbuf [id actor]] (actorskin/getpoints actor oldbuf vis-rect)) newbuf3 actors)]

        ;; draw points
        (webgl/drawpoints! newgfx projection newbuf4)

        (let [newbuf5 (floatbuf/empty! newbuf4)
              newbuf6 (reduce (fn [oldbuf [id actor]] (actorskin/getlines actor oldbuf vis-rect)) newbuf5 actors)
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
  [{{:keys [actors surfaces particles inited loaded endpos vis-rect] :as world} :world keycodes :keycodes curr-level :curr-level commands :commands sounds :sounds :as state} msg]
  (cond
    loaded ; create new state
    (let [currcodes {:left (keycodes 37)
                     :right (keycodes 39)
                     :up (keycodes 38)
                     :down (keycodes 40)
                     :punch (keycodes 70)
                     :run (keycodes 32)
                     :kick (keycodes 83)
                     :block (keycodes 68)}
          
          newactors (reduce (fn [result [id actor]]
                              (let [newactor (cond
                                               (not= :hero id) (actor/update-actor actor nil surfaces actors 1.0)
                                               :else (actor/update-actor actor currcodes surfaces actors 1.0))]
                                (assoc result id newactor)))
                            {}
                            actors)

          ;; extract commands
          newcommands (reduce (fn [result [id actor]]
                                (if (empty? (:commands actor))
                                  result
                                  (conj result (:commands actor))))
                              commands
                              newactors)

          ;; remove commands if needed
          newnewactors (if (empty? newcommands)
                         newactors
                         (reduce (fn [result [id actor]]
                                   (if (empty? (:commands actor))
                                     result
                                     (assoc result id (-> actor (assoc :commands []) (assoc :action-sent true)))))
                                 newactors
                                 newactors))

          ;; check finish sign
          newnewcommands (let [[ex ey] endpos
                          [bx by] (get-in newactors [:hero :bases :base_l :p])
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
    
    (and msg (= (:id msg) "level")) ; load new level
    (let [svglevel (:shapes msg)
          [ l r b t] vis-rect
          points (map :path (filter #(and (= (% :id) "Surfaces") (not (contains? % :color))) svglevel ))
          pivots (filter #(if (:id %) (clojure.string/includes? (:id %) "Pivot") false) svglevel)
          surfaces (phys2/surfaces-from-pointlist points)
          lines (clj->js (reduce (fn [result {[tx ty] :t [bx by] :b}] (concat result [tx ty 1.0 1.0 1.0 1.0 (+ tx bx) (+ ty by) 1.0 1.0 1.0 1.0])) [] surfaces))
          seeds (map #(particle/init 0.0 0.0 [1.0 1.0 1.0 0.5] [(+ 0.1 (rand 0.6)) (+ 0.05 (rand 0.3))]  :seed) (take 20 (cycle "a")))
          newworld (-> {:actors {}
                        :guns []
                        :infos []
                        :inited true
                        :loaded true
                        :particles seeds
                        :surfaces surfaces
                        :surfacelines lines}
                       (create-actors pivots))]
      
      (cond-> state
        true (assoc :world newworld)
        true (update-gen-sliders)
        (= curr-level 0) (load-ui layouts/generator)
        (> curr-level 0) (load-ui layouts/hud)))

    :else ; return unchanged
    state))


(defn update-keycodes [state msg]
  (let [keycodes (if (and msg (= (:id msg) "key"))
                   (assoc (:keycodes state) (:code msg) (:value msg))
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

  (resize-context!)

  (let [world {:inited false
               :actors {}
               :guns []
               :infos []
               :endpos [0 0]
               :surfaces []
               :surfacelines []
               :particles []
               :vis-rect [0 0 0 0]} ; visible rect in world

        sound {:argh0 (js/Audio. "sounds/argh0.mp3")
               :argh1 (js/Audio. "sounds/argh1.mp3")
               :argh2 (js/Audio. "sounds/argh2.mp3")
               :death0 (js/Audio. "sounds/death0.mp3")
               :death1 (js/Audio. "sounds/death1.mp3")
               :punch0 (js/Audio. "sounds/punch0.mp3")
               :punch1 (js/Audio. "sounds/punch1.mp3")
               :punch2 (js/Audio. "sounds/punch2.mp3")
               :shot (js/Audio. "sounds/shot.mp3")
               :theme (js/Audio. "sounds/theme.mp3")}
        
        state {:gfx (webgl/init)
               :gui (uiwebgl/init)
               :world world
               :views {}
               :baseid nil 
               :viewids []
               :curr-level 0
               :floatbuffer (floatbuf/create!)
               :keycodes {}
               :commands []
               :trans [500.0 300.0]
               :speed [0.0 0.0]
               :msgch (chan)
               :sounds sound
               :volumes {:music 0.5
                         :effects 0.5}}

        final (-> state
                  (load-defaults!)
                  (load-ui layouts/info))]

    (load-font! final "Ubuntu Bold" "css/Ubuntu-Bold.ttf")
    (load-level! final (:curr-level final))
    (init-events! final)
    
    (animate final
             (fn [prestate frame time]
               (if (= (mod frame 1) 0 ) ; frame skipping for development
                 (let [msg (poll! (:msgch prestate))]
                   (-> prestate
                       (execute-commands)
                       ;; world
                       (update-keycodes msg)
                       (update-world msg)
                       (draw-world frame msg)
                       ;; ui
                       (update-ui msg)
                       (draw-ui frame)))
                   prestate)))))
  
(defonce mainloop (main))
