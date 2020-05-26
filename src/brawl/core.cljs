(ns ^:figwheel-hooks brawl.core
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [tubax.core :refer [xml->clj]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! chan put! take! poll!]]
            [cljs.core.async :refer-macros [go]]
            [brawl.svg :as svg]
            [gui.webgl :as uiwebgl]
            [gui.math4 :as math4]
            [brawl.shape :as shape]
            [brawl.webgl :as webgl]
            [brawl.actor :as actor]
            [brawl.actorskin :as actorskin]
            [mpd.phys2 :as phys2]
            [mpd.math2 :as math2]
            [brawl.ui :as brawlui]
            [brawl.names :as names]
            [brawl.particle :as particle]
            [brawl.layouts :as layouts]
            [brawl.defaults :as defaults]
            [brawl.floatbuffer :as floatbuf])
  (:import [goog.events EventType]))


(defn resize-context!
  "resize canvas on window resize"
  []
  (dom/setProperties
   (dom/getElement "main")
   (clj->js {:width (.-innerWidth js/window)
             :height (.-innerHeight js/window)})))


(defn load-font!
  "load external font"
  [state name url]
  (let [font (js/FontFace. name (str "url(" url ")"))]
    (.then (.load font) (fn []
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


(defn init-events!
  "start event listening"
  [state]
  (let [key-codes (atom {})
        mouse-down (atom false)]
    
    (events/listen
     js/document
     EventType.KEYDOWN
     (fn [event]
       (let [code (.-keyCode event)
             prev (get @key-codes code)]
         (swap! key-codes assoc code true)
         (if (not prev) (put! (:msgch state) {:id "key" :code (.-keyCode event) :value true})))))

    (events/listen
     js/document
     EventType.KEYUP
     (fn [event]
       (let [code (.-keyCode event)
             prev (get @key-codes code)]
         (swap! key-codes assoc code false)
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
       (swap! mouse-down not)
       (put! (:msgch state) {:id "mouse" :type "down" :point [(.-clientX event) (.-clientY event)]})))

    (events/listen
     js/document
     EventType.MOUSEUP
     (fn [event]
       (swap! mouse-down not)
       (put! (:msgch state) {:id "mouse" :type "up" :point [(.-clientX event) (.-clientY event)]})))

    (events/listen
     js/document
     EventType.MOUSEMOVE
     (fn [event]
       (if @mouse-down (put! (:msgch state) {:id "mouse" :type "down" :point [(.-clientX event) (.-clientY event)]}))))

    (events/listen
     js/window
     EventType.RESIZE
     (fn [event]
       (put! (:msgch state) {:id "resize"})
       (resize-context!)))))


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


(defn execute-commands [{:keys [curr-level] commands :commands :as state}]
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
           (-> oldstate (assoc-in path-metrics nmetrics) (brawlui/update-gen-sliders)))
         (= text "set-hitrate") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :hitrate (:ratio command))
                         (actor/basemetrics-normalize :hitrate))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (brawlui/update-gen-sliders)))
         (= text "set-height") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :height (:ratio command))
                         (actor/basemetrics-normalize :height))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (brawlui/update-gen-sliders)))
         (= text "set-speed") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :speed (:ratio command))
                         (actor/basemetrics-normalize :speed))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (brawlui/update-gen-sliders)))
         (= text "set-stamina") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :stamina (:ratio command))
                         (actor/basemetrics-normalize :stamina))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (brawlui/update-gen-sliders)))
         (= text "randomize")
         (let [nbase (-> (actor/basemetrics-random)
                         (actor/basemetrics-normalize :height))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (brawlui/update-gen-sliders)))
         
         (= text "show-menu") (brawlui/load-ui oldstate layouts/menu)
         (= text "continue") (brawlui/load-ui oldstate (if (= (:curr-level oldstate) 0) layouts/generator layouts/hud))
         (= text "options")
         (let [newstate (brawlui/load-ui oldstate layouts/options)]
           (-> newstate
               (brawlui/set-slider-value :Music (get-in newstate [:volumes :music]))
               (brawlui/set-slider-value :Effects (get-in newstate [:volumes :effects]))
               (brawlui/align)))
         
         (= text "donate") (defaults/save-defaults! oldstate)
         (= text "options back") (brawlui/load-ui oldstate layouts/menu)

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
             (brawlui/load-ui layouts/info)
             (update :commands conj {:text "load-level"}))
         
         (= text "next-level")
         (if (= curr-level 6)
           ;; show congrats screen
           (brawlui/load-ui oldstate layouts/info)
           ;; load next level
           (-> oldstate
               (brawlui/load-ui layouts/info)
               (update :commands conj {:text "load-level"})))

         (= text "load-level") (load-next-level oldstate)

         :else oldstate)))
   (assoc state :commands [])
   commands))


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
          [l r b t :as view-rect] [(- tx w) (+ tx w) (+ ty h) (- ty h)]
          projection (math4/proj_ortho (+ l 50) (- r 50) (- b 50) (+ t 50) -1.0 1.0)    
          variation (Math/floor (mod (/ frame 10.0) 3.0 ))
          newgfx (if (and msg (= (:id msg) "level")) (webgl/loadshapes gfx (filter #(if (:id %) (not (clojure.string/includes? (:id %) "Pivot")) true) (:shapes msg))) gfx)

          newbuf (floatbuf/empty! floatbuffer)
          newbuf1 (reduce (fn [oldbuf [id actor]]
                            (actorskin/get-skin-triangles actor oldbuf variation view-rect))
                          newbuf
                          actors)]
      ;; draw triangles
      (webgl/clear! newgfx)
      (webgl/drawshapes! newgfx projection trans variation)
      (webgl/drawtriangles! newgfx projection newbuf1)

      (let [newbuf2 (floatbuf/empty! newbuf1)
            newbuf3 (reduce (fn [oldbuf particle] (particle/get-point particle oldbuf)) newbuf2 particles) ;; particles
            newbuf4 (reduce (fn [oldbuf [id actor]] (actorskin/getpoints actor oldbuf view-rect)) newbuf3 actors)]

        ;; draw points
        (webgl/drawpoints! newgfx projection newbuf4)

        (let [newbuf5 (floatbuf/empty! newbuf4)
              newbuf6 (reduce (fn [oldbuf [id actor]] (actorskin/getlines actor oldbuf view-rect)) newbuf5 actors)
              newbuf7 (floatbuf/append! newbuf6 surfacelines)]
          ;; draw lines
          (webgl/drawlines! newgfx projection newbuf7)

          (-> state
              (assoc :gfx newgfx)
              (assoc-in [:world :view-rect] view-rect)
              (assoc :floatbuffer newbuf7)))))
    state))


(defn update-world
  "updates phyisics and actors"
  [{{:keys [actors surfaces particles inited loaded endpos view-rect finished] :as world} :world keycodes :keycodes curr-level :curr-level commands :commands sounds :sounds controls :controls :as state} msg]
  (if loaded
    (let [newactors (reduce (fn [result [id actor]]
                              (let [newactor (cond
                                               (not= :hero id) (actor/update-actor actor nil surfaces actors 1.0)
                                               :else (actor/update-actor actor controls surfaces actors 1.0))]
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
          ;; finished sign?
          ended (let [[ex ey] endpos
                      [bx by] (get-in newactors [:hero :bases :base_l :p])
                      dx (- bx ex)
                      dy (- by ey)]
                  (if (and (< (Math/abs dx) 50.0) (< (Math/abs dy) 50)) true false))
          
          newnewcommands (if (and (not finished) ended) (conj newcommands {:text "next-level"}) newcommands)

          newparticles (map (fn [particle] (particle/upd particle view-rect)) particles)
          
          newworld (assoc world :actors newnewactors :particles newparticles :finished ended)]

      (-> state
          (assoc :commands newnewcommands)
          (assoc :world newworld)))
    state))


(defn reset-world [{:keys [curr-level view-rect] :as state} msg]
  (if (and msg (= (:id msg) "level"))
    (let [svglevel (:shapes msg)
          [l r b t] view-rect
          points (map :path (filter #(and (= (% :id) "Surfaces") (not (contains? % :color))) svglevel ))
          pivots (filter #(if (:id %) (clojure.string/includes? (:id %) "Pivot") false) svglevel)
          surfaces (phys2/surfaces-from-pointlist points)
          lines (clj->js (reduce (fn [result {[tx ty] :t [bx by] :b}] (concat result [tx ty 1.0 1.0 1.0 1.0 (+ tx bx) (+ ty by) 1.0 1.0 1.0 1.0])) [] surfaces))
          seeds (map #(particle/init 0.0 0.0 [1.0 1.0 1.0 0.5] [(+ 0.1 (rand 0.6)) (+ 0.05 (rand 0.3))]  :seed) (range 0 20))
          newworld (-> {:actors {}
                        :guns []
                        :infos []
                        :inited true
                        :loaded true
                        :finished false
                        :particles seeds
                        :surfaces surfaces
                        :surfacelines lines}
                       (create-actors pivots))]

      (cond-> state
        true (assoc :world newworld)
        true (brawlui/update-gen-sliders)
        true (assoc :commands [])
        (= curr-level 0) (brawlui/load-ui layouts/generator)
        (> curr-level 0) (brawlui/load-ui layouts/hud)))
    state))


(defn update-controls [{:keys [keycodes controls] :as state } msg]
  (if (and msg (= (:id msg) "key"))
    (let [new-codes (assoc (:keycodes state) (:code msg) (:value msg))
          new-controls {:left (new-codes 37)
                       :right (new-codes 39)
                       :up (new-codes 38)
                       :down (new-codes 40)
                       :punch (new-codes 70)
                       :run (new-codes 32)
                       :kick (new-codes 83)
                       :block (new-codes 68)}]
      (-> state
          (assoc :keycodes new-codes)
          (assoc :controls new-controls)))
    state))


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
               :loaded false
               :guns []
               :infos []
               :actors {}
               :endpos [0 0]
               :surfaces []
               :particles []
               :surfacelines []
               :view-rect [0 0 0 0]}

        sound {:shot (js/Audio. "sounds/shot.mp3")
               :theme (js/Audio. "sounds/theme.mp3")
               :argh0 (js/Audio. "sounds/argh0.mp3")
               :argh1 (js/Audio. "sounds/argh1.mp3")
               :argh2 (js/Audio. "sounds/argh2.mp3")
               :death0 (js/Audio. "sounds/death0.mp3")
               :death1 (js/Audio. "sounds/death1.mp3")
               :punch0 (js/Audio. "sounds/punch0.mp3")
               :punch1 (js/Audio. "sounds/punch1.mp3")
               :punch2 (js/Audio. "sounds/punch2.mp3")}

        state {:ui (brawlui/init)
               :gfx (webgl/init)
               :world world
               :curr-level 0
               :floatbuffer (floatbuf/create!)
               :commands []
               :keycodes {}
               :controls {:left false
                          :right false
                          :up false
                          :down false
                          :punch false
                          :run false
                          :kick false
                          :block false}
               :trans [500.0 300.0]
               :speed [0.0 0.0]
               :msgch (chan)
               :sounds sound
               :volumes {:music 0.5
                         :effects 0.5}}

        final (-> state
                  (defaults/load-defaults!)
                  (brawlui/load-ui layouts/info))]

    (load-font! final "Ubuntu Bold" "css/Ubuntu-Bold.ttf")
    (load-level! final (:curr-level final))
    (init-events! final)

    (animate
     final
     (fn [prestate frame time]
       (if (= (mod frame 1) 0 ) ; frame skipping for development
         (let [msg (poll! (:msgch prestate))]
           (-> prestate
               (reset-world msg)
               (update-controls msg)
               (execute-commands)
               ;; world
               (update-world msg)
               (draw-world frame msg)
               ;; ui
               (brawlui/update-ui msg)
               (brawlui/draw-ui frame)))
         prestate)))))

(defonce mainloop (main))
