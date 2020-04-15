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
            [brawl.layouts :as layouts])
  (:import [goog.events EventType]))
  

(defn load-level! [channel name]
  "load level svg's"
  (go
    (let [response (<! (http/get name
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
     (put! tchch {:type "down" :point [(.-clientX event) (.-clientY event)]})))

  (events/listen
   js/document
   EventType.MOUSEUP
   (fn [event]
     (put! tchch {:type "up" :point [(.-clientX event) (.-clientY event)]})))

  (events/listen
   js/window
   EventType.RESIZE
   (fn [event] (resize-context!))))


(defn draw-ui! [{:keys [gui views viewids] :as state} frame]
  (let [projection (math4/proj_ortho 0 (.-innerWidth js/window) (.-innerHeight js/window) 0 -10.0 10.0)
        newgui (uiwebgl/draw! gui projection (map views viewids))]
    (assoc state :gui newgui)))

  
(defn update-ui [views baseviews touchevent]
  (if touchevent
    (let [touched-views (ui/collect-pressed-views views (:point touchevent))]
      (println "touched-views" touched-views)
      ))
  (ui/align views baseviews 0 0 (. js/window -innerWidth) (. js/window -innerHeight)))
  

(defn update-translation [state keyevent]           
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


(defn draw-world! [state frame]
  "draws background, actors, masses with projection"
  (let [gfx (:gfx state)
        world (:world state)
        actor (first (:actors world))
        [fax fay] (:p (get-in actor [:masses :base_l]))
        [fbx fby] (:p (get-in actor [:masses :base_r]))
        [tx ty] [ (+ fax (/ (- fbx fax ) 2)) (+ fby (/ (- fby fay) 2))  ]
        [sx sy] (:speed state)
        ratio (/ (min (max (Math/abs sx) (Math/abs sy)) 40.0) 40.0)
        r (/ (.-innerWidth js/window) (.-innerHeight js/window) )
        h 300.0
        w (* h r)
        projection (math4/proj_ortho
                    (- tx (+ w (* ratio 50.0)))
                      (+ tx (+ w (* ratio 50.0)))
                      (+ ty (+ h (* ratio 50.0)))
                      (- ty (+ h (* ratio 50.0)))
                      -1.0 1.0)
        
        variation (Math/floor (mod (/ frame 20.0) 3.0 ))]
    (webgl/clear! gfx)
    (webgl/drawshapes! gfx projection (:trans state) variation)

    (doall (map (fn [act]
                  (webgl/drawtriangles! gfx projection (actorskin/get-skin-triangles act variation))
                  ;(webgl/drawpoints! gfx projection (actorskin/getpoints act))
                  ;(webgl/drawlines! gfx projection (actorskin/getlines act))
                  ) (:actors world)))
    
    (webgl/drawpoints! gfx projection (map :p (vals (:masses world))))
    (webgl/drawlines! gfx projection (:surfacelines world))
    ))


(defn create-actors
  "add actors, infos, guns and enpoint to scene based on pivot points in svg"
  [state pivots]
  (reduce (fn [{:keys [actors guns infos] :as state1} {id :id path :path}]         
            (let [pos (nth path 3)
                  toks (clojure.string/split id #"_")
                  type (first (second toks))]
              (cond (= type "l") (assoc state1 :actors (conj actors (actor/init (first pos) (second pos))))
                    (= type "g") (assoc state1 :guns (conj guns {:pos pos}))
                    (= type "e") (assoc state1 :endpos pos)
                    (= type "i") (assoc state1 :infos (conj infos {:pos pos :index (js/parseInt (second type))})))     
         )) state pivots))


(defn update-world
  "updates phyisics and actors"
  [{:keys [actors surfaces masses setup] :as world} keycodes svglevel]
  (cond
    setup ; create new state
    (let [newactors (vec (map #(actor/update-actor % {:left (keycodes 37)
                                                      :right (keycodes 39)
                                                      :up (keycodes 38)
                                                      :down (keycodes 40)
                                                      :punch (keycodes 70)
                                                      :run (keycodes 32)
                                                      :kick (keycodes 83)
                                                      :block (keycodes 68)
                                                      } surfaces 1.0) actors))
          newmasses (-> masses
                        (phys2/add-gravity [0.0 0.2])
                        ;;(phys2/keep-angles (:aguards state))
                        ;;(phys2/keep-distances (:dguards state))
                        (phys2/move-masses surfaces))]
      (-> world
          (assoc :actors newactors)
          (assoc :masses newmasses)))
    svglevel ; load new level
    (let [points (map :path (filter #(and (= (% :id) "Surfaces") (not (contains? % :color))) svglevel ))
          pivots (filter #(clojure.string/includes? (:id %) "Pivot") svglevel)
          surfaces (phys2/surfaces-from-pointlist points)
          lines (reduce (fn [result {t :t b :b}] (conj result t (math2/add-v2 t b))) [] surfaces)]       
      (-> world
          (create-actors pivots)
          (assoc :setup true)
          (assoc :surfaces surfaces)
          (assoc :surfacelines lines)))
    :else ; return unchanged
    world))


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
        views (ui/gen-from-desc {} layouts/generator) ; viewmap contains all visible views as id-desc pairs
        baseviews (ui/get-base-ids layouts/generator)
        viewids (ui/collect-visible-ids views baseviews  "")
        world {:setup false
               :actors [] ; (actor/init 580.0 300.0)]
               :guns []
               :infos []
               :endpos [0 0]
               :masses {:0 (phys2/mass2 500.0 300.0 1.0 1.0 0.9)}
               :dguards []
               :aguards []
               :surfaces []
               :surfacelines []}
        state {:gfx gfx
               :gui gui
               :world world
               :views views
               :baseviews baseviews
               :viewids viewids
               :level_file "level0.svg"
               :texfile "font.png"
               :keycodes {}
               :trans [500.0 300.0]
               :speed [0.0 0.0]}
        svgch (chan) ;; level svg channel
        imgch (chan) ;; texture image channel
        tchch (chan) ;; touch channel
        keych (chan)];; key press channel
    
    (resize-context!)
    (init-events! keych tchch)
    (load-image! imgch (:texfile state))
    (load-level! svgch (:level_file state))
    
    (animate state (fn [prestate frame time]
       (if (= (mod frame 1) 0 ) ; frame skipping for development
         (let [svglevel (poll! svgch)
               teximage (poll! imgch)
               keyevent (poll! keych)
               tchevent (poll! tchch)

               newviews (update-ui (:views prestate) (:baseviews prestate) tchevent)      

               newworld (update-world (:world prestate) (:keycodes prestate) svglevel)

               newstate (cond-> prestate
                          svglevel (assoc :gfx (webgl/loadshapes (:gfx prestate) svglevel))
                          true (assoc :world newworld)
                          true (assoc :views newviews)
                          true (update-translation keyevent))]
           (if (:setup newworld) (draw-world! newstate frame))
           (draw-ui! newstate frame))
         prestate)))))

(main)
