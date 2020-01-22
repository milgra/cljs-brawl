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


(defn init-events! [keych tchch]
  "start event listening"
 
  (events/listen
   js/document
   EventType.KEYDOWN
   (fn [event] (put! keych {:code (.-keyCode event) :value true})))
  
  (events/listen
   js/document
   EventType.KEYUP
   (fn [event] (put! keych {:code (.-keyCode event) :value false})))
  
  (events/listen
   js/window
   EventType.RESIZE
   (fn [event] (resize-context!))))


(defn draw-world! [state frame]
  "draws background, actors, masses with projection"
  (let [
        glstate (state :glstate)
        world (state :world)
        actor ((world :actors) 0)
        head ((get-in actor [:masses :head]) :p)
        [tx ty] head
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
    
    (webgl/clear! glstate)
    (webgl/drawshapes! glstate projection (:trans state) variation)
    (webgl/drawtriangles! glstate projection (actor/get-skin-triangles actor))
    (webgl/drawlines! glstate projection (:surfacelines world))
    (webgl/drawpoints! glstate projection (map :p (vals (:masses world))))
    (webgl/drawpoints! glstate projection (actor/getpoints actor))
    (webgl/drawlines! glstate projection (actor/getlines actor))))


(defn draw-ui! [state frame]
  (let [projection (math4/proj_ortho 0 (.-innerWidth js/window) (.-innerHeight js/window) 0 -10.0 10.0)
        uistate (state :uistate)
        views (state :views)
        viewids (ui/collect-visible-ids views ((views :baseview) :sv) "")
        newstate (uiwebgl/draw! uistate projection (map views viewids))]
    newstate))
  

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


(defn update-world [{:keys [actors surfaces masses] :as world} keycodes]
  "updates phyisics and actors"
  (let [newactor (actor/newstate
                  (first actors)
                  {:left (keycodes 37) :right (keycodes 39) :up false :down false }
                  surfaces
                  1.0)
        ;;newmasses (mass/update-masses masses surfaces 1.0)
        newmasses (-> masses
                      (phys2/add-gravity [0.0 0.2])
                      ;;(phys2/timescale delta)
                      ;;(phys2/keep-angles (:aguards state))
                      ;;(phys2/keep-distances (:dguards state))
                      (phys2/move-masses surfaces)
                      ;;(phys2/timescale (/ 1.0 delta)))
                      )]
    (-> world
        (assoc-in [:actors 0] newactor)
        (assoc :masses newmasses))))


(defn update-ui [views]
  (ui/align
   views
   ((views :baseview) :sv)
   0
   0
   (. js/window -innerWidth)
   (. js/window -innerHeight)))


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
  (let
      [glstate (webgl/init)
       uistate (uiwebgl/init)

       world {:actors [(actor/init 480.0 300.0)]
              :masses {:0 (phys2/mass2 500.0 300.0 1.0 1.0 0.9)}
              :dguards []
              :aguards []
              :surfaces []
              :surfacelines []}
       
       views (ui/gen-from-desc
              layouts/hud
              (get-in uistate [:tempcanvas]))

       state {:world world
              :glstate glstate
              :uistate uistate
              :views views
              :level_file "level0.svg"
              :level_state "none"
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
    
    (animate
     state
     (fn [prestate frame time]
       (cond 
         
         (= (:level_state prestate) "none")
         (do
           (load-level! svgch (:level_file prestate))
           (assoc prestate :level_state "loading"))
         
         (= (:level_state prestate) "loading")
         (let [shapes (poll! svgch)]
           (if shapes
             (let [points (map :path (filter #(and (= (% :id) "Surfaces") (not (contains? % :color))) shapes ))
                   surfaces (phys2/surfaces-from-pointlist points)
                   lines (reduce (fn [result {t :t b :b}] (conj result t (math2/add-v2 t b))) [] surfaces)]

               (-> prestate
                   (assoc :glstate (webgl/loadshapes (:glstate prestate) shapes))
                   (assoc-in [:world :surfaces] surfaces)
                   (assoc-in [:world :surfacelines] lines)
                   (assoc :level_state "loaded")))
               prestate))

         (= (:level_state prestate) "loaded")
         (let [teximage (poll! imgch)
               keyevent (poll! keych)
               tchevent (poll! tchch)

               newviews (update-ui (:views prestate))
               
               newworld (update-world (:world prestate) (:keycodes prestate))
               newstate (-> (assoc prestate :world newworld)
                            (assoc :views newviews)
                            (update-translation keyevent))]
           
           (draw-world! newstate frame)
           ;;(draw-ui! newstate frame)

           newstate))))))

(main)
