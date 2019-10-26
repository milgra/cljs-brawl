(ns ^:figwheel-hooks brawl.core
  (:require [goog.dom :as gdom]
            [goog.events :as events]
            [tubax.core :refer [xml->clj]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! chan put! take! poll!]]
            [cljs.core.async :refer-macros [go]]
            [cljs-webgl.context :as context]
            [cljs-webgl.shaders :as shaders]
            [cljs-webgl.constants.draw-mode :as draw-mode]
            [cljs-webgl.constants.data-type :as data-type]
            [cljs-webgl.constants.buffer-object :as buffer-object]
            [cljs-webgl.constants.shader :as shader]
            [cljs-webgl.buffers :as buffers]
            [cljs-webgl.typed-arrays :as ta]
            [brawl.surface :as surface]
            [brawl.svg :as svg]
            [brawl.physics :as physics]
            [brawl.mass :as mass]
            [brawl.math4 :as math4]
            [brawl.shape :as shape])
  (:import [goog.events EventType]))
  
;;(println "AA This text is printed from src/brawl/core.cljs. Go ahead and edit it and see reloading in action.")


(def vertex-source
  "attribute highp vec4 position;
   attribute highp vec4 color;
   varying highp vec4 colorv;
   varying highp vec4 positionv;
   uniform mat4 projection;
   void main ( )
   {
	gl_Position = projection * position;
	gl_PointSize = 8.0;
	colorv = color;
	positionv = position;
   }")


(def fragment-source
  "varying highp vec4 colorv;
   varying highp vec4 positionv;
   void main( )
   {
	gl_FragColor = colorv;
   	if ( colorv.w == 1.0 && colorv.x < 0.8 )
   	{
       	 highp float ver = sin(positionv.y)+1.0;
       	 highp float hor = sin(positionv.x)+1.0;
       	 highp float dia = sin((positionv.x+positionv.y)/1.5)+1.0;
       	 ver = floor(ver * 2.0)/4.0;
       	 hor = floor(hor * 2.0)/4.0;
       	 dia = floor(dia * 2.0)/4.0;
       	 if ( colorv.x >= colorv.y && colorv.x >= colorv.z ) gl_FragColor.x -= ver*0.05;
       	 if ( colorv.y >= colorv.x && colorv.y >= colorv.z ) gl_FragColor.y -= hor*0.05;
	 if ( colorv.z >= colorv.x && colorv.z >= colorv.y ) gl_FragColor.z -= dia*0.2;
   	}
   }")


(defn gen-vertex-triangle [ vertexes color ]
  (let [r ( / (bit-shift-right (bit-and color 0xFF0000) 16) 255.0 )
        g ( / (bit-shift-right (bit-and color 0x00FF00) 8) 255.0 )
        b ( / (bit-and color 0x0000FF) 255.0 ) ]
    (map #( concat % [0.0 1.0 r g b 1.0] ) vertexes )))


(defn load-level! [channel name]
  (go
    (let [response (<! (http/get "level1.svg"
                                 ;; parameters
                                 {:with-credentials? false}))
          xmlstr (xml->clj (:body response) {:strict false})
          level (svg/psvg xmlstr "")
          shapes level ;; (filter #(not= (% :id) "Surfaces") level)
          surfacepoints (filter #(= (% :id) "Surfaces") level)
          surfaces (surface/generate-from-pointlist surfacepoints)
          vertexes (flatten
                     ( map
                      (fn [shape]
                        (if (contains? shape :color)
                          ( gen-vertex-triangle (shape/triangulate_c (:path shape) ) (:color shape ))))
                      shapes))]
      ;;(for [x (range 0 10)] (p/mass2 (rand 1000) (rand 1000)))]
      (println "map loaded")
      (put! channel vertexes))))


(defn animate [state draw-fn]
  (letfn [(loop [oldstate frame]
            (fn [time]
              (let [newstate (draw-fn oldstate frame time)]
              (.requestAnimationFrame js/window (loop newstate (inc frame))))
              ))]
    ((loop state 0) 0 )))


(defn main []
  (println "main") 
  (let
      [context (context/get-context (.getElementById js/document "main"))

       shader (shaders/create-program
               context
               (shaders/create-shader context shader/vertex-shader vertex-source)
               (shaders/create-shader context shader/fragment-shader fragment-source))

       scene_buffer (buffers/create-buffer
                     context
                     (ta/float32 [ 500.0 500.0 0.0 1.0 1.0 1.0 1.0 1.0 ])
                     buffer-object/array-buffer
                     buffer-object/static-draw)

       actor_buffer (buffers/create-buffer
                     context
                     (ta/float32 [ 500.0 500.0 0.0 1.0 1.0 1.0 1.0 1.0 ])
                     buffer-object/array-buffer
                     buffer-object/dynamic-draw)
       
       location_pos (shaders/get-attrib-location context shader "position")
       location_col (shaders/get-attrib-location context shader "color")

       initstate {:level_file "level0.svg"
                  :level_state "none"
                  :keypresses {}
                  :trans [500.0 500.0]
                  :speed [0.0 0.0]}

       filechannel (chan)
       keychannel (chan)]

    ;; key listeners

    (events/listen
     js/document
     EventType.KEYDOWN
     (fn [event] (put! keychannel {:code (.-keyCode event) :value true})))
    
    (events/listen
     js/document
     EventType.KEYUP
     (fn [event] (put! keychannel {:code (.-keyCode event) :value false})))

    ;; runloop
    
    (animate
     initstate
     (fn [state frame time]
       (cond 
         
         (= (:level_state state) "none")
         (do
           (load-level! filechannel (:level_file state))
           (assoc state :level_state "loading")
           )
         
         (= (:level_state state) "loading")
         (let [vertexes (poll! filechannel)]
           (if vertexes
             (do
               (println "vertexes arrived")
               (.bindBuffer context buffer-object/array-buffer scene_buffer)
               (.bufferData context
                            buffer-object/array-buffer
                            (ta/float32 vertexes)
                            buffer-object/static-draw)
               (-> state
                   (assoc :level_state "loaded")
                   (assoc :vertexes vertexes)))
             state))

         (= (:level_state state) "loaded")
         (let [[tx ty] (:trans state)
               [sx sy] (:speed state)
               ratio (/ (min (max (Math/abs sx) (Math/abs sy)) 40.0) 40.0)
               projection (math4/proj_ortho
                           (- tx (+ 150.0 (* ratio 50.0)))
                           (+ tx (+ 150.0 (* ratio 50.0)))
                           (+ ty (+ 150.0 (* ratio 50.0)))
                           (- ty (+ 150.0 (* ratio 50.0)))
                           -1.0 1.0)

               keyevent (poll! keychannel)]
           
           (buffers/clear-color-buffer context 0.1 0.0 0 1)
           
           ;; (actors/update actor controlstate)
           
           ;; draw scene buffer
           
           (.bindBuffer context buffer-object/array-buffer scene_buffer)
           
           (buffers/draw!
            context
            :count (/ (count (:vertexes state)) 8)
            :shader shader
            :draw-mode draw-mode/triangles               
            :attributes [{:buffer scene_buffer
                          :location location_pos
                          :components-per-vertex 4
                          :type data-type/float
                          :offset 0
                          :stride 32}
                         {:buffer scene_buffer
                          :location location_col
                          :components-per-vertex 4
                          :type data-type/float
                          :offset 16
                          :stride 32}]
            :uniforms [{:name "projection"
                        :type :mat4
                        :values projection}])
           
           ;; draw actor buffer
           
           (.bindBuffer context buffer-object/array-buffer actor_buffer)
           
           ;; load in new vertexdata
           
           (.bufferData context
                        buffer-object/array-buffer
                        (ta/float32 [tx ty 0.0 1.0 1.0 1.0 1.0 1.0])
                        buffer-object/dynamic-draw)
           
           (buffers/draw!
            context
            :count 1
            :shader shader
            :draw-mode draw-mode/points               
            :attributes [{:buffer actor_buffer
                          :location location_pos
                          :components-per-vertex 4
                          :type data-type/float
                          :offset 0
                          :stride 32}
                         {:buffer actor_buffer
                          :location location_col
                          :components-per-vertex 4
                          :type data-type/float
                          :offset 16
                          :stride 32}]
            :uniforms [{:name "projection"
                        :type :mat4
                        :values projection}]
            )

         
           ;; handle keypresses, modify main point trans

           (let [keycodes (if keyevent
                              (assoc (:keypresses state) (:code keyevent) (:value keyevent))
                              (:keypresses state))

                 nsx (cond
                       (keycodes 37) (- sx 1.0)
                       (keycodes 39) (+ sx 1.0)
                       "default" (* sx 0.9))

                 nsy (cond
                       (keycodes 38) (- sy 1.0)
                       (keycodes 40) (+ sy 1.0)
                       "default" (* sy 0.9))

                 ntx (+ tx sx)
                 nty (+ ty sy)]

             ;; return with updated state
             
             (-> state
                 (assoc :keypresses keycodes)
                 (assoc :speed [nsx nsy])
                 (assoc :trans [ntx nty]))
             )
           )
         )
       )
     )
    )
  )


(main)

;; template functions

(def app-state (atom {}))

(defn multiply [a b] (* a b))

(defn get-app-element []
  (gdom/getElement "app"))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! app-state update-in [:__figwheel_counter] inc)
  (println "app-state" app-state)
)
