(ns brawl.webgl
  (:require [cljs-webgl.context :as context]
            [cljs-webgl.shaders :as shaders]
            [cljs-webgl.texture :as texture]
            [cljs-webgl.constants.texture-unit :as texture-unit]
            [cljs-webgl.constants.draw-mode :as draw-mode]
            [cljs-webgl.constants.data-type :as data-type]
            [cljs-webgl.constants.buffer-object :as buffer-object]
            [cljs-webgl.constants.shader :as shader]
            [cljs-webgl.constants.texture-target :as texture-target]
            [cljs-webgl.buffers :as buffers]
            [cljs-webgl.typed-arrays :as ta]
            [cljs-webgl.constants.capability :as capability]
            [cljs-webgl.constants.blending-factor-dest :as blend]
            [brawl.shape :as shape]
            [brawl.floatbuffer :as floatbuf]))


(def vertex-source
  "attribute highp vec2 position;
   attribute highp vec4 color;
   varying highp vec2 positionv;
   varying highp vec4 colorv;
   uniform mat4 projection;
   void main ( )
   {
	gl_Position = projection * vec4(position,0.0,1.0);
	gl_PointSize = 8.0;
	colorv = color;
	positionv = position;
   }")


(def fragment-source
  "varying highp vec4 colorv;
   varying highp vec2 positionv;
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


(defn fuzz-path!
  "makes path fuzzy a little bit for cartoon effect"
  [vertexes]
  (vec (map (fn [[x y]] (list (+ x (+ -1.0 (rand 2.0))) (+ y (+ -1.0 (rand 2.0))))) vertexes)))


(defn gen-vertex-triangle
  "generates vertexes for points"
  [vertexes color]
  (let [r ( / (bit-shift-right (bit-and color 0xFF0000) 16) 255.0 )
        g ( / (bit-shift-right (bit-and color 0x00FF00) 8) 255.0 )
        b ( / (bit-and color 0x0000FF) 255.0 ) ]
      (map (fn [[x y]] [x y r g b 1.0]) vertexes)))


(defn gen-shapes-triangles!
  "generates triangles for shapes"
  [shapes]
  (->> shapes
       (filter #(contains? % :color))
       (map (fn [shape] (gen-vertex-triangle (shape/triangulate-c (fuzz-path! (:path shape))) (:color shape ))))
       (flatten)))


(defn init
  "creates webgl"
  []
  (let [context (context/get-context (.getElementById js/document "main"))

        shader (shaders/create-program
                context
                (shaders/create-shader context shader/vertex-shader vertex-source)
                (shaders/create-shader context shader/fragment-shader fragment-source))
        
        scene-buffer (buffers/create-buffer
                      context
                      (ta/float32 [500.0 500.0 1.0 1.0 1.0 1.0])
                      buffer-object/array-buffer
                      buffer-object/static-draw)
        
        actor-buffer (buffers/create-buffer
                      context
                      (ta/float32 [500.0 500.0 1.0 1.0 1.0 1.0])
                      buffer-object/array-buffer
                      buffer-object/dynamic-draw)

        mass-buffer (buffers/create-buffer
                     context
                     (ta/float32 [500.0 500.0 1.0 1.0 1.0 1.0])
                     buffer-object/array-buffer
                     buffer-object/dynamic-draw)

        line-buffer (buffers/create-buffer
                     context
                     (ta/float32 [500.0 500.0 1.0 1.0 1.0 1.0
                                  500.0 500.0 1.0 1.0 1.0 1.0])
                     buffer-object/array-buffer
                     buffer-object/static-draw)
        
        location-pos (shaders/get-attrib-location context shader "position")
        location-col (shaders/get-attrib-location context shader "color")]

    {:context context
     :shader shader
     :mass-buffer mass-buffer
     :line-buffer line-buffer
     :scene-buffer scene-buffer
     :actor-buffer actor-buffer
     :location-pos location-pos
     :location-col location-col}))


(defn load-shapes
  "uploads shapes to shape buffer"
  [state shapes]
  (let [{:keys [context scene-buffer line-buffer]} state
        vertexesA (gen-shapes-triangles! shapes)
        vertexesB (gen-shapes-triangles! shapes)
        vertexesC (gen-shapes-triangles! shapes)
        vertexes (concat vertexesA vertexesB vertexesC)
        vertex-counts [ (count vertexesA) (count vertexesB) (count vertexesC) ]
        vertex-starts [ 0 (vertex-counts 0) (+ (vertex-counts 0 ) ( vertex-counts 1 ) ) ]]
    
    (.bindBuffer context buffer-object/array-buffer scene-buffer)
    (.bufferData context
                 buffer-object/array-buffer
                 (ta/float32 vertexes)
                 buffer-object/static-draw)

    (-> state
        (assoc :vertexes vertexes)
        (assoc :vertex-counts vertex-counts)
        (assoc :vertex-starts vertex-starts))))


(defn clear!
  "clears context"
  [state]
  (let [{context :context} state]
    (buffers/clear-color-buffer context 0.0 0.0 0.0 1.0)))


(defn draw-shapes!
  [state projection variation]
  (let [{:keys [context shader scene-buffer location-pos location-col vertex-counts vertex-starts]} state]
    (.bindBuffer context buffer-object/array-buffer scene-buffer)
    (buffers/draw!
     context
     :count (int (/ (vertex-counts variation) 6))
     :first (int (/ (vertex-starts variation) 6))
     :shader shader
     :draw-mode draw-mode/triangles               
     :attributes [{:buffer scene-buffer
                   :location location-pos
                   :components-per-vertex 2
                   :type data-type/float
                   :offset 0
                   :stride 24}
                  {:buffer scene-buffer
                   :location location-col
                   :components-per-vertex 4
                   :type data-type/float
                   :offset 8
                   :stride 24}]
     :uniforms [{:name "projection"
                 :type :mat4
                 :values projection}])
    state))


(defn draw-triangles!
  [state projection floatbuffer]
  (let [{:keys [context shader location-pos location-col actor-buffer]} state]
    (.bindBuffer context buffer-object/array-buffer actor-buffer)
    (.bufferData context buffer-object/array-buffer (:data floatbuffer) buffer-object/dynamic-draw)
    (buffers/draw!
     context
     :count (/ (:index floatbuffer) 6)
     :shader shader
     :draw-mode draw-mode/triangles
     :attributes [{:buffer actor-buffer
                   :location location-pos
                   :components-per-vertex 2
                   :type data-type/float
                   :offset 0
                   :stride 24}
                  {:buffer actor-buffer
                   :location location-col
                   :components-per-vertex 4
                   :type data-type/float
                   :offset 8
                   :stride 24}]
     :uniforms [{:name "projection"
                 :type :mat4
                 :values projection}])
    state))


(defn draw-lines!
  [state projection floatbuffer ]
  (let [{:keys [context shader line-buffer location-pos location-col]} state]
    (.bindBuffer context buffer-object/array-buffer line-buffer)
    (.bufferData context buffer-object/array-buffer (:data floatbuffer) buffer-object/dynamic-draw)
    (buffers/draw!
     context
     :count (/ (:index floatbuffer) 6)
     :shader shader
     :draw-mode draw-mode/lines
     :attributes [{:buffer line-buffer
                   :location location-pos
                   :components-per-vertex 2
                   :type data-type/float
                   :offset 0
                   :stride 24}
                  {:buffer line-buffer
                   :location location-col
                   :components-per-vertex 4
                   :type data-type/float
                   :offset 8
                   :stride 24}]
     :uniforms [{:name "projection"
                 :type :mat4
                 :values projection}])
    state))


(defn draw-points!
  [state projection floatbuffer]
  (let [{:keys [context shader mass-buffer location-pos location-col]} state]
    (.bindBuffer context buffer-object/array-buffer mass-buffer)
    (.bufferData context buffer-object/array-buffer (:data floatbuffer) buffer-object/dynamic-draw)
    (buffers/draw!
     context
     :count (/ (:index floatbuffer) 6)
     :shader shader
     :draw-mode draw-mode/points               
     :attributes [{:buffer mass-buffer
                   :location location-pos
                   :components-per-vertex 2
                   :type data-type/float
                   :offset 0
                   :stride 24}
                  {:buffer mass-buffer
                   :location location-col
                   :components-per-vertex 4
                   :type data-type/float
                   :offset 8
                   :stride 24}]
     :uniforms [{:name "projection"
                 :type :mat4
                 :values projection}]
     :capabilities {capability/blend true}
     :blend-function [[blend/src-alpha blend/one-minus-src-alpha]])    
    state))
