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
            [brawl.shape :as shape]))
  
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


(defn fuzz-path! [ vertexes ]
  (vec (map (fn [[x y]] (list (+ x (+ -1.0 (rand 2.0))) (+ y (+ -1.0 (rand 2.0))))) vertexes)))


(defn gen-vertex-triangle [ vertexes color ]
  (let [r ( / (bit-shift-right (bit-and color 0xFF0000) 16) 255.0 )
        g ( / (bit-shift-right (bit-and color 0x00FF00) 8) 255.0 )
        b ( / (bit-and color 0x0000FF) 255.0 ) ]
      (map (fn [[x y]] [ x y r g b 1.0] ) vertexes )))


(defn gen-shapes-triangle! [shapes]
  (remove nil? (flatten (map
                         (fn [shape]
                           (if (contains? shape :color)
                             ( gen-vertex-triangle
                              (shape/triangulate_c
                               (fuzz-path! (:path shape)))
                              (:color shape ))))
                         shapes))))


(defn init []
  (let [context (context/get-context (.getElementById js/document "main"))

        shader (shaders/create-program
                context
                (shaders/create-shader context shader/vertex-shader vertex-source)
                (shaders/create-shader context shader/fragment-shader fragment-source))
        
        scene_buffer (buffers/create-buffer
                      context
                      (ta/float32 [500.0 500.0 1.0 1.0 1.0 1.0])
                      buffer-object/array-buffer
                      buffer-object/static-draw)
        
        actor_buffer (buffers/create-buffer
                      context
                      (ta/float32 [500.0 500.0 1.0 1.0 1.0 1.0])
                      buffer-object/array-buffer
                      buffer-object/dynamic-draw)

        mass_buffer (buffers/create-buffer
                     context
                     (ta/float32 [500.0 500.0 1.0 1.0 1.0 1.0])
                     buffer-object/array-buffer
                     buffer-object/dynamic-draw)

        line_buffer (buffers/create-buffer
                     context
                     (ta/float32 [500.0 500.0 1.0 1.0 1.0 1.0
                                  500.0 500.0 1.0 1.0 1.0 1.0])
                     buffer-object/array-buffer
                     buffer-object/static-draw)
        
        location_pos (shaders/get-attrib-location context shader "position")
        location_col (shaders/get-attrib-location context shader "color")]

    {:context context
     :shader shader
     :scene_buffer scene_buffer
     :actor_buffer actor_buffer
     :mass_buffer mass_buffer
     :line_buffer line_buffer
     :location_pos location_pos
     :location_col location_col}))


(defn loadshapes [{:keys [context scene_buffer line_buffer] :as state} shapes]
  (let [vertexesA (gen-shapes-triangle! shapes)
        vertexesB (gen-shapes-triangle! shapes)
        vertexesC (gen-shapes-triangle! shapes)
        vertexes (concat vertexesA vertexesB vertexesC)
        vertexcounts [ (count vertexesA) (count vertexesB) (count vertexesC) ]
        vertexstarts [ 0 (vertexcounts 0) (+ (vertexcounts 0 ) ( vertexcounts 1 ) ) ]]

    (.bindBuffer context buffer-object/array-buffer scene_buffer)
    (.bufferData context
                 buffer-object/array-buffer
                 (ta/float32 vertexes)
                 buffer-object/static-draw)

    (-> state
        (assoc :vertexes vertexes)
        (assoc :vertexcounts vertexcounts)
        (assoc :vertexstarts vertexstarts))))


(defn clear! [ {context :context} ]
  (buffers/clear-color-buffer context 0.0 0.0 0.0 1.0))


(defn drawshapes! [{:keys [context shader scene_buffer actor_buffer location_pos location_col vertexes vertexcounts vertexstarts ] :as state} projection [tx ty] variation]

  (.bindBuffer context buffer-object/array-buffer scene_buffer)

  (buffers/draw!
   context
   :count (int (/ (vertexcounts variation) 6))
   :first (int (/ (vertexstarts variation) 6))
   :shader shader
   :draw-mode draw-mode/triangles               
   :attributes [{:buffer scene_buffer
                 :location location_pos
                 :components-per-vertex 2
                 :type data-type/float
                 :offset 0
                 :stride 24}
                {:buffer scene_buffer
                 :location location_col
                 :components-per-vertex 4
                 :type data-type/float
                 :offset 8
                 :stride 24}]
   :uniforms [{:name "projection"
               :type :mat4
               :values projection}])
  
  state)


(defn drawtriangles! [ {:keys [context shader location_pos location_col actor_buffer] :as state } projection points ]
    
  (.bindBuffer context buffer-object/array-buffer actor_buffer)
  (.bufferData context buffer-object/array-buffer (ta/float32 (flatten points)) buffer-object/dynamic-draw)
  
  (buffers/draw!
   context
   :count (count points)
   :shader shader
   :draw-mode draw-mode/triangles
   :attributes [{:buffer actor_buffer
                 :location location_pos
                 :components-per-vertex 2
                 :type data-type/float
                 :offset 0
                 :stride 24}
                {:buffer actor_buffer
                 :location location_col
                 :components-per-vertex 4
                 :type data-type/float
                 :offset 8
                 :stride 24}]
   :uniforms [{:name "projection"
               :type :mat4
               :values projection}])

  state)


(defn drawlines! [ {:keys [context shader line_buffer location_pos location_col] :as state} projection lines ]
  
  (.bindBuffer context buffer-object/array-buffer line_buffer)
    
  (.bufferData context
               buffer-object/array-buffer
               (ta/float32
                (vec
                 (flatten
                  (map
                   (fn voxelize [[tx ty]]
                     [tx ty 1.0 1.0 1.0 1.0]) lines))))
               buffer-object/dynamic-draw)
  
  (buffers/draw!
   context
   :count (count lines)
   :shader shader
   :draw-mode draw-mode/lines
   :attributes [{:buffer line_buffer
                 :location location_pos
                 :components-per-vertex 2
                 :type data-type/float
                 :offset 0
                 :stride 24}
                {:buffer line_buffer
                 :location location_col
                 :components-per-vertex 4
                 :type data-type/float
                 :offset 8
                 :stride 24}]
   :uniforms [{:name "projection"
               :type :mat4
               :values projection}])

  state)


(defn drawpoints! [{:keys [context shader mass_buffer location_pos location_col] :as state} projection points]

  (.bindBuffer context buffer-object/array-buffer mass_buffer)
  
  (.bufferData context
               buffer-object/array-buffer
               (ta/float32
                (vec
                 (flatten
                  (map
                   (fn voxelize [[tx ty]]
                     [tx ty 1.0 1.0 1.0 1.0]) points))))
               buffer-object/dynamic-draw)
  
  (buffers/draw!
   context
   :count (count points)
   :shader shader
            :draw-mode draw-mode/points               
            :attributes [{:buffer mass_buffer
                          :location location_pos
                          :components-per-vertex 2
                          :type data-type/float
                          :offset 0
                          :stride 24}                        {:buffer mass_buffer
                          :location location_col
                          :components-per-vertex 4
                          :type data-type/float
                          :offset 8
                          :stride 24}]
            :uniforms [{:name "projection"
                        :type :mat4
                        :values projection}])

  state)
