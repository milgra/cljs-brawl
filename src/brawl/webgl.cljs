(ns brawl.webgl
  (:require [cljs-webgl.context :as context]
            [cljs-webgl.shaders :as shaders]
            [cljs-webgl.constants.draw-mode :as draw-mode]
            [cljs-webgl.constants.data-type :as data-type]
            [cljs-webgl.constants.buffer-object :as buffer-object]
            [cljs-webgl.constants.shader :as shader]
            [cljs-webgl.buffers :as buffers]
            [cljs-webgl.typed-arrays :as ta]
            [brawl.shape :as shape]))
  
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


(defn fuzz-path! [ vertexes ]
  (vec (map (fn [[x y]] (list (+ x (+ -1.0 (rand 2.0))) (+ y (+ -1.0 (rand 2.0))))) vertexes)))


(defn gen-vertex-triangle [ vertexes color ]
  (let [r ( / (bit-shift-right (bit-and color 0xFF0000) 16) 255.0 )
        g ( / (bit-shift-right (bit-and color 0x00FF00) 8) 255.0 )
        b ( / (bit-and color 0x0000FF) 255.0 ) ]
      (map (fn [[x y]] [ x y 0.0 1.0 r g b 1.0] ) vertexes )))


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
                      (ta/float32 [500.0 500.0 0.0 1.0 1.0 1.0 1.0 1.0])
                      buffer-object/array-buffer
                      buffer-object/static-draw)
        
        actor_buffer (buffers/create-buffer
                      context
                      (ta/float32 [500.0 500.0 0.0 1.0 1.0 1.0 1.0 1.0])
                      buffer-object/array-buffer
                      buffer-object/dynamic-draw)
     
        mass_buffer (buffers/create-buffer
                     context
                     (ta/float32 [500.0 500.0 0.0 1.0 1.0 1.0 1.0 1.0])
                     buffer-object/array-buffer
                     buffer-object/dynamic-draw)

        line_buffer (buffers/create-buffer
                     context
                     (ta/float32 [500.0 500.0 0.0 1.0 1.0 1.0 1.0 1.0
                                  500.0 500.0 0.0 1.0 1.0 1.0 1.0 1.0])
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

(partition 4 2 [1 2 3 4 5 6 7])


(defn loadshapes [{:keys [context scene_buffer line_buffer] :as state} shapes]
  (let [vertexesA (gen-shapes-triangle! shapes)
        vertexesB (gen-shapes-triangle! shapes)
        vertexesC (gen-shapes-triangle! shapes)
        vertexes (concat vertexesA vertexesB vertexesC)
        vertexcounts [ (count vertexesA) (count vertexesB) (count vertexesC) ]
        vertexstarts [ 0 (vertexcounts 0) (+ (vertexcounts 0 ) ( vertexcounts 1 ) ) ]
        
        surfaces (filter #(and (= (% :id) "Surfaces") (not (contains? % :color))) shapes)
        lines (flatten
               (map
                (fn [shape]
                  ( println "e" (partition 2 1 (:path shape)))
                    (gen-vertex-triangle (partition 2 (flatten (partition 2 1 (:path shape)))) 0xFFFFFF))
                surfaces))]

    (println "lines" lines)
    
    (.bindBuffer context buffer-object/array-buffer scene_buffer)
    (.bufferData context
                 buffer-object/array-buffer
                 (ta/float32 vertexes)
                 buffer-object/static-draw)

    (.bindBuffer context buffer-object/array-buffer line_buffer)
    (.bufferData context
                 buffer-object/array-buffer
                 (ta/float32 lines)
                 buffer-object/static-draw)
    (-> state
        (assoc :vertexes vertexes)
        (assoc :vertexcounts vertexcounts)
        (assoc :vertexstarts vertexstarts)
        (assoc :lines lines))))


(defn drawshapes! [{:keys [context shader scene_buffer actor_buffer location_pos location_col vertexes vertexcounts vertexstarts ] :as state} projection [tx ty] variation]
             
  (buffers/clear-color-buffer context 0.1 0.0 0 1)

  ;; draw shapes buffer
  
  (.bindBuffer context buffer-object/array-buffer scene_buffer)
  
  (buffers/draw!
   context
   :count (int (/ (vertexcounts variation) 8))
   :first (int (/ (vertexstarts variation) 8))
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
                        :values projection}])
  ;; return state
  state)


(defn drawlines! [ {:keys [context shader line_buffer location_pos location_col lines] :as state} projection ]

  ;; draw line buffer
    
  (.bindBuffer context buffer-object/array-buffer line_buffer)
  
  (buffers/draw!
   context
   :count (/ (count lines) 8)
   :shader shader
   :draw-mode draw-mode/lines
   :attributes [{:buffer line_buffer
                 :location location_pos
                 :components-per-vertex 4
                 :type data-type/float
                 :offset 0
                 :stride 32}
                {:buffer line_buffer
                 :location location_col
                 :components-per-vertex 4
                 :type data-type/float
                 :offset 16
                 :stride 32}]
   :uniforms [{:name "projection"
               :type :mat4
               :values projection}]))


(defn drawmasses! [{:keys [context shader mass_buffer location_pos location_col] :as state} projection masses]
     
  ;; draw mass buffer
  
  (.bindBuffer context buffer-object/array-buffer mass_buffer)
  
  ;; load in new vertexdata
  
  (.bufferData context
               buffer-object/array-buffer
               (ta/float32
                (vec
                 (flatten
                  (map
                   (fn voxelize [{[tx ty] :trans}]
                     [tx ty 0.0 1.0 1.0 1.0 1.0 1.0]) masses))))
               buffer-object/dynamic-draw)
  
  (buffers/draw!
   context
   :count (count masses)
   :shader shader
            :draw-mode draw-mode/points               
            :attributes [{:buffer mass_buffer
                          :location location_pos
                          :components-per-vertex 4
                          :type data-type/float
                          :offset 0
                          :stride 32}                        {:buffer mass_buffer
                          :location location_col
                          :components-per-vertex 4
                          :type data-type/float
                          :offset 16
                          :stride 32}]
            :uniforms [{:name "projection"
                        :type :mat4
                        :values projection}])
  ;; return state
  state)
