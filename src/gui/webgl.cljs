(ns gui.webgl
  (:require [gui.bitmap :as bitmap]
            [gui.texmap :as texmap]
            [brawl.floatbuffer :as fb]
            [clojure.string :as str]
            [cljs-webgl.context :as context]
            [cljs-webgl.shaders :as shaders]
            [cljs-webgl.texture :as texture]
            [cljs-webgl.buffers :as buffers]
            [cljs-webgl.typed-arrays :as arrays]
            [cljs-webgl.constants.shader :as shader]
            [cljs-webgl.constants.buffer-object :as buffer-object]
            [cljs-webgl.constants.draw-mode :as draw-mode]
            [cljs-webgl.constants.data-type :as data-type]
            [cljs-webgl.constants.capability :as capability]
            [cljs-webgl.constants.blending-factor-dest :as blend]
            [cljs-webgl.constants.texture-unit :as texture-unit]))


(def ui-vertex-source
  "attribute highp vec4 position;
   attribute highp vec2 texcoord;
   varying highp vec2 texcoordv;
   uniform mat4 projection;
   void main ( )
   {
      gl_Position = projection * position;
      texcoordv = texcoord;
   }")


(def ui-fragment-source
  "varying highp vec2 texcoordv;
   uniform sampler2D texture_main;
   void main( )
   {
      gl_FragColor = texture2D( texture_main , texcoordv);
}")


(defn init []
  "initializes webgl module"
  (let [context (context/get-context
                 (.getElementById js/document "main")
                 {:premultiplied-alpha false :alpha false})

        tempcanvas (.createElement js/document "canvas")

        ui-shader (shaders/create-program
                   context
                   (shaders/create-shader context shader/vertex-shader ui-vertex-source)
                   (shaders/create-shader context shader/fragment-shader ui-fragment-source))

        ui-buffer (buffers/create-buffer
                   context
                   (arrays/float32 [0.0 0.0 0.0 0.0])
                   buffer-object/array-buffer
                   buffer-object/static-draw)

        ui-texmap (texmap/init 1024 1024 0 0 0 0)
        ui-texture (.createTexture context)
        
        ui-location-pos (shaders/get-attrib-location context ui-shader "position")
        ui-location-texcoord (shaders/get-attrib-location context ui-shader "texcoord")]

    (set! (. tempcanvas -width) 1000)
    (set! (. tempcanvas -height) 200)
    
    {:context context
     :tempcanvas tempcanvas
     :floatbuffer (fb/create!)
     :textures {}
     :ui-shader ui-shader
     :ui-buffer ui-buffer
     :ui-texmap ui-texmap
     :ui-texture ui-texture
     :ui-location-pos ui-location-pos
     :ui-location-texcoord ui-location-texcoord}))


(defn sizes-for-glyph [canvas text height]
  "returns glyph sizes"
  (let [context (.getContext canvas "2d" )
        itemhth (* height 1.2)]
    (set! (.-font context) (str height "px Ubuntu Bold"))
    (set! (.-fillStyle context) "#000000")
    (set! (.-textBaseline context) "middle")
    {:width (int (.-width (.measureText context text)))
     :height (int itemhth)}))


(defn int-to-rgba [color]
  (let [r (bit-and (bit-shift-right color 24) 0xFF)
        g (bit-and (bit-shift-right color 16) 0xFF)
        b (bit-and (bit-shift-right color 8) 0xFF)
        a (bit-and color 0xFF)]
    (str "rgba("r","g","b","(/ a 255)")")))


(defn bitmap-for-glyph [canvas width height texture]
  "returns glyph bitmap"
  (let [size (:size texture)
        forecol (int-to-rgba (:color texture))
        backcol (int-to-rgba (:background texture))
        context (.getContext canvas "2d")]
    (.clearRect context 0 0 (.-width canvas) (.-height canvas))
    (set! (.-fillStyle context) backcol)
    (.fillRect context 0 0 (.-width canvas) (.-height canvas))
    (set! (.-font context) (str "bolder " size "px Ubuntu Bold"))
    (set! (.-fillStyle context) forecol)
    (set! (.-textBaseline context) "middle")
    (let [itemwth (int (.-width (.measureText context (:text texture))))]
      (.fillText context (:text texture) (int (* (- width itemwth) 0.5)) (int (/ height 1.8)))
      {:data (.-data (.getImageData context 0 0 width height))
       :width width
       :height height})))


(defn tex-gen-for-ids [tempcanvas ui-texmap views]
  "generates textures for descriptor"
  (loop [remviews views
         tmap ui-texmap]
    (if (empty? remviews)
      tmap
      (let [{:keys [id texture w h] :as view} (first remviews)
            newtmap (if (texmap/hasbmp? tmap texture)
                      tmap
                      (cond

                        (= (:type texture) "Debug")
                        ;; show full texture in quad
                        (assoc-in tmap [:contents texture] [0 0 1 1])

                        (= (:type texture) "Image")
                        ;; show image in quad
                        (tmap)

                        (= (:type texture) "Color")
                        ;; show color in quad
                        (let [r (bit-and (bit-shift-right (:color texture) 24) 0xFF)
                              g (bit-and (bit-shift-right (:color texture) 16) 0xFF)
                              b (bit-and (bit-shift-right (:color texture) 8) 0xFF)
                              a (bit-and (:color texture) 0xFF)]
                          (texmap/setbmp tmap (bitmap/init 10 10 r g b a) texture 1))

                        (= (:type texture) "Label")
                        ;; show glyph
                        (let [bmp (bitmap-for-glyph tempcanvas w h texture)]
                          (texmap/setbmp tmap bmp texture 0))

                        :default
                        ;; return empty texmap if unknown
                        tmap))]
        
        (recur (rest remviews) newtmap)))))


(defn reset [{:keys [ui-texmap] :as state}]
  (assoc state :ui-texmap (texmap/reset ui-texmap)))


(defn clear! [{:keys [context ui-texmap] :as state}]
  (buffers/clear-color-buffer
   context
   0.1
   0.1
   0.4
   1.0))


(defn draw! [{:keys [context
                     tempcanvas
                     floatbuffer
                     textures
                     ui-shader
                     ui-buffer
                     ui-location-pos
                     ui-location-texcoord
                     ui-texmap
                     ui-texture] :as state }
             projection
             views]
  "draw views defined by x y width height and texure requirements." 
  ;(cljs.pprint/pprint views)

  (let [;; generate textures for new views
        newtexmap (tex-gen-for-ids tempcanvas ui-texmap views)
        ;; generate vertex data from views
        resfb (fb/empty! floatbuffer)
        newfb (reduce (fn [oldbuf {:keys [id x y w h texture] :as view}]
                        (if-not texture
                          oldbuf
                          (let [[tlx tly brx bry] (texmap/getbmp newtexmap texture)]
                            (fb/append!
                             oldbuf
                             (array x y tlx tly
                                    (+ x w) y brx tly
                                    x (+ y h) tlx bry
                                    
                                    (+ x w) y brx tly
                                    (+ x w) (+ y h) brx bry
                                    x (+ y h) tlx bry))))) resfb views)]
    
    ;(cljs.pprint/pprint vertexes)>
    
    ;; upload texture map if changed
    (when (newtexmap :changed)
      (texture/upload-texture
      context
       ui-texture
       (:data (:texbmp newtexmap))
       1024
       1024))

    ;; upload buffer 
    (buffers/upload-buffer
     context
     ui-buffer
     (:data newfb)
     buffer-object/array-buffer
     buffer-object/dynamic-draw)

    ;; draw vertexes
    (buffers/draw!
     context
     :count (/ (:index newfb) 4)
     :first 0
     :shader ui-shader
     :draw-mode draw-mode/triangles
     :attributes [{:buffer ui-buffer
                   :location ui-location-pos
                   :components-per-vertex 2
                   :type data-type/float
                   :offset 0
                   :stride 16}
                  {:buffer ui-buffer
                   :location ui-location-texcoord
                   :components-per-vertex 2
                   :type data-type/float
                   :offset 8
                   :stride 16}]
     :uniforms [{:name "projection"
                 :type :mat4
                 :values projection}
                {:name "texture_main"
                 :type :sampler-2d
                 :values 0}]
     :capabilities {capability/blend true}
     :blend-function [[blend/src-alpha blend/one-minus-src-alpha]]
     :textures [{:texture ui-texture 
                :name "texture_main"
                :texture-unit texture-unit/texture0}])

    (-> state
        (assoc :ui-texmap (assoc newtexmap :changed false))
        (assoc :floatbuffer newfb)
        )))
