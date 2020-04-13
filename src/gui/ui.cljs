(ns gui.ui
  (:require [gui.texmap :as texmap]
            [gui.bitmap :as bitmap]
            [gui.webgl :as webgl]
            [clojure.string :as str]))


(defn gen-view [id name class width height color]
  "generate basic view with color"
  {:class class
   :name name
   :id id
   :x 0
   :y 0
   ;
   :hor "0" ; align to center by default
   :ver "0" ; align to center by default
   ; if its between 0 and 1 its percentage, if its over its fixed
   :width width
   :height height
   :color (if (= class "Debug") ; Debug or Color 0xFFFFFFFF or texture name
            "Debug"
            (str "Color 0x" color))
   :subviews []})

  
(defn gen-id [n]
  "generates fixed length alfanumeric hash"
   (let [chars (map char (concat (range 48 57) (range 65 90) (range 97 122)))
         id (take n (repeatedly #(rand-nth chars)))]
     (reduce str id)))


(defn add-subview [{subviews :subviews :as view} subview]
  "inserts subview's id to views subviews property"
  (assoc view :subviews (conj subviews (subview :id))))

; generator

(defn gen-elements [viewmap line]
  (let [words (str/split line #"\s+")
        views (reduce (fn [res word]
                        (let [id (when (> (count word) 1)
                                   (cond
                                     (str/starts-with? word "|") (subs word 1)
                                     (str/ends-with? word "|") (subs word 0 (dec (count word)))
                                     :else word))]
                          (if id
                            (let [kw (keyword id)
                                  view (assoc (gen-view kw id "none" 100 100 "00000000") :text id)]
                              (assoc res kw view))
                            res)))
                      {}
                      words)]
    (into viewmap views)))


(defn gen-label [tempcanvas text size]
  (let [{lw :width lh :height} (webgl/sizes-for-glyph tempcanvas text size)
        hash (keyword (gen-id 8))
        view (gen-view hash text "Label" lw lh "00000000")]
  (-> view
      (assoc :color (str "Glyph " size "%" text))
      (assoc :text text)
      (assoc :hor "0")
      (assoc :ver "0"))
  ))


(defn gen-subviews
  "generate subview if needed"
  [viewmap {:keys[class text] :as view} tempcanvas]
  (cond
    (= class "Button")
    (let [subview (gen-label tempcanvas text 40)
          newview (add-subview view subview)]
      (-> viewmap
          (assoc (:id subview) subview)
          (assoc (:id newview) newview)))
    :else
    viewmap))


(defn gen-details [viewmap line tempcanvas]
  (let [words (str/split line #"\s+")
        id (first words)
        view ((keyword id) viewmap)
        newview (reduce (fn [res word]
                          (let [[key value] (str/split word #":")]
                            (if (= key "Color")
                              (assoc res :color (str "Color 0x" value)) ; set color texture
                              (assoc res (keyword (str/lower-case key)) value))))
                        view
                        (rest words))
        base (:baseview viewmap)]
    (-> viewmap
        (assoc (keyword id) newview) ; store new view
        (gen-subviews newview tempcanvas) ; generate subviews and update new view
        (assoc :baseview (add-subview base newview))))) ; add view as base view subview


(defn gen-base-views [lines tempcanvas]
  "generate basic views from line descriptions"
  (let [baseview (gen-view :base "base" "base" 100 100 "00000000")
        result (reduce
                (fn [viewmap line]
                  (cond
                    (str/starts-with? line "|") (gen-elements viewmap line) ; schema line with element names
                    (< 0 (count line)) (gen-details viewmap line tempcanvas) ; detail line with element details
                    :else viewmap)) ; unchanged viewmap
                {:baseview baseview}
                lines)]
    ; (println "result" result)
    result))


(defn gen-from-desc [desc tempcanvas]
  "generate view structure from description"  
  (let [lines (str/split-lines desc)]
    (-> (gen-base-views lines tempcanvas)
        ;(replace-alignment-names)
        )))


(defn add-align [view top bottom left right hor ver]
  "add alignment properties to view"
  (-> view
      (assoc :top top)
      (assoc :bottom bottom)
      (assoc :left left)
      (assoc :right right)
      (assoc :hor hor)
      (assoc :ver ver)))


(defn get-key-for-name [viewmap name]
  "extract view id from id - view map based on view name"
  (cond
    (= name nil) nil
    (= name "0") "0"
    :else (when name
            (let [pick (first (filter #(= ((val %) :name) name) (seq viewmap)))]
              (when pick (key pick))))))


(defn replace-alignment-names [viewmap]
  "replaces view names with view id's in viewmap's alignment properties"
  (reduce
   (fn [oldmap item]
     (let [{:keys [top bottom left right ver hor]} (val item)]
       (assoc oldmap (key item)
              (-> (val item)
                  (assoc :top (get-key-for-name oldmap top))
                  (assoc :bottom (get-key-for-name oldmap bottom))
                  (assoc :left (get-key-for-name oldmap left))
                  (assoc :right (get-key-for-name oldmap right))
                  (assoc :hor (get-key-for-name oldmap hor))
                  (assoc :ver (get-key-for-name oldmap ver))))))
   viewmap
   viewmap))


(defn align-view [viewmap id cx cy width height]
  "aligns view"
  (let [view (get viewmap id)
        {:keys [x y top bottom left right ver hor class text] w :width h :height } view
        topview (get viewmap top)
        bottomview (get viewmap bottom)
        leftview (get viewmap left)
        rightview (get viewmap right)
        horview (get viewmap hor)
        verview (get viewmap ver)
        result
        (-> view
            (assoc :x (cond
                        ;; align to view on the left or to screen edge
                        (not= left nil)
                        (if (= left "0")
                          0
                          (+ cx (:x leftview) (:width leftview)))
                        ;; align to view on the right or to screen edge
                        (not= right nil)
                        (if (= right "0")
                          (- width w)
                          (- (:x rightview) w))
                        ;; align to horizontal center or between left align and right align view
                        (not= hor nil)
                        (if (= hor "0")
                          (+ cx (- (/ width 2) (/ w 2)))
                          (- (- (:x leftview) (/ (- (:x rightview)(+ (:x leftview)(:width leftview))) 2) ) (/ w 2)))
                        ;; or leave x position as is
                        :default
                        x))
            (assoc :y (cond
                        ;; align to view on the top or to screen edge
                        (not= top nil)
                        (if (= top "0")
                          0
                          (+ (:y topview)(:height topview)))
                        ;; align to view on the bottom or to screen edge
                        (not= bottom nil)
                        (if (= bottom "0")
                          (- height h)
                          (- (:y bottomview) h))
                        ;; align to vertical center or between bottom and top align view
                        (not= ver nil)
                        (if (= ver "0")
                          (+ cy (- (/ height 2) (/ h 2)))
                          (- (- (:y bottomview) (/ (- (:y bottomview)(+ (:y topview)(:height topview))) 2 )) (/ h 2)))
                        :default
                        y)))]
    ;(println "a:" class text x y width height
    ;         "to" cx cy width height
    ;         "final" (result :x) (result :y) (result :width) (result :height))
    result))


(defn align [viewmap coll cx cy width height]
  "iterate through all views and align them based on their alignment switches"
  (reduce (fn [oldmap id]
            (let [view (get oldmap id)
                  {:keys [x y top bottom left right ver hor id class text] w :width h :height} view
                  ;; filter nil and 0 switches, 0 means to full parent view
                  toalign (filter #(and (not= % nil) (not= % "0")) [top bottom left right ver hor])
                  ;; first align relative views
                  newmap (align oldmap toalign (+ cx x) (+ cy y) width height)
                  ;; align self
                  newview (align-view newmap id cx cy width height)
                  ;; align subviews
                  newnewmap (align newmap (:subviews newview) (:x newview) (:y newview) (:width newview) (:height newview))]
              (assoc newnewmap id newview)))
          viewmap
          coll))


(defn collect-visible-ids [viewmap coll path]
  "collects ids of views that are currently visible"
  (println "path:" path "coll:" coll)
  (reduce
   (fn [res id]
     (let [view (viewmap id)]
       (concat res (collect-visible-ids viewmap (:subviews view) (str path ":" (:class view) )))))
   coll
   coll))


(defn collect-pressed-views [viewmap event]
  "collects view under touch point"
  (reduce
   (fn [result view]
     (let [{:keys [id x y] w :width h :height} view
           px (:x event)
           py (:y event)]
       (if (and (and (> px x) (< px (+ x w))) (and (> py y) (< py (+ y h))))
         (conj result id)
         result)))
   []
   (vals viewmap)))
