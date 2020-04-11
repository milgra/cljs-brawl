(ns gui.ui
  (:require [gui.texmap :as texmap]
            [gui.bitmap :as bitmap]
            [gui.webgl :as webgl]
            [clojure.string :as str]))


(defn gen-id [n]
  "generates fixed length alfanumeric hash"
   (let [chars (map char (concat (range 48 57) (range 65 90) (range 97 122)))
         id (take n (repeatedly #(rand-nth chars)))]
     (reduce str id)))


(defn gen-view [id name class width height color]
  "generate basic view with color"
  {:class class
   :name name
   :id id
   :x 0
   :y 0
   ;; if its between 0 and 1 its percentage, if its over its fixed
   :width width
   :height height
   :color (if (= class "Debug")
         "Debug"
         (str "Color 0x" color))
   :subviews []})


(defn gen-label [tempcanvas text size]
  (let [{lw :widthidth lh :height} (webgl/sizes-for-glyph tempcanvas text size)
        hash (keyword (gen-id 8))
        view (gen-view hash text "Label" lw lh "00000000")]
  (-> view
      (assoc :color (str "Glyph " size "%" text))
      (assoc :text text)
      (assoc :ha "0")
      (assoc :va "0"))
  ))


(defn add-align [view ta ba la ra ha va]
  "add alignment properties to view"
  (-> view
      (assoc :ta ta)
      (assoc :ba ba)
      (assoc :la la)
      (assoc :ra ra)
      (assoc :ha ha)
      (assoc :va va)))


(defn parse-desc [desc]
  "parse view description and create map"
  "D CLButton TEDonate BCFFFFFF55 FCFFFFFFFF TAO HA0 WI150 HE50"
  (let [words (str/split desc #" ")]
    (reduce
     (fn [result word]
       (cond
         (= (count word) 1) (assoc result :id word)
         (str/starts-with? word "WI") (assoc result :width (js/parseInt (subs word 2) 10))
         (str/starts-with? word "HE") (assoc result :height (js/parseInt (subs word 2) 10))
         (str/starts-with? word "TE") (assoc result :text (str/replace (subs word 2) #"~" " "))
         :default (assoc result (keyword (str/lower-case (subs word 0 2))) (subs word 2))))
     {}
     words)))


(defn add-subview [{subviews :subviews :as view} subview]
  "inserts subview's id to views sv property"
  (assoc view :subviews (conj subviews (subview :id))))


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
     (let [{:keys [ta ba la ra va ha]} (val item)]
       (assoc oldmap (key item)
              (-> (val item)
                  (assoc :ta (get-key-for-name oldmap ta))
                  (assoc :ba (get-key-for-name oldmap ba))
                  (assoc :la (get-key-for-name oldmap la))
                  (assoc :ra (get-key-for-name oldmap ra))
                  (assoc :ha (get-key-for-name oldmap ha))
                  (assoc :va (get-key-for-name oldmap va))))))
   viewmap
   viewmap))


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
                                  view (assoc (gen-view kw "none" "none" 100 100 "00000000") :text id)]
                              (assoc res kw view))
                            res)))
                      {}
                      words)]
    (into viewmap views)))


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
                            (assoc res (keyword (str/lower-case key)) value)))
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
    (println "result" result)
    result))


(defn gen-from-desc [desc tempcanvas]
  "generate view structure from description"  
  (let [lines (str/split-lines desc)]
    (-> (gen-base-views lines tempcanvas)
        (replace-alignment-names))))


(defn align-view [viewmap id cx cy width height]
  "aligns view"
  (let [view (get viewmap id)
        {:keys [x y ta ba la ra va ha class text] w :width h :height } view
        taview (get viewmap ta)
        baview (get viewmap ba)
        laview (get viewmap la)
        raview (get viewmap ra)
        haview (get viewmap ha)
        vaview (get viewmap va)
        result
        (-> view
            (assoc :x (cond
                        ;; align to view on the left or to screen edge
                        (not= la nil)
                        (if (= la "0")
                          0
                          (+ cx (laview :x) (laview :width)))
                        ;; align to view on the right or to screen edge
                        (not= ra nil)
                        (if (= ra "0")
                          (- width w)
                          (- (raview :x) w))
                        ;; align to horizontal center or between left align and right align view
                        (not= ha nil)
                        (if (= ha "0")
                          (+ cx (- (/ width 2) (/ w 2)))
                          (- (- (laview :x) (/ (- (raview :x)(+ (laview :x)(laview :width))) 2) ) (/ w 2)))
                        ;; or leave x position as is
                        :default
                        x))
            (assoc :y (cond
                        ;; align to view on the top or to screen edge
                        (not= ta nil)
                        (if (= ta "0")
                          0
                          (+ (taview :y)(taview :height)))
                        ;; align to view on the bottom or to screen edge
                        (not= ba nil)
                        (if (= ba "0")
                          (- height h)
                          (- (baview :y) h))
                        ;; align to vertical center or between bottom and top align view
                        (not= va nil)
                        (if (= va "0")
                          (+ cy (- (/ height 2) (/ h 2)))
                          (- (- (baview :y) (/ (- (baview :y)(+ (taview :y)(taview :height))) 2 )) (/ h 2)))
                        :default
                        y)))]
    ;;(println "a:" cl te x y w h
   ;;         "to" cx cy width height
    ;;         "final" (result :x) (result :y) (result :width) (result :height))
    result))


(defn align [viewmap coll cx cy width height]
  "iterate through all views and align them based on their alignment switches"
  (reduce (fn [oldmap id]
            (let [view (get oldmap id)
                  {:keys [x y ta ba la ra va ha id class text] w :width h :height} view
                  ;; filter nil and 0 switches, 0 means to full parent view
                  toalign (filter #(and (not= % nil) (not= % "0")) [ta ba la ra va ha])
                  ;; first align relative views
                  newmap (align oldmap toalign (+ cx x) (+ cy y) width height)
                  ;; align self
                  newview (align-view newmap id cx cy width height)
                  ;; align subviews
                  newnewmap (align newmap (newview :subviews) (newview :x) (newview :y) (newview :width) (newview :height))]
              (assoc newnewmap id newview)))
          viewmap
          coll))


(defn collect-visible-ids [viewmap coll path]
  "collects ids of views that are currently visible"
  ;;(println "c:" path)
  (reduce
   (fn [res id]
     (let [view (viewmap id)]
       (concat res (collect-visible-ids viewmap (view :subviews) (str path ":" (view :class) )))))
   coll
   coll))


(defn collect-pressed-views [viewmap event]
  "collects view under touch point"
  (reduce
   (fn [result view]
     (let [{:keys [id x y] w :width h :height} view
           px (event :x)
           py (event :y)]
       (if (and (and (> px x) (< px (+ x w))) (and (> py y) (< py (+ y h))))
         (conj result id)
         result)))
   []
   (vals viewmap)))
