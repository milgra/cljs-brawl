;; texture map
;; places smaller bitmaps into one big bitmap to use it as gpu texture

(ns gui.texmap
  (:require [gui.bitmap :as bitmap]))


(defn init [w h r g b a]
  "create texture map with given background color"
  (let [result {:texbmp (bitmap/init w h r g b a)
                :contents {}
                :changed true
                :lasth 0
                :lastx 0
                :lasty 0}]
    result))


(defn hasbmp? [{contents :contents} id]
  "texmap contains bitmap with given id?"
  (contains? contents id))


(defn getbmp [{contents :contents} id]
  "returns bitmap with given id"
  (get contents id))


(defn setbmp [{:keys [texbmp contents lastx lasty lasth] :as texmap}
              {:keys [data width height] :as bitmap}
              texid
              inset]
  "adds bmp to texmap with given id"
  (let [;;new height is 0 if entering new row else check if we have to increase it
        newh (if (> (+ lastx width) (texbmp :width))
               0
               (if (> height lasth)
                 height
                 lasth))

        ;;increase new y position if entering new row
        newy (if (> (+ lastx width) (texbmp :width))
               (+ lasty lasth)
               lasty)

        ;;jump to row start if entering new row
        newx (if (> (+ lastx width) (texbmp :width))
               0
               lastx)

        texw  (+ newx width)
        texh  (+ newy height)

        newtlx (/ (+ newx inset) (texbmp :width))
        newtly (/ (+ newy inset) (texbmp :height))
        newbrx (/ (- texw inset) (texbmp :width))
        newbry (/ (- texh inset) (texbmp :height))
        
        over? (> newh (texbmp :height))]
    
    (if over?
      nil
      (-> texmap
          (assoc-in [:contents texid] [newtlx newtly newbrx newbry])
          (assoc :lastx texw)
          (assoc :lasty newy)
          (assoc :lasth newh)
          (assoc :texbmp (bitmap/insert texbmp bitmap newx newy))
          (assoc :changed true)))))
