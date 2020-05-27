(ns gui.bitmap)


(defn init [w h r g b a]
  "creates bitmap with given base color"
  (let [length (* w h 4)
        data (js/Uint8Array. length)
        chunk (js/Uint8Array. [r g b a])]
    (loop [index 0]
      (if (< index length)
        (do
          (.set data chunk index)
          (recur (+ index 4)))
        {:data data
         :width w
         :height h}))))


(defn clear [{:keys [width height data] :as bmp} r g b a]
  (let [length (* width height 4)
        chunk (js/Uint8Array. [r g b a])]
    (loop [index 0]
      (if (< index length)
        (do
          (.set data chunk index)
          (recur (+ index 4)))
        bmp))))


(defn insert [{da :data wa :width ha :height :as bmp}
              {db :data wb :width hb :height :as src}
              x y]
  "insert bitmap into a larger bitmap"
  (loop [index 0]
    (let [src_s (* (* index wb) 4)
          src_e (* (+ (* index wb) wb) 4)
          src_row (.slice db src_s src_e)
          bmp_s (* (+ (* (+ y index) wa) x) 4)]
      (.set da src_row bmp_s)
      (if (< index hb)
        (recur (inc index))
        bmp))))

;;(insert (init 10 10 0xFF 0 0 0xFF) (init 5 5 0 0 0 0xFF) 2 2 )
