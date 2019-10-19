(ns brawl.core
  (:require [clojure.xml :as xml]
            [quil.core :as q]
            [quil.middleware]
            [brawl.svg :as svg]
            [brawl.surface :as surface]
            [brawl.physics :as physics]
            [brawl.mass :as mass])
  (:gen-class))


(defn setup
  "initialize game"
  []
  (q/frame-rate 60)
  (q/color-mode :rgb)

  (let [level (svg/psvg (xml/parse "level0.svg") "")
        shapes (filter #(not= (% :id) "Surfaces") level)
        surfacepoints (filter #(= (% :id) "Surfaces") level)
        surfaces (surface/generate-from-pointlist surfacepoints)
        masses [(mass/mass2 500.0 0.0)]]
        ;;(for [x (range 0 10)] (p/mass2 (rand 1000) (rand 1000)))

    {:mainmass (mass/mass2 500.0 0.0)
     :trans [0.0 0.0]
     :keysdown { :l-down false :r-down false :u-down false :d-down false }
     :shapes shapes
     :masses masses
     :surfaces surfaces
     :surfacepoints surfacepoints}))


(defn draw-state
  "draw game"
  [{mainmass :mainmass :as state}]

  (q/background 240)
  (q/scale 0.5 0.5)
  (q/translate (:trans state))

  (let [[mx my] (:trans mainmass)
        shapes (:shapes state)
        surfacepoints (:surfacepoints state)]

    ;; shapes

    (loop [[shp :as src] shapes]
      (when (not-empty src)
        (let [col (:color shp)
              pth (:path shp)
              r (bit-shift-right (bit-and col 0xFF0000) 16)
              g (bit-shift-right (bit-and col 0x00FF00) 8)
              b (bit-and col 0x0000FF)]
          (q/fill r g b)
          (q/no-stroke)
          (q/begin-shape)
          (loop [[[x y] :as coords] pth]
            (when (not-empty coords)
              (q/vertex x y)
              (recur (rest coords))))
          (q/end-shape)
          (recur (rest src)))))

    ;; surfaces

    (loop [[shp :as src] surfacepoints]
      (when (not-empty src)
        (let [pth (:path shp)]
          (q/no-fill)
          (q/stroke 0)
          (q/begin-shape)
          (loop [[[x y] :as coords] pth]
            (when (not-empty coords)
              (q/vertex x y)
              (recur (rest coords))))
          (q/end-shape))))

    ;; main mass

    (loop [[{[mx my] :trans} :as masses]
           (:masses state)]
      (when (not-empty masses)
        (q/ellipse mx my 30 30)
        (recur (rest masses))))

    (q/fill 155 255 255)
    (q/ellipse mx my 100 100)))


(defn update-state
  "update game state"
  [{:keys [mainmass keysdown masses trans] :as state}]

  (let [{:keys [l-down r-down u-down d-down]} keysdown

        [x y] (:trans mainmass)
        [sx sy] (:basis mainmass)

        ;; actual and wanted translation of screen
        [txa tya] trans
        txw (* -1 (- x (q/width)))
        tyw (* -1 (- y (q/height)))

        dc {true -1 false 0}
        ic {true 1 false 0}

        mx (+ (dc l-down) (ic r-down))
        my (+ (dc u-down) (ic d-down))

        fx (if (= mx 0)
             (* sx 0.9)
             (+ sx (* 1.0 mx)))

        fy (if (= my 0)
             (* sy 0.9)
             (+ sy (* 1.0 my)))]

    (-> (assoc state
               :mainmass
               (assoc mainmass
                      :trans [ (+ x fx) (+ y fy) ]
                      :basis [ fx fy ] )
               :trans [(+ txa (/ (- txw txa) 14.0))(+ tya (/ (- tyw tya) 14.0))])
        (mass/update-masses ,,, 1.0))))


(defn key-pressed
  "key pressed event, updating keysdown state"
  [{keysdown :keysdown :as state} event]
  (assoc state :keysdown
         (case (:key-code event)
           37 (assoc keysdown :l-down true)
           39 (assoc keysdown :r-down true)
           38 (assoc keysdown :u-down true)
           40 (assoc keysdown :d-down true)
           keysdown)))


(defn key-released
  "key released event, udpating keysdown state"
  [{keysdown :keysdown :as state} event]
  (assoc state :keysdown
         (case (:key-code event)
           37 (assoc keysdown :l-down false)
           39 (assoc keysdown :r-down false)
           38 (assoc keysdown :u-down false)
           40 (assoc keysdown :d-down false)
           keysdown)))


(defn -main
  "entering point"
  []
  (q/defsketch my-sketch
    :setup setup
    :draw draw-state
    :update update-state
    :key-pressed key-pressed
    :key-released key-released
    :size [900 600]
    :title "You spin my circle right round"
    :renderer :opengl
    :features [:keep-on-top]
    :middleware [quil.middleware/fun-mode]))

(-main)


(
 (fn minpath [src]
   (loop [line (reverse src)
          curr (repeat (count src) {:sum 0 :pth [0]})]
     (if (> (count line) 1)
       (let [sum (map
                  (fn [nmbr pair]
                    (->
                     pair
                     (update :sum #(+ % nmbr ))
                     (update :pth #(conj % nmbr))))
                  (first line)
                  curr)
             flt (map
                  (fn [[{s1 :sum :as o1}{s2 :sum :as o2}]]
                    ( if ( < s1 s2 ) o1 o2 ))
                  (partition 2 1 sum))]
         (recur (rest line) flt))
       (reverse (rest (concat (:pth (first curr)) (first line)))))))
 '([3][2 4][1 9 3][9 9 2 4][4 6 6 7 8][5 7 3 5 1 4]))
 '([1][2 4][5 1 4][2 3 4 5]))
