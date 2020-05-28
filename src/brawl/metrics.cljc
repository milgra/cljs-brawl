(ns brawl.metrics)

(defn basemetrics-normalize [ {:keys [hitpower hitrate stamina speed height color_a color_b] :as base} mkey]
  (let [half (- 1.25 (* height 0.5)) 
        hp (if (or (= mkey :hitrate) (= mkey :height)) (- half hitrate) hitpower) 
        hr (if (or (= mkey :hitpower) (= mkey :height)) (- half hitpower) hitrate)
        st (if (or (= mkey :speed) (= mkey :height)) (- half speed) stamina)
        sp (if (or (= mkey :stamina) (= mkey :height)) (- half stamina) speed)
        nhp (cond (< hr 0) (- hp hr) (> hr 1.0) (+ hp (- hr 1.0)) :else hp) ; check overflow
        nhr (cond (< hp 0) (- hr hp) (> hp 1.0) (+ hr (- hp 1.0)) :else hr)
        nst (cond (< sp 0) (- st sp) :else st)
        nsp (cond (< st 0) (- sp st) :else sp)]
    (assoc base :hitpower nhp :hitrate nhr :stamina nst :speed nsp)))


(defn basemetrics-default []
  (basemetrics-normalize
   {:height 0.5
    :hitpower 0.5
    :hitrate 0.5
    :stamina 0.5
    :speed 0.5
    :color_a [1.0 0.0 0.0 1.0]
    :color_b [0.0 0.0 1.0 1.0]} :height))


(defn basemetrics-random []
  (basemetrics-normalize
   {:height (/ (rand 10) 10)
    :hitpower (/ (rand 10) 10)
    :hitrate (/ (rand 10) 10)
    :stamina (/ (rand 10) 10)
    :speed (/ (rand 10) 10)
    :color_a [1.0 (rand) (rand) 1.0]
    :color_b [1.0 (rand) (rand) 1.0]} :height))


(defn generate-metrics [{:keys [hitpower hitrate stamina speed height color_a color_b] :as base}]
  (let [hp (cond (> hitpower 1.0) 1.0 (< hitpower 0.0) 0.0 :else hitpower)
        hr (cond (> hitrate 1.0) 1.0 (< hitrate 0.0) 0.0 :else hitrate)
        st (cond (> stamina 1.0) 1.0 (< stamina 0.0) 0.0 :else stamina)
        sp (cond (> speed 1.0) 1.0 (< speed 0.0) 0.0 :else speed)

        size (cond (> height 1.0) 1.0
                   (< height 0.0) 0.0
                   :else height)

        headl (+ 16.0 (* size 8.0))
        bodyl (+ 50.0 (* size 20.0)) 
        arml (+ 50.0 (* size 20.0)) 
        legl (+ 60.0 (* size 20.0)) 

        headw (+ 36.0 (* size 8.0)) 
        neckw (+ 4.0 (* hp 5.0)) 
        armw (+ 4.0 (* hp 7.0)) 
        hipw (+ 6.0 (* st 20.0)) 
        legw (+ 6.0 (* st 5.0)) 
        
        runs (+ 8.0 (* sp 4.0) height)
        walks (* runs 0.5)
        punchs (+ 7.0 (* hr 2.0))
        kicks (+ 0.2 hr)

        maxh (+ 100.0 (* st 10.0))
        maxp (+ 100.0 (* hp 10.0))

        hitp (+ (* maxp 0.3) (* maxp 0.2 hp ) )
        kickp (+ (* maxp 0.3) (* maxp 0.2 hp ) )

        [ra ga ba] color_a
        [rb gb bb] color_b

        dra (if (> ra 0.2) (- ra 0.2) ra)
        dga (if (> ga 0.2) (- ga 0.2) ga)
        dba (if (> ba 0.2) (- ba 0.2) ba)

        drb (if (> rb 0.2) (- rb 0.2) rb)
        dgb (if (> gb 0.2) (- gb 0.2) gb)
        dbb (if (> bb 0.2) (- bb 0.2) bb)
        ; TODO bodyw needed?
        result {:headl headl :bodyl bodyl :arml arml :legl legl ; lengths
                :headw headw :neckw neckw :armw armw :bodyw headw :hipw hipw :legw legw ; widths
                :walks walks :runs runs   :punchs punchs :kicks kicks ; speed
                :maxp maxp :hitp hitp  :kickp kickp :maxh maxh ; power and health
                :cola color_a :colb [dra dga dba 1.0] :colc color_b :cold [drb dgb dbb 1.0]
                :base base}] ; colors
    result))

