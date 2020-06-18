(ns brawl.metrics)

(defn basemetrics-normalize
  "normalize base values"
  [base mkey]
  (let [{:keys [hitpower hitrate stamina speed height color_a color_b]} base
        half (- 1.25 (* height 0.5))
        hp (if (= mkey :hitpower) hitpower (- half hitrate)) 
        hr (if (= mkey :hitrate) hitrate (- half hp))
        sp (if (= mkey :speed) speed (- half stamina))
        st (if (= mkey :stamina) stamina (- half sp))
        nhp (max 0.0 (min 1.0 (min hp half)))
        nhr (max 0.0 (min 1.0 (min hr half)))
        nst (max 0.0 (min 1.0 (min st half)))
        nsp (max 0.0 (min 1.0 (min sp half)))]
    (assoc base :hitpower nhp :hitrate nhr :stamina nst :speed nsp)))


(defn basemetrics-default
  "default base metrics"
  []
  (basemetrics-normalize
   {:height 0.5
    :hitpower 0.5
    :hitrate 0.5
    :stamina 0.5
    :speed 0.5
    :color_a [1.0 0.0 0.0 1.0]
    :color_b [0.0 0.0 1.0 1.0]} :height))


(defn basemetrics-random
  "rabdin base metrics"
  []
  (basemetrics-normalize
   {:height (/ (rand 10) 10)
    :hitpower (/ (rand 10) 10)
    :stamina (/ (rand 10) 10)
    :hitrate (/ (rand 10) 10)
    :speed (/ (rand 10) 10)
    :color_a [1.0 (rand) (rand) 1.0]
    :color_b [1.0 (rand) (rand) 1.0]} :height))


(defn generate-metrics
  "generate metrics based on base metrics"
  [base]
  (let [{:keys [hitpower hitrate stamina speed height color_a color_b]} base
        hp hitpower
        hr hitrate
        st stamina
        sp speed
        size height

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
        dbb (if (> bb 0.2) (- bb 0.2) bb)]
    
    {:headl headl :bodyl bodyl :arml arml :legl legl ; lengths
     :headw headw :neckw neckw :armw armw :bodyw headw :hipw hipw :legw legw ; widths
     :walks walks :runs runs   :punchs punchs :kicks kicks ; speed
     :maxp maxp :hitp hitp  :kickp kickp :maxh maxh ; power and health
     :cola color_a :colb [dra dga dba 1.0] :colc color_b :cold [drb dgb dbb 1.0]
     :base base})) ; colors

