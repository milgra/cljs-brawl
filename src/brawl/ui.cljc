(ns brawl.ui
  (:require [gui.kinetix :as kinetix]
            [gui.math4 :as math4]
            [gui.webgl :as uiwebgl]
            [brawl.actor :as actor]
            [brawl.defaults :as defaults]
            [brawl.layouts :as layouts]))


(defn init []
  {:views {}
   :baseid nil 
   :viewids []})


(defn load-ui
  "load new ui stack"
  [{{:keys [views viewids] :as ui} :ui :as state }  description]
  (let [views (kinetix/gen-from-desc {} description)
        baseid (keyword (:id description))
        viewids (kinetix/collect-visible-ids views [baseid] "")
        alignedviews (kinetix/align views [baseid] 0 0 (. js/window -innerWidth) (. js/window -innerHeight))]

    (assoc state :ui {:baseid baseid
                      :views alignedviews
                      :viewids viewids})))

  
(defn update-ui [{{:keys [baseid views commands-ui]} :ui :as state} msg]
  (let [results (if (and msg (= (:id msg) "mouse"))
                  (let [touched-views (kinetix/collect-pressed-views views (:point msg))]
                    (reduce
                     (fn [result {:keys [class] :as view}]
                       (cond
                         (= class "Slider") (conj result (kinetix/touch-slider view views msg))
                         (= class "Button") (conj result (kinetix/touch-button view views msg))
                         :else result))
                     []
                     (map views touched-views)))
                  [])
        newviews (reduce (fn [oldviews {newviews :views}]
                           (reduce #(assoc oldviews (:id %2) %2) oldviews newviews))
                         views
                         results)
        
        newnewviews (if (and msg (= (:id msg) "resize"))
                      (kinetix/align newviews [baseid] 0 0 (. js/window -innerWidth) (. js/window -innerHeight))
                      newviews)
        
        newcommands (map :command results)]
    (cond-> state
      true (assoc-in [:ui :views] newnewviews)
      true (assoc :commands-ui (concat commands-ui newcommands)))))


(defn align [{{:keys [baseid views]} :ui :as state}]
  (assoc-in state [:ui :views] (kinetix/align views [baseid] 0 0 (. js/window -innerWidth) (. js/window -innerHeight))))


(defn update-gen-sliders [{:keys [ui world] :as state}]
  (let [views (:views ui)
        {:keys [height hitpower hitrate stamina speed]} (get-in world [:actors :hero :metrics :base])
        hpsl (:Hitpower views)
        hrsl (:Hitrate views)
        hesl (:Height views)
        spsl (:Speed views)
        stsl (:Stamina views)
        newv (-> views
                 (kinetix/set-slider-value hpsl hitpower)
                 (kinetix/set-slider-value hrsl hitrate)
                 (kinetix/set-slider-value hesl height)
                 (kinetix/set-slider-value spsl speed)
                 (kinetix/set-slider-value stsl stamina))]
    (assoc-in state [:ui :views] newv)))


(defn set-slider-value [{ {:keys [views] } :ui :as state} id value]
  (assoc-in state [:ui :views] (kinetix/set-slider-value views (id views) value)))


(defn execute-commands
  [{:keys [level] commands :commands-ui ui-drawer :ui-drawer :as state}]
  (reduce
   (fn [oldstate {text :text :as command}]
     (let [hero (get-in oldstate [:worlds :actors :hero])
           path-metrics [:world :actors :hero :metrics]]
       (cond
         ;; ui
         (= text "set-hitpower") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :hitpower (:ratio command))
                         (actor/basemetrics-normalize :hitpower))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
         (= text "set-hitrate") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :hitrate (:ratio command))
                         (actor/basemetrics-normalize :hitrate))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
         (= text "set-height") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :height (:ratio command))
                         (actor/basemetrics-normalize :height))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
         (= text "set-speed") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :speed (:ratio command))
                         (actor/basemetrics-normalize :speed))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
         (= text "set-stamina") ; update base metrics and generate new metrics for hero
         (let [nbase (-> (get-in hero [:metrics :base])
                         (assoc :stamina (:ratio command))
                         (actor/basemetrics-normalize :stamina))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
         (= text "randomize")
         (let [nbase (-> (actor/basemetrics-random)
                         (actor/basemetrics-normalize :height))
               nmetrics (actor/generate-metrics nbase)]
           (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
         
         (= text "show-menu") (load-ui oldstate layouts/menu)
         (= text "continue") (load-ui oldstate (if (= (:level oldstate) 0) layouts/generator layouts/hud))
         (= text "options")
         (let [newstate (load-ui oldstate layouts/options)]
           (-> newstate
               (set-slider-value :Music (get-in newstate [:volumes :music]))
               (set-slider-value :Effects (get-in newstate [:volumes :effects]))
               (align)))
         
         (= text "donate") (defaults/save-defaults! oldstate)
         (= text "options back") (load-ui oldstate layouts/menu)

         (= text "redraw-ui") (assoc oldstate :ui-drawer (uiwebgl/reset ui-drawer))
         
         (= text "left") (assoc-in oldstate [:keycodes 37] true)
         (= text "right") (assoc-in oldstate [:keycodes 39] true)
         (= text "jump") (assoc-in oldstate [:keycodes 38] true)
         (= text "down") (assoc-in oldstate [:keycodes 40] true)
         (= text "run") (assoc-in oldstate [:keycodes 32] true)
         (= text "punch") (assoc-in oldstate [:keycodes 70] true)
         (= text "kick") (assoc-in oldstate [:keycodes 83] true)
         (= text "block") (assoc-in oldstate [:keycodes 68] true)
         
         (= text "start-game")
         (-> oldstate
             (load-ui layouts/info)
             (update :commands-ui conj {:text "load-level"}))

         :else oldstate)))
   (assoc state :commands-ui [])
   commands))