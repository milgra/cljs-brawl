(ns brawl.ui
  (:require [gui.kinetix :as kinetix]
            [gui.math4 :as math4]
            [gui.webgl :as uiwebgl]
            [brawl.actor :as actor]
            [brawl.defaults :as defaults]
            [brawl.layouts :as layouts]))


(defn init
  "creates empty ui structure"
  []
  {:views {}
   :baseid nil 
   :viewids []
   :projection (math4/proj_ortho 0 (.-innerWidth js/window) (.-innerHeight js/window) 0 -10.0 10.0)})


(defn load-ui
  "load new ui stack"
  [state description]
  (let [views (kinetix/gen-from-desc {} description)
        baseid (keyword (:id description))
        viewids (kinetix/collect-visible-ids views [baseid] "")
        alignedviews (kinetix/align views [baseid] 0 0 (. js/window -innerWidth) (. js/window -innerHeight))]

    (assoc state :ui {:views alignedviews
                      :baseid baseid
                      :viewids viewids
                      :projection (math4/proj_ortho 0 (.-innerWidth js/window) (.-innerHeight js/window) 0 -10.0 10.0)})))

  
(defn update-ui
  "update view based on user actions"
  [{{:keys [baseid views commands-ui projection]} :ui :as state} msg]
  (let [pressed-views (if-not (and msg (= (:id msg) "mouse"))
                        nil
                        (kinetix/collect-pressed-views views (:point msg)))

        altered-views (if-not pressed-views
                        []
                        (mapcat :views
                                (reduce (fn [result {:keys [class] :as view}]
                                          (cond
                                            (= class "Slider") (conj result (kinetix/touch-slider view views msg))
                                            (= class "Button") (conj result (kinetix/touch-button view views msg))
                                            :else result))
                                        []
                                        (map views pressed-views))))
        
        new-views (reduce #(assoc %1 (:id %2) %2) views altered-views)

        newcommands (map :command altered-views)

        newnew-views (if-not msg
                      new-views
                      (kinetix/align new-views [baseid] 0 0 (. js/window -innerWidth) (. js/window -innerHeight)))

        new-projection (if-not (and msg (= (:id msg) "resize"))
                         projection
                         (math4/proj_ortho 0 (.-innerWidth js/window) (.-innerHeight js/window) 0 -10.0 10.0))]

    (-> state
        (assoc-in [:ui :projection] new-projection)
        (assoc-in [:ui :views] newnew-views)
        (assoc :commands-ui (concat commands-ui newcommands)))))


(defn align
  "align view stack, usually on context size change"
  [{{:keys [baseid views]} :ui :as state}]
  (assoc-in state [:ui :views] (kinetix/align views [baseid] 0 0 (. js/window -innerWidth) (. js/window -innerHeight))))


(defn update-gen-sliders
  "updates generator sliders"
  [{:keys [ui world] :as state}]
  (let [views (:views ui)
        {:keys [height hitpower hitrate stamina speed]} (get-in world [:actors :hero :metrics :base])
        hpsl (:Hitpower views)
        hrsl (:Hitrate views)
        hesl (:Height views)
        spsl (:Speed views)
        stsl (:Stamina views)
        new-views (-> views
                      (kinetix/set-slider-value hpsl hitpower)
                      (kinetix/set-slider-value hrsl hitrate)
                      (kinetix/set-slider-value hesl height)
                      (kinetix/set-slider-value spsl speed)
                      (kinetix/set-slider-value stsl stamina))]
    (assoc-in state [:ui :views] new-views)))


(defn set-slider-value [{{:keys [views]} :ui :as state} id value]
  "sets slider value"
  (assoc-in state [:ui :views] (kinetix/set-slider-value views (id views) value)))


(defn execute-commands
  "executes commands coming from the ui"
  [{:keys [level commands-ui ui-drawer] :as state} msg]
  (let [commands (if-not (and msg (= (:id msg) "redraw-ui"))
                   commands-ui
                   (conj commands-ui {:text "redraw-ui"}))]
        (reduce
         (fn [oldstate {text :text type :type :as command}]
           (let [hero (get-in oldstate [:worlds :actors :hero])
                 path-metrics [:world :actors :hero :metrics]]
             (cond
               (= text "start-game")
               (-> oldstate
                   (load-ui layouts/info)
                   (update :commands-world conj {:text "load-level"}))
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
               (and (= text "randomize") (= type "down")) ; randomizes generator values
               (let [nbase (-> (actor/basemetrics-random)
                               (actor/basemetrics-normalize :height))
                     nmetrics (actor/generate-metrics nbase)]
                 (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))         
               (and (= text "show-menu") (= type "down")) ; shows menu view
               (load-ui oldstate layouts/menu)
               (and (= text "continue") (= type "down")) ; shows hud
               (load-ui oldstate (if (= (:level oldstate) 0) layouts/generator layouts/hud))
               (and (= text "options") (= type "down")) ; shows options view
               (let [newstate (load-ui oldstate layouts/options)]
                 (-> newstate ;  set slider values
                     (set-slider-value :Music (get-in newstate [:volumes :music]))
                     (set-slider-value :Effects (get-in newstate [:volumes :effects]))
                     (align)))
               (and (= text "donate") (= type "down")) ; opens donate link in browser
               (defaults/save-defaults! oldstate)
               (and (= text "options back") (= type "down")) ; opens menu view
               (load-ui oldstate layouts/menu)
               ;; on-screen control buttons
               (and (= text "left") (= type "down")) (assoc-in oldstate [:keycodes 37] true)
               (and (= text "left") (= type "up")) (assoc-in oldstate [:keycodes 37] false)
               (and (= text "right")(= type "down")) (assoc-in oldstate [:keycodes 39] true)
               (and (= text "right")(= type "up")) (assoc-in oldstate [:keycodes 39] false)
               (and (= text "jump")(= type "down")) (assoc-in oldstate [:keycodes 38] true)
               (and (= text "jump")(= type "up")) (assoc-in oldstate [:keycodes 38] false)
               (and (= text "down")(= type "down")) (assoc-in oldstate [:keycodes 40] true)
               (and (= text "down")(= type "up")) (assoc-in oldstate [:keycodes 40] false)
               (and (= text "run")(= type "down")) (assoc-in oldstate [:keycodes 32] true)
               (and (= text "run")(= type "up")) (assoc-in oldstate [:keycodes 32] false)
               (and (= text "punch")(= type "down")) (assoc-in oldstate [:keycodes 70] true)
               (and (= text "punch")(= type "up")) (assoc-in oldstate [:keycodes 70] false)
               (and (= text "kick")(= type "down")) (assoc-in oldstate [:keycodes 83] true)
               (and (= text "kick")(= type "up")) (assoc-in oldstate [:keycodes 83] false)
               (and (= text "block")(= type "down")) (assoc-in oldstate [:keycodes 68] true)
               (and (= text "block")(= type "up")) (assoc-in oldstate [:keycodes 68] false)
               ;; font loaded, reset textures to force redraw with new fonts
               (= text "redraw-ui") (assoc oldstate :ui-drawer (uiwebgl/reset ui-drawer))
               :else oldstate)))
         (assoc state :commands-ui [])
         commands)))
