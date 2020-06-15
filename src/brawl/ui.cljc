(ns brawl.ui
  (:require [goog.window :as window]
            [gui.kinetix :as kinetix]
            [gui.math4 :as math4]
            [gui.webgl :as uiwebgl]
            [brawl.audio :as audio]
            [brawl.actor :as actor]
            [brawl.metrics :as metrics]
            [brawl.defaults :as defaults]
            [brawl.layouts :as layouts]))


(defn init
  "creates empty ui structure"
  []
  {:views {}
   :baseid nil 
   :viewids []
   :projection (math4/proj_ortho 0 (.-innerWidth js/window) (.-innerHeight js/window) 0 -10.0 10.0)})


(defn align-and-upload
  "update ui drawer's content"
  [{{:keys [baseid views viewids]} :ui ui-drawer :ui-drawer :as state}]
  (let [newproj (math4/proj_ortho 0 (.-innerWidth js/window) (.-innerHeight js/window) 0 -10.0 10.0)
        newviews (kinetix/align views [baseid] 0 0 (. js/window -innerWidth) (. js/window -innerHeight))]
    (-> state
        (assoc-in [:ui :views] newviews)
        (assoc-in [:ui :projection] newproj)
        (assoc :ui-drawer (uiwebgl/update! ui-drawer (map newviews viewids))))))


(defn load-ui
  "load new ui stack"
  [{:keys [ui-drawer] :as state} description]
  (let [views (kinetix/gen-from-desc {} description (/ 1.0 (:ui-ratio state)))
        baseid (keyword (:id description))
        viewids (kinetix/collect-visible-ids views [baseid] "")
        newstate (assoc state :ui {:views views
                                   :baseid baseid
                                   :viewids viewids})]
    (align-and-upload newstate)))
  

(defn update-ui
  "update view based on user actions"
  [{{:keys [baseid views viewids commands-ui projection]} :ui ui-drawer :ui-drawer :as state} msg time delta]
  (if-not (and msg (= (:id msg) "mouse"))
    state
    (let [pressed-views (kinetix/collect-pressed-views views (:point msg))
          altered-views (if-not pressed-views
                          []
                          (reduce (fn [result {:keys [class] :as view}]
                                    (cond
                                      (= class "Slider") (conj result (kinetix/touch-slider view views msg))
                                      (= class "Button") (conj result (kinetix/touch-button view views msg))
                                      :else result))
                                  []
                                  (map views pressed-views)))
          
          new-views (reduce #(assoc %1 (:id %2) %2) views (mapcat :views altered-views))

          new-commands (map :command altered-views)]
      (-> state
          (assoc-in [:ui :views] new-views)
          (assoc :commands-ui (concat commands-ui new-commands))
          (align-and-upload)))))


(defn set-slider-value [{{:keys [views]} :ui :as state} id value]
  "sets slider value"
  (assoc-in state [:ui :views] (kinetix/set-slider-value views (id views) value)))


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


(defn execute-commands
  "executes commands coming from the ui"
  [{:keys [level commands-ui ui-drawer] :as state} msg]
  (let [commands (if-not msg
                   commands-ui
                   (cond
                     (= (:id msg) "redraw-ui") (conj commands-ui {:text "redraw-ui"})
                     (= (:id msg) "resize") (conj commands-ui {:text "redraw-ui"})))
        result (reduce
                (fn [oldstate {text :text type :type ratio :ratio :as command}]
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
                                      (metrics/basemetrics-normalize :hitpower))
                            nmetrics (metrics/generate-metrics nbase)]
                        (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
                      (= text "set-hitrate") ; update base metrics and generate new metrics for hero
                      (let [nbase (-> (get-in hero [:metrics :base])
                                      (assoc :hitrate (:ratio command))
                                      (metrics/basemetrics-normalize :hitrate))
                            nmetrics (metrics/generate-metrics nbase)]
                        (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
                      (= text "set-height") ; update base metrics and generate new metrics for hero
                      (let [nbase (-> (get-in hero [:metrics :base])
                                      (assoc :height (:ratio command))
                                      (metrics/basemetrics-normalize :height))
                            nmetrics (metrics/generate-metrics nbase)]
                        (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
                      (= text "set-speed") ; update base metrics and generate new metrics for hero
                      (let [nbase (-> (get-in hero [:metrics :base])
                                      (assoc :speed (:ratio command))
                                      (metrics/basemetrics-normalize :speed))
                            nmetrics (metrics/generate-metrics nbase)]
                        (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
                      (= text "set-stamina") ; update base metrics and generate new metrics for hero
                      (let [nbase (-> (get-in hero [:metrics :base])
                                      (assoc :stamina (:ratio command))
                                      (metrics/basemetrics-normalize :stamina))
                            nmetrics (metrics/generate-metrics nbase)]
                        (-> oldstate (assoc-in path-metrics nmetrics) (update-gen-sliders)))
                      (= text "set music volume")
                      (-> oldstate
                          (assoc-in [:volumes :music] ratio)
                          (audio/set-music-volume)                 
                          (defaults/save-defaults!))
                      (= text "set effects volume")
                      (-> oldstate
                          (assoc-in [:volumes :effects] ratio)
                          (audio/set-effects-volume)                 
                          (defaults/save-defaults!))
                      (and (= text "show physics") (= type "up")) ; shows options view
                      (defaults/save-defaults! (update oldstate :physics not))
                      (and (= text "randomize") (= type "up")) ; randomizes generator values
                      (let [nbase (-> (metrics/basemetrics-random)
                                      (metrics/basemetrics-normalize :height))
                            nmetrics (metrics/generate-metrics nbase)]
                        (-> oldstate
                            (assoc-in path-metrics nmetrics)
                            (assoc :metrics nbase)
                            (defaults/save-defaults!)
                            (update-gen-sliders)))         
                      (and (= text "show-menu") (= type "up")) ; shows menu view
                      (load-ui oldstate layouts/menu)
                      (and (= text "continue") (= type "down")) ; shows hud
                      (if (= (:level oldstate) 0)
                        (-> oldstate
                            (load-ui layouts/generator)
                            (update-gen-sliders))
                        (-> oldstate
                            (load-ui layouts/hud)))
                      (and (= text "new game") (= type "up"))
                      (update oldstate :commands-world conj {:text "new-game"})
                      (and (= text "restart level") (= type "up"))
                      (update oldstate :commands-world conj {:text "restart-level"})
                      (and (= text "options") (= type "up")) ; shows options view
                      (let [newstate (load-ui oldstate layouts/options)]
                        (-> newstate ;  set slider values
                            (set-slider-value :Music (get-in newstate [:volumes :music]))
                            (set-slider-value :Effects (get-in newstate [:volumes :effects]))))
                      (and (= text "fullscreen") (= type "up")) ; randomizes generator values
                      (do
                        (if (.-fullscreenElement js/document)
                          (.exitFullscreen js/document)
                          (.requestFullscreen (.-documentElement js/document)))
                        oldstate)
                      (and (= text "donate") (= type "up")) ; opens donate link in browser
                      (do
                        (goog.window/open "https://paypal.me/milgra")
                        oldstate)
                      (and (= text "source code") (= type "up")) ; opens donate link in browser
                      (do
                        (goog.window/open "https://github.com/milgra/cljs-brawl")
                        oldstate)
                      (and (= text "options back") (= type "up")) ; opens menu view
                      (load-ui oldstate layouts/menu)
                      ;; on-screen control buttons
                      (and (= text "left") (= type "down"))
                      (-> oldstate
                          (assoc-in [:keycodes 37] true)
                          (assoc-in [:keycodes 32] true))
                      (and (= text "left") (= type "up"))
                      (-> oldstate
                          (assoc-in [:keycodes 37] false)
                          (assoc-in [:keycodes 32] false))
                      (and (= text "right")(= type "down"))
                      (-> oldstate
                          (assoc-in [:keycodes 39] true)
                          (assoc-in [:keycodes 32] true))
                      (and (= text "right")(= type "up"))
                      (-> oldstate
                          (assoc-in [:keycodes 39] false)
                          (assoc-in [:keycodes 32] false))
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
                      (= text "redraw-ui")
                      (-> oldstate
                          (assoc :ui-drawer (uiwebgl/reset ui-drawer)))

                      :else oldstate)))
                (assoc state :commands-ui [])
                commands)]
    (if (> (count commands) 0)
      (align-and-upload result)
      result)))
