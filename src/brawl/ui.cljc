(ns brawl.ui
  (:require
    [brawl.actor :as actor]
    [brawl.audio :as audio]
    [brawl.defaults :as defaults]
    [brawl.layouts :as layouts]
    [brawl.metrics :as metrics]
    [goog.window :as window]
    [gui.kinetix :as kinetix]
    [gui.math4 :as math4]
    [gui.webgl :as uiwebgl]))


(defn init
  "creates empty ui structure"
  []
  {:views {}
   :baseid nil
   :viewids []
   :projection (math4/proj_ortho 0 (.-innerWidth js/window) (.-innerHeight js/window) 0 -10.0 10.0)})


(defn align-and-upload
  "update ui drawer's content"
  [state]
  (let [{:keys [baseid views viewids]} (:ui state)
        {:keys [ui-drawer]} state
        newproj (math4/proj_ortho 0 (.-innerWidth js/window) (.-innerHeight js/window) 0 -10.0 10.0)
        newviews (kinetix/align views [baseid] 0 0 (. js/window -innerWidth) (. js/window -innerHeight))]
    (-> state
      (assoc-in [:ui :views] newviews)
      (assoc-in [:ui :projection] newproj)
      (assoc :ui-drawer (uiwebgl/update! ui-drawer (map newviews viewids))))))


(defn load-ui
  "load new ui stack"
  [state description]
  (let [{:keys [ui-ratio]} state
        views (kinetix/gen-from-desc {} description (/ 1.0 ui-ratio))
        baseid (keyword (:id description))
        viewids (kinetix/collect-visible-ids views [baseid] "")
        newstate (assoc state :ui {:views views
                                   :baseid baseid
                                   :viewids viewids})]
    (align-and-upload newstate)))


(defn update-ui
  "update view based on user actions"
  [state msg time delta]
  (if-not (and msg (= (:id msg) "mouse"))
    state
    (let [{:keys [views commands-ui]} (:ui state)
          pressed-views (kinetix/collect-pressed-views views (:point msg))
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


(defn set-slider-value
  "sets slider value"
  [state id value]
  (let [{:keys [views]} (:ui state)]
    (assoc-in state [:ui :views] (kinetix/set-slider-value views (id views) value))))


(defn update-gen-sliders
  "updates generator sliders"
  [state]
  (let [{:keys [ui world]} state
        {:keys [height hitpower hitrate stamina speed]} (get-in world [:actors :hero :metrics :base])
        views (:views ui)
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


(defn update-metrics-value
  [state changed-key changed-val]
  (let [hero (get-in state [:world :actors :hero])
        path-metrics [:world :actors :hero :metrics]
        nbase (-> (get-in hero [:metrics :base])
                (assoc changed-key changed-val)
                (metrics/basemetrics-normalize changed-key))
        nmetrics (metrics/generate-metrics nbase)]
    (-> state
      (assoc-in path-metrics nmetrics)
      (update-gen-sliders))))


(defn execute-command
  [state command]
  (let [{:keys [level ui-drawer]} state
        {text :text type :type ratio :ratio} command]
    (cond
      ;; on-screen controls
      (and (= text "left") (= type "down"))   (-> state
                                                (assoc-in [:controls :left] true)
                                                (assoc-in [:controls :run] true))
      (and (= text "left") (= type "up"))     (-> state
                                                (assoc-in [:controls :left] false)
                                                (assoc-in [:controls :run] false))
      (and (= text "right") (= type "down"))   (-> state
                                                 (assoc-in [:controls :right] true)
                                                 (assoc-in [:controls :run] true))
      (and (= text "right") (= type "up"))     (-> state
                                                 (assoc-in [:controls :right] false)
                                                 (assoc-in [:controls :run] false))
      (and (= text "jump") (= type "down"))        (assoc-in state [:controls :up] true)
      (and (= text "jump") (= type "up"))          (assoc-in state [:controls :up] false)
      (and (= text "down") (= type "down"))        (assoc-in state [:controls :down] true)
      (and (= text "down") (= type "up"))          (assoc-in state [:controls :down] false)
      (and (= text "punch") (= type "down"))       (assoc-in state [:controls :punch] true)
      (and (= text "punch") (= type "up"))         (assoc-in state [:controls :punch] false)
      (and (= text "kick") (= type "down"))        (assoc-in state [:controls :kick] true)
      (and (= text "kick") (= type "up"))          (assoc-in state [:controls :kick] false)
      (and (= text "block") (= type "down"))       (assoc-in state [:controls :block] true)
      (and (= text "block") (= type "up"))         (assoc-in state [:controls :block] false)
      ;; main menu
      (and (= text "show-menu") (= type "up"))    (load-ui state layouts/menu)
      (and (= text "new game") (= type "up"))     (update state :commands-world conj {:text "new-game"})
      (and (= text "continue") (= type "down"))   (if (= (:level state) 0)
                                                    (-> state
                                                      (load-ui layouts/generator)
                                                      (update-gen-sliders))
                                                    (-> state
                                                      (load-ui layouts/hud)))
      (and (= text "options") (= type "up"))      (let [newstate (load-ui state layouts/options)]
                                                    (-> newstate
                                                      (set-slider-value :Music (get-in newstate [:volumes :music]))
                                                      (set-slider-value :Effects (get-in newstate [:volumes :effects]))))
      (and (= text "donate") (= type "up"))       (do (goog.window/open "https://paypal.me/milgra") state)
      (and (= text "source code") (= type "up"))  (do (goog.window/open "https://github.com/milgra/cljs-brawl") state)
      ;; options menu
      (= text "set music volume")                 (-> state
                                                    (assoc-in [:volumes :music] ratio)
                                                    (audio/set-music-volume)
                                                    (defaults/save-defaults!))
      (= text "set effects volume")               (-> state
                                                    (assoc-in [:volumes :effects] ratio)
                                                    (audio/set-effects-volume)
                                                    (defaults/save-defaults!))
      (and (= text "show physics") (= type "up")) (defaults/save-defaults! (update state :physics not))
      (and (= text "fullscreen") (= type "up"))   (do
                                                    (if (.-fullscreenElement js/document)
                                                      (.exitFullscreen js/document)
                                                      (.requestFullscreen (.-documentElement js/document)))
                                                    state)
      (and (= text "options back") (= type "up")) (load-ui state layouts/menu)
      ;; generator screen
      (= text "set-speed")                        (update-metrics-value state :speed (:ratio command))
      (= text "set-height")                       (update-metrics-value state :height (:ratio command))
      (= text "set-hitrate")                      (update-metrics-value state :hitrate (:ratio command))
      (= text "set-stamina")                      (update-metrics-value state :stamina (:ratio command))
      (= text "set-hitpower")                     (update-metrics-value state :hitpower (:ratio command))
      (and (= text "randomize") (= type "up"))    (let [nbase (metrics/basemetrics-random)
                                                        nmetrics (metrics/generate-metrics nbase)]
                                                    (-> state
                                                      (assoc-in [:world :actors :hero :metrics] nmetrics)
                                                      (assoc :metrics nbase)
                                                      (defaults/save-defaults!)
                                                      (update-gen-sliders)))
      (and (= text "start-game") (= type "up"))    (-> state
                                                     (load-ui layouts/info)
                                                     (update :commands-world conj {:text "load-level"}))
      ;; wasted screen
      (and (= text "restart level") (= type "up")) (update state :commands-world conj {:text "restart-level"})
      ;; font loaded, reset textures to force redraw with new fonts
      (= text "redraw-ui")                         (assoc state :ui-drawer (uiwebgl/reset ui-drawer))
      ;; return untouched
      :else state)))


(defn execute-commands
  "executes commands coming from the ui"
  [state msg]
  (let [{:keys [level commands-ui]} state
        commands (if-not msg
                   commands-ui
                   (cond
                     (= (:id msg) "redraw-ui") (conj commands-ui {:text "redraw-ui"})
                     (= (:id msg) "resize") (conj commands-ui {:text "redraw-ui"})))]
    (if (empty? commands)
      ;; nothing happend
      state
      ;; execute commands, they probably alter the ui so alignment and upload needed
      (align-and-upload
        (reduce
          (fn [oldstate command] (execute-command oldstate command))
          (assoc state :commands-ui [])
          commands)))))
