(ns brawl.audio)


(defn sounds
  []
  {:shot (js/Audio. "sounds/shot.mp3")
   :theme (js/Audio. "sounds/theme.mp3")
   :argh0 (js/Audio. "sounds/argh0.mp3")
   :argh1 (js/Audio. "sounds/argh1.mp3")
   :argh2 (js/Audio. "sounds/argh2.mp3")
   :death0 (js/Audio. "sounds/death0.mp3")
   :death1 (js/Audio. "sounds/death1.mp3")
   :punch0 (js/Audio. "sounds/punch0.mp3")
   :punch1 (js/Audio. "sounds/punch1.mp3")
   :punch2 (js/Audio. "sounds/punch2.mp3")})


(defn set-music-volume
  [{:keys [sounds volumes] :as state}]
  (set! (.-volume  (:theme sounds)) (:music volumes))
  state)


(defn set-effects-volume
  [{:keys [sounds volumes] :as state}]
  (doall (map (fn [[id audio]] (set! (.-volume audio) (:effects volumes))) sounds))
  (set-music-volume state))
