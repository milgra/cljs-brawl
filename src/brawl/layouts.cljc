(ns brawl.layouts)

;; class - built in classes Button, Indicator
;; color - background color
;; width - pixel or percent "100 px" "30 %"
;; height - pixel or percent "100 px" "30 %"
;; center-x - x center of element, optional, percent "50 %"
;; center-y - y center of element, optional, percent "20 %"
;; top - positon of top edge of element, pixel or percent, absolute or relative "Edge 50 %" "CustomButton 10 px"
;; bottom - positon of top edge of element, pixel or percent, absolute or relative "Edge 50 %" "CustomButton 10 px"
;; left - positon of top edge of element, pixel or percent, absolute or relative "Edge 50 %" "CustomButton 10 px"
;; right - positon of top edge of element, pixel or percent, absolute or relative "Edge 50 %" "CustomButton 10 px"

(def sliders
;; |Hitpower Height Stamina|
;; |Hitrate         Speed  |
  [{:id "Hitpower"
    :class "Slider"
    :texture "Color 0x555555FF"
    :label "Label 25 Hitpower 00000000 FFFFFFFF"
    :width "150 px"
    :height "50 px"
    :command "set-hitpower"}

   {:id "Height"
    :class "Slider"
    :texture "Color 0x555555FF"
    :label "Label 25 Height 00000000 FFFFFFFF"
    :width "150 px"
    :height "50 px"
    :center-x "Edge 50 %"
    :command "set-heivght"}
   
   {:id "Stamina"
    :class "Slider"
    :texture "Color 0x555555FF"
    :label "Label 25 Stamina 00000000 FFFFFFFF"
    :width "150 px"
    :height "50 px"
    :right "Edge 0 px"
    :command "set-stamina"}
   
   {:id "Hitrate"
    :class "Slider"
    :texture "Color 0x555555FF"
    :label "Label 25 Hitrate 00000000 FFFFFFFF"
    :width "150 px"
    :height "50 px"
    :bottom "Edge 0 px"
    :command "set-hitrate"}
   
   {:id "Speed"
    :class "Slider"
    :texture "Color 0x555555FF"
    :label "Label 25 Speed 00000000 FFFFFFFF"
    :width "150 px"
    :height "50 px"
    :right "Edge 0 px"
    :bottom "Edge 0 px"
    :command "set-speed"}])
  

(def generator
;;|                Menu|
;;|     Sliders        |
;;|                    |
;;|     Randomize      |
;;|     StartGame      |
  [{:id "Menu"
    :class "Button"
    :texture "Color 0xFFFFFF09"
    :label "Label 25 Menu FFFFFF11 FFFFFFFF"
    :command "show-menu" 
    :width "150 px" 
    :height "50 px" 
    :top "Edge 0 px" 
    :right "Edge 0 px"}

   {:id "Sliders"
    :texture "Color FF00FF55"
    :width "453 px"
    :height "101 px"
    :center-x "Edge 50 %"
    :center-y "Edge 10 %"
    :subviews sliders}

   {:id "Randomize"
    :class "Button" 
    :texture "Color 0xFFFFFF19"
    :label "Label 25 Randomize FFFFFF09 FFFFFFFF"
    :command "randomize" 
    :width "200 px" 
    :height "50 px" 
    :center-x "Edge 50 %"
    :bottom "StartGame 10 px"}
   
   {:id "StartGame"
    :class "Button"
    :texture "Color 0xDD0000FF"
    :label "Label 25 Startgame FFFFFF11 FFFFFFFF"
    :command "start-game" 
    :width "200 px" 
    :height "50 px"
    :center-x "Edge 50 %"
    :bottom "Edge 50 px"}])


(def hud
;;                                      Menu|
;;                                          |
;;Punch                                     |
;;Block Jump                      Left Right|
;;Kick  Shoot   Health Bullets Power    Down|
  [{:id "Menu"
    :class "Button"
    :texture "Color 0xFFFFFF19"
    :label "Label 25 Menu FFFFFF11 FFFFFFFF"
    :command "show-menu" 
    :width "150 px" 
    :height "50 px" 
    :top "Edge 0 px" 
    :right "Edge 0 px"}

   {:id "Kick"
    :class "Button"
    :texture "Color 0xFFFFFF09"
    :label "Label 25 KICK(S) FFFFFF09 FFFFFFFF"
    :command "kick"
    :width "150 px"
    :height "150 px"
    :left "Edge 4 px"
    :bottom "Edge 4 px"}

   {:id "Block"
    :class "Button" 
    :texture "Color 0xFFFFFF09"
    :label "Label 25 BLOCK(D) FFFFFF09 FFFFFFFF"
    :command "block" 
    :width "150 px" 
    :height "150 px" 
    :left "Edge 4 px"
    :bottom "Kick 4 px"}
   
   {:id "Punch"
    :class "Button" 
    :texture "Color 0xFFFFFF09"
    :label "Label 25 PUNCH(F) FFFFFF09 FFFFFFFF"
    :command "punch" 
    :width "150 px" 
    :height "150 px" 
    :left "Edge 4 px"
    :bottom "Block 4 px"}

   {:id "Shoot"
    :class "Button" 
    :texture "Color 0xFFFFFF09"
    :label "Label 25 SHOOT(V) FFFFFF09 FFFFFFFF"
    :command "shoot" 
    :width "150 px" 
    :height "150 px" 
    :left "Kick 4 px"
    :bottom "Edge 4 px"}
   
   {:id "Jump"
    :class "Button" 
    :texture "Color 0xFFFFFF09"
    :label "Label 25 JUMP(UP) FFFFFF09 FFFFFFFF"
    :command "jump" 
    :width "150 px" 
    :height "150 px" 
    :left "Kick 4 px"
    :bottom "Shoot 4 px"}

   {:id "Down"
    :class "Button"
    :texture "Color FFFFFF09"
    :label "Label 25 DOWN FFFFFF09 FFFFFFFF"
    :command "down"
    :width "150 px"
    :height "150 px"
    :right "Edge 4 px"
    :bottom "Edge 4 px"}

   {:id "Right"
    :class "Button"
    :texture "Color FFFFFF09"
    :label "Label 25 RIGHT FFFFFF09 FFFFFFFF"
    :command "right"
    :width "150 px"
    :height "150 px"
    :right "Edge 4 px"
    :bottom "Down 4 px"}

   {:id "Left"
    :class "Button"
    :texture "Color FFFFFF09"
    :label "Label 25 LEFT FFFFFF09 FFFFFFFF"
    :command "left"
    :width "150 px"
    :height "150 px"
    :right "Right 4 px"
    :bottom "Down 4 px"}])


(def menu
  ;;
  ;;    BRAWL
  ;;    Continue
  ;;    New Game
  ;;    Options
  ;;    Donate
  ;;    Exit
  ;;
  [{:id "Menu"
    :class ""
    :texture "Color 0x000000EE"
    :width "100 %" 
    :height "100 %" 
    :subviews
    
    [{:id "Brawl"
      :class "Button"
      :texture "Color 0xFFFFFF0F"
      :label "Label 50 BRAWL 00000000 FFFFFFFF"
      :width "400 px" 
      :height "140 px" 
      :bottom "Continue 8 px" 
      :center-x "Edge 50 %"
      :subviews [{:id "By"
                  :class "Button"
                  :texture "Color 0x00000000"
                  :label "Label 15 ByMilanToth 00000000 FFFFFFFF"
                  :width "150 px" 
                  :height "40 px" 
                  :right "Edge 4 px" 
                  :bottom "Edge 4 px"}]}

     {:id "Continue"
      :class "Button"
      :command "continue"
      :texture "Color 0xFFFFFF0F"
      :label "Label 25 Continue 00000000 FFFFFFFF"
      :width "350 px" 
      :height "50 px" 
      :center-y "Edge 40 %" 
      :center-x "Edge 50 %"}

     {:id "NewGame"
      :class "Button"
      :command "new-game"
      :texture "Color 0xFFFFFF0F"
      :label "Label 25 NewGame 00000000 FFFFFFFF"
      :width "350 px" 
      :height "50 px" 
      :top "Continue 4 px" 
      :center-x "Edge 50 %"}

     {:id "Options"
      :class "Button"
      :command "options"
      :texture "Color 0xFFFFFF0F"
      :label "Label 25 Options 00000000 FFFFFFFF"
      :width "350 px" 
      :height "50 px" 
      :top "NewGame 4 px" 
      :center-x "Edge 50 %"}

     {:id "Donate"
      :class "Button"
      :command "donate"
      :texture "Color 0xFFFFFF0F"
      :label "Label 25 Donate 00000000 FFFFFFFF"
      :width "350 px" 
      :height "50 px" 
      :top "Options 4 px" 
      :center-x "Edge 50 %"}

     {:id "Exit"
      :class "Button"
      :command "exit"
      :texture "Color 0xFFFFFF0F"
      :label "Label 25 Exit 00000000 FFFFFFFF"
      :width "350 px" 
      :height "50 px" 
      :top "Donate 4 px" 
      :center-x "Edge 50 %"}]}])


(def options
  ;;
  ;;    BRAWL
  ;;    Continue
  ;;    New Game
  ;;    Options
  ;;    Donate
  ;;    Exit
  ;;
  [{:id "Options"
    :class ""
    :texture "Color 0x000000EE"
    :width "100 %" 
    :height "100 %" 
    :subviews
    
    [{:id "Music"
      :class "Slider"
      :command "set music volume"
      :texture "Color 0xFFFFFF0F"
      :label "Label 25 MusicVolume 00000000 FFFFFFFF"
      :width "350 px" 
      :height "50 px" 
      :center-y "Edge 40 %" 
      :center-x "Edge 50 %"}

     {:id "Sound"
      :class "Slider"
      :command "set sound volume"
      :texture "Color 0xFFFFFF0F"
      :label "Label 25 SoundVolume 00000000 FFFFFFFF"
      :width "350 px" 
      :height "50 px" 
      :top "Music 4 px" 
      :center-x "Edge 50 %"}

     {:id "Physics"
      :class "Button"
      :command "show physics"
      :texture "Color 0xFFFFFF0F"
      :label "Label 25 Show/HidePhysics 00000000 FFFFFFFF"
      :width "350 px" 
      :height "50 px" 
      :top "Sound 4 px" 
      :center-x "Edge 50 %"}

     {:id "Fullscreen"
      :class "Button"
      :command "donate"
      :texture "Color 0xFFFFFF0F"
      :label "Label 25 ToggleFullscreen 00000000 FFFFFFFF"
      :width "350 px" 
      :height "50 px" 
      :top "Physics 4 px" 
      :center-x "Edge 50 %"}

     {:id "OptBack"
      :class "Button"
      :command "options back"
      :texture "Color 0xFFFFFF0F"
      :label "Label 25 Back 00000000 FFFFFFFF"
      :width "350 px" 
      :height "50 px" 
      :top "Fullscreen 4 px" 
      :center-x "Edge 50 %"}]}])


(def info
;;|                Menu|
;;|     Sliders        |
;;|                    |
;;|     Randomize      |
;;|     StartGame      |
  [{:id "Info"
    :class ""
    :texture "Color 0xFF0000AA"
    :width "100 %" 
    :height "100 %" 
    :subviews
    
    [{:id "Reason"
      :class "Slider"
      :texture "Color 0xFF000055"
      :label "Label 30 HealthProblems 00000000 FFFFFFFF"
      :width "100 %" 
      :height "50 px" 
      :top "Edge 50 px" 
      :right "Edge 0 px"}

     {:id "Message"
      :class "Slider"
      :texture "Color 0x00FF0055"
      :label "Label 30 LOADING 00000000 FFFFFFFF"
      :width "500 px" 
      :height "90 px" 
      :center-y "Edge 50 %" 
      :center-x "Edge 50 %"}

     {:id "Restart"
      :class "Button"
      :command "restart level"
      :texture "Color 0xFFFFFF55"
      :label "Label 25 Restart 00000000 FFFFFFFF"
      :width "500 px" 
      :height "50 px" 
      :bottom "Edge 50 px" 
      :center-x "Edge 50 %"}]}])
