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
    :texture "Label 30 Hitpower 000000AA FFFFFFFF"
    :width "150 px"
    :height "50 px"
    :command "set-hitpower"}

   {:id "Height"
    :class "Slider"
    :texture "Label 30 Height 000000AA FFFFFFFF"
    :width "150 px"
    :height "50 px"
    :center-x "Edge 50 %"
    :command "set-height"}
   
   {:id "Stamina"
    :class "Slider"
    :texture "Label 30 Stamina 000000AA FFFFFFFF"
    :width "150 px"
    :height "50 px"
    :right "Edge 0 px"
    :command "set-stamina"}
   
   {:id "Hitrate"
    :class "Slider"
    :texture "Label 30 Hitrate 000000AA FFFFFFFF"
    :width "150 px"
    :height "50 px"
    :bottom "Edge 0 px"
    :command "set-hitrate"}
   
   {:id "Speed"
    :class "Slider"
    :texture "Label 30 Speed 000000AA FFFFFFFF"
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
    :texture "Label 30 Menu FFFFFF11 FFFFFFFF"
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
    :texture "Label 30 Randomize FFFFFF11 FFFFFFFF"
    :command "randomize" 
    :width "200 px" 
    :height "50 px" 
    :center-x "Edge 50 %"
    :bottom "StartGame 10 px"}
   
   {:id "StartGame"
    :class "Button" 
    :texture "Label 30 Startgame FFFFFF11 FFFFFFFF"
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
"
X CLIndicator TEXP BCFFFFFF55 FCFFFFFFFF TA0 LA0 WI150 HE50
M CLButton TEMenu BCFF00FF55 FCFFFFFFFF TA0 RA0 WI150 HE50 COShowMenu
H CLIndicator TEHealth BCFF000055 FCFFFFFFFF BA0 RAS WI200 HE50
S CLLabel TEBullets BC00FF0055 FCFFFFFFFF BA0 HA0 WI50 HE50
P CLIndicator TEPower BC0000FF55 FCFFFFFFFF BA0 LAS WI200 HE50
B CLButton TEBlock BCFFFFFF55 FCFFFFFFFF BAK LA0 WI100 HE100
K CLButton TEKick BCFFFFFF55 FCFFFFFFFF BA0 LA0 WI100 HE100
C CLButton TEPunch BCFFFFFF55 FCFFFFFFFF BAB LA0 WI100 HE100
G CLButton TEGun BCFFFFFF55 FCFFFFFFFF BA0 LAK WI100 HE100
J CLButton TEJump BCFFFFFF55 FCFFFFFFFF BAG LAB WI100 HE100
L CLButton TELeft BCFFFFFF55 FCFFFFFFFF BA0 RAR WI100 HE100
R CLButton TERight BCFFFFFF55 FCFFFFFFFF BAD RA0 WI100 HE100
D CLButton TEDown BCFFFFFF55 FCFFFFFFFF BA0 RA0 WI100 HE100
E CLDebug TA0 LA0 WI300 HE300")
