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
    :class "Indicator"
    :texture "Label 40 Hitpower 0xFFFFFFFF 0xFF00FF55"
    :width "200 px"
    :height "50 px"}
   
   {:id "Height"
    :class "Indicator"
    :texture "Label 40 Height 0xFFFFFFFF 0xFF00FF55"
    :width "200 px"
    :height "50 px"}
   
   {:id "Stamina"
    :class "Indicator"
    :texture "Label 40 Stamin 0xFFFFFFFF 0xFF00FF55"
    :width "200 px"
    :height "50 px"}
   
   {:id "Hitrate"
    :class "Indicator"
    :texture "Label 40 Hitrate 0xFFFFFFFF 0xFF00FF55"
    :width "200 px"
    :height "50 px"}
   
   {:id "Speed"
    :class "Indicator"
    :texture "Label 40 Speed 0xFFFFFFFF 0xFF00FF55"
    :width "200 px"
    :height "50 px"}])
  

(def generator
;;|                Menu|
;;|     Sliders        |
;;|                    |
;;|     Randomize      |
;;|     StartGame      |
  [{:id "Menu"
    :class "Button"
    :texture "Label 40 Menu 0xFFFFFFFF 0xFF00FF55"
    :command "ShowMenu" 
    :width "150 px" 
    :height "50 px" 
    :top "Edge 0 px" 
    :right "Edge 0 px"}

   {:id "Sliders"
    :texture "Color 0xFF00FF55"
    :width "450 px"
    :height "300 px"
    :center-x "Edge 50 %"
    :center-y "Edge 50 %"
    :subviews sliders}

   {:id "Randomize"
    :class "Button" 
    :texture "Label 40 Randomize 0xFFFFFFFF 0xFF00FF55"
    :command "Randomize" 
    :width "150 px" 
    :height "50 px" 
    :center-x "Edge 50 %"
    :bottom "StartGame 10 px"}
   
   {:id "StartGame"
    :class "Button" 
    :texture "Label 40 Startgame 0xFFFFFFFF 0xFF00FF55"
    :command "Startgame" 
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
