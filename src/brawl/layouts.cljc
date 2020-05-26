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

  {:id "Sliders"
   :texture {:type "Color" :color 0x00000000}
   :width "453 px"
   :height "101 px"
   :center-x "Edge 50 %"
   :center-y "Edge 10 %"
   :subviews 

   [{:id "Hitpower"
     :class "Slider"
     :texture {:type "Color" :color 0x555555FF}
     :label {:type "Label" :text "Hitpower" :size 25 :color 0xFFFFFFFF}
     :width "150 px"
     :height "50 px"
     :command "set-hitpower"}

    {:id "Height"
     :class "Slider"
     :texture {:type "Color" :color 0x555555FF}
     :label {:type "Label" :text "Height" :size 25 :color 0xFFFFFFFF}
     :width "150 px"
     :height "50 px"
     :center-x "Edge 50 %"
     :command "set-heivght"}
    
    {:id "Stamina"
     :class "Slider"
     :texture {:type "Color" :color 0x555555FF}
     :label {:type "Label" :text "Stamina" :size 25 :color 0xFFFFFFFF}
     :width "150 px"
     :height "50 px"
     :right "Edge 0 px"
     :command "set-stamina"}
    
    {:id "Hitrate"
     :class "Slider"
     :texture {:type "Color" :color 0x555555FF}
     :label {:type "Label" :text "Hitrate" :size 25 :color 0xFFFFFFFF}
     :width "150 px"
     :height "50 px"
     :bottom "Edge 0 px"
     :command "set-hitrate"}
    
    {:id "Speed"
     :class "Slider"
     :texture {:type "Color" :color 0x555555FF}
     :label {:type "Label" :text "Speed" :size 25 :color 0xFFFFFFFF}
     :width "150 px"
     :height "50 px"
     :right "Edge 0 px"
     :bottom "Edge 0 px"
     :command "set-speed"}]})
  

(def generator
;;|                Menu|
;;|     Sliders        |
;;|                    |
;;|     Randomize      |
;;|     StartGame      |
  {:id "Generator"
   :class ""
   :texture {:type "Empty"}
   :width "100 %" 
   :height "100 %" 
   :subviews
   
   [{:id "Menu"
     :class "Button"
     :texture {:type "Color" :color 0xFFFFFF09}
     :label {:type "Label" :text "Menu" :size 25 :color 0xFFFFFFFF :background 0xFFFFFF11}
     :command "show-menu" 
     :width "150 px" 
     :height "50 px" 
     :top "Edge 0 px" 
     :right "Edge 0 px"}

    sliders

    {:id "Randomize"
     :class "Button" 
     :texture {:type "Color" :color 0xFFFFFF19}
     :label {:type "Label" :text "Randomize" :size 25 :color 0xFFFFFFFF :background 0xFFFFFF11}
     :command "randomize" 
     :width "200 px" 
     :height "50 px" 
     :center-x "Edge 50 %"
     :bottom "StartGame 10 px"}
    
    {:id "StartGame"
     :class "Button"
     :texture {:type "Color" :color 0xDD0000FF}
     :label {:type "Label" :text "Startgame" :size 25 :color 0xFFFFFFFF :background 0xFFFFFF11}
     :command "start-game" 
     :width "200 px" 
     :height "50 px"
     :center-x "Edge 50 %"
     :bottom "Edge 50 px"}]})


(def hud
;;                                      Menu|
;;                                          |
;;Punch                                     |
;;Block Jump                      Left Right|
;;Kick  Shoot   Health Bullets Power    Down|
  {:id "Hud"
   :class ""
   :texture {:type "Empty"}
   :width "100 %" 
   :height "100 %" 
   :subviews
   
   [{:id "Menu"
     :class "Button"
     :texture {:type "Color" :color 0xFFFFFF19}
     :label {:type "Label" :text "Menu" :size 25 :color 0xFFFFFFFF :background 0xFFFFFF11}
     :command "show-menu" 
     :width "150 px" 
     :height "50 px" 
     :top "Edge 0 px" 
     :right "Edge 0 px"}

    {:id "Kick"
     :class "Button"
     :texture {:type "Color" :color 0xFFFFFF19}
     :label {:type "Label" :text "KICK (S)" :size 25 :color 0xFFFFFFFF :background 0xFFFFFF11}
     :command "kick"
     :width "150 px"
     :height "150 px"
     :left "Edge 4 px"
     :bottom "Edge 4 px"}

    {:id "Block"
     :class "Button" 
     :texture {:type "Color" :color 0xFFFFFF19}
     :label {:type "Label" :text "BLOCK (D)" :size 25 :color 0xFFFFFFFF :background 0xFFFFFF11}
     :command "block" 
     :width "150 px" 
     :height "150 px" 
     :left "Edge 4 px"
     :bottom "Kick 4 px"}
    
    {:id "Punch"
     :class "Button" 
     :texture {:type "Color" :color 0xFFFFFF19}
     :label {:type "Label" :text "PUNCH (F)" :size 25 :color 0xFFFFFFFF :background 0xFFFFFF11}
     :command "punch" 
     :width "150 px" 
     :height "150 px" 
     :left "Edge 4 px"
     :bottom "Block 4 px"}

    {:id "Shoot"
     :class "Button" 
     :texture {:type "Color" :color 0xFFFFFF19}
     :label {:type "Label" :text "SHOOT (V)" :size 25 :color 0xFFFFFFFF :background 0xFFFFFF11}
     :command "shoot" 
     :width "150 px" 
     :height "150 px" 
     :left "Kick 4 px"
     :bottom "Edge 4 px"}
    
    {:id "Jump"
     :class "Button" 
     :texture {:type "Color" :color 0xFFFFFF19}
     :label {:type "Label" :text "JUMP" :size 25 :color 0xFFFFFFFF :background 0xFFFFFF11}
     :command "jump" 
     :width "150 px" 
     :height "150 px" 
     :left "Kick 4 px"
     :bottom "Shoot 4 px"}

    {:id "Down"
     :class "Button"
     :texture {:type "Color" :color 0xFFFFFF19}
     :label {:type "Label" :text "DOWN" :size 25 :color 0xFFFFFFFF :background 0xFFFFFF11}
     :command "down"
     :width "150 px"
     :height "150 px"
     :right "Edge 4 px"
     :bottom "Edge 4 px"}

    {:id "Right"
     :class "Button"
     :texture {:type "Color" :color 0xFFFFFF19}
     :label {:type "Label" :text "RIGHT" :size 25 :color 0xFFFFFFFF :background 0xFFFFFF11}
     :command "right"
     :width "150 px"
     :height "150 px"
     :right "Edge 4 px"
     :bottom "Down 4 px"}

    {:id "Left"
     :class "Button"
     :texture {:type "Color" :color 0xFFFFFF19}
     :label {:type "Label" :text "LEFT" :size 25 :color 0xFFFFFFFF :background 0xFFFFFF11}
     :command "left"
     :width "150 px"
     :height "150 px"
     :right "Right 4 px"
     :bottom "Down 4 px"}]})


(def menu
  ;;
  ;;    BRAWL
  ;;    Continue
  ;;    New Game
  ;;    Options
  ;;    Donate
  ;;    Exit
  ;;
  {:id "Menu"
   :class ""
   :texture {:type "Color" :color 0x000000EE}
   :width "100 %" 
   :height "100 %" 
   :subviews
   
   [{:id "Brawl"
     :class "Button"
     :texture {:type "Color" :color 0xFFFFFF0F}
     :label {:type "Label" :text "BRAWL" :size 50 :color 0xFFFFFFFF}
     :width "400 px" 
     :height "140 px" 
     :bottom "Continue 8 px" 
     :center-x "Edge 50 %"
     :subviews [{:id "By"
                 :class "Button"
                 :texture {:type "Color" :color 0x00000000}
                 :label {:type "Label" :text "By Milan Toth" :size 15 :color 0xFFFFFFFF}
                 :width "150 px" 
                 :height "40 px" 
                 :right "Edge 4 px" 
                 :bottom "Edge 4 px"}]}

    {:id "Continue"
     :class "Button"
     :command "continue"
     :texture {:type "Color" :color 0xFFFFFF0F}
     :label {:type "Label" :text "Continue" :size 25 :color 0xFFFFFFFF}
     :width "350 px" 
     :height "50 px" 
     :center-y "Edge 40 %" 
     :center-x "Edge 50 %"}

    {:id "NewGame"
     :class "Button"
     :command "new-game"
     :texture {:type "Color" :color 0xFFFFFF0F}
     :label {:type "Label" :text "New Game" :size 25 :color 0xFFFFFFFF}
     :width "350 px" 
     :height "50 px" 
     :top "Continue 4 px" 
     :center-x "Edge 50 %"}

    {:id "Options"
     :class "Button"
     :command "options"
     :texture {:type "Color" :color 0xFFFFFF0F}
     :label {:type "Label" :text "Options" :size 25 :color 0xFFFFFFFF}
     :width "350 px" 
     :height "50 px" 
     :top "NewGame 4 px" 
     :center-x "Edge 50 %"}

    {:id "Donate"
     :class "Button"
     :command "donate"
     :texture {:type "Color" :color 0xFFFFFF0F}
     :label {:type "Label" :text "Donate" :size 25 :color 0xFFFFFFFF}
     :width "350 px" 
     :height "50 px" 
     :top "Options 4 px" 
     :center-x "Edge 50 %"}]})


(def options
  ;;
  ;;    BRAWL
  ;;    Continue
  ;;    New Game
  ;;    Options
  ;;    Donate
  ;;    Exit
  ;;
  {:id "Options"
   :class ""
   :texture {:type "Color" :color 0x000000EE}
   :width "100 %" 
   :height "100 %" 
   :subviews
   
   [{:id "Music"
     :class "Slider"
     :command "set music volume"
     :texture {:type "Color" :color 0xFFFFFF0F}
     :label {:type "Label" :text "Music Volume" :size 25 :color 0xFFFFFFFF}
     :width "350 px" 
     :height "50 px" 
     :center-y "Edge 40 %" 
     :center-x "Edge 50 %"}

    {:id "Effects"
     :class "Slider"
     :command "set sound volume"
     :texture {:type "Color" :color 0xFFFFFF0F}
     :label {:type "Label" :text "Sound Volume" :size 25 :color 0xFFFFFFFF}
     :width "350 px" 
     :height "50 px" 
     :top "Music 4 px" 
     :center-x "Edge 50 %"}

    {:id "Physics"
     :class "Button"
     :command "show physics"
     :texture {:type "Color" :color 0xFFFFFF0F}
     :label {:type "Label" :text "Show Physics" :size 25 :color 0xFFFFFFFF}
     :width "350 px" 
     :height "50 px" 
     :top "Sound 4 px" 
     :center-x "Edge 50 %"}

    {:id "Fullscreen"
     :class "Button"
     :command "donate"
     :texture {:type "Color" :color 0xFFFFFF0F}
     :label {:type "Label" :text "Fullscreen" :size 25 :color 0xFFFFFFFF}
     :width "350 px" 
     :height "50 px" 
     :top "Physics 4 px" 
     :center-x "Edge 50 %"}

    {:id "OptBack"
     :class "Button"
     :command "options back"
     :texture {:type "Color" :color 0xFFFFFF0F}
     :label {:type "Label" :text "Back" :size 25 :color 0xFFFFFFFF}
     :width "350 px" 
     :height "50 px" 
     :top "Fullscreen 4 px" 
     :center-x "Edge 50 %"}]})


(def info
  {:id "Info"
   :class ""
   :texture {:type "Color" :color 0x00AA00AA}
   :width "100 %" 
   :height "100 %" 
   :subviews
   
   [{:id "Message"
     :class ""
     :texture {:type "Color" :color 0x00FF0055}
     :width "100 %" 
     :height "90 px" 
     :center-y "Edge 50 %" 
     :center-x "Edge 50 %"
     :subviews

     [{:id "Text"
      :class ""
      :texture {:type "Label" :text "Loading" :size 30 :color 0xFFFFFFFF :background 0xFFFFFF00}
      :width "400 px" 
      :height "90 px" 
      :center-y "Edge 50 %" 
      :center-x "Edge 50 %"}]}]})


(def failure
  {:id "Failure"
   :class ""
   :texture {:type "Color" :color 0xFF0000AA}
   :width "100 %" 
   :height "100 %" 
   :subviews
   
   [{:id "Reason"
     :class ""
     :texture {:type "Color" :color 0xFFFFFF55}
     :width "100 %" 
     :height "50 px" 
     :top "Edge 50 px" 
     :center-x "Edge 50 %"
     :subviews
     
     [{:id "Text"
       :class ""
       :texture {:type "Label" :text "Health Problems" :size 30 :color 0xFFFFFFFF :background 0xFFFFFF00}
       :width "400 px" 
       :height "50 px" 
       :center-y "Edge 50 %" 
       :center-x "Edge 50 %"}]}
    
    {:id "Message"
     :class ""
     :texture {:type "Color" :color 0xFF000055}
     :width "100 %" 
     :height "90 px" 
     :center-y "Edge 50 %" 
     :center-x "Edge 50 %"
     :subviews
     
     [{:id "MsgText"
       :class ""
       :texture {:type "Label" :text "Wasted" :size 30 :color 0xFFFFFFFF :background 0xFFFFFF00}
       :width "400 px" 
       :height "90 px" 
       :center-y "Edge 50 %" 
       :center-x "Edge 50 %"}]
     }

    {:id "Restart"
     :class ""
     :texture {:type "Color" :color 0x00FF00AA}
     :width "100 %" 
     :height "50 px" 
     :bottom "Edge 50 px" 
     :center-x "Edge 50 %"
     :subviews

     [{:id "Button"
       :class "Button"
       :command "restart level"
       :texture {:type "Color" :color 0x00000000}
       :label {:type "Label" :text "Continue" :size 30 :color 0xFFFFFFFF :background 0xFFFFFF00}
       :width "200 px" 
       :height "50 px" 
       :center-y "Edge 50 %" 
       :center-x "Edge 50 %"}]}]})
