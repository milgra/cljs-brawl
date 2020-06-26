# CLJS-BRAWL 0.9b

A physics based 2D side scrolling action game written in Clojurescript/WebGL.

If you find the game joyful or the source code useful please consider donating : [DONATE](www.paypal.me/milgra)

I'm looking for a job, so if you need a clojure programmer don't hesitate to contact me! :)

[PLAY THE GAME](https://milgra.github.io/cljs-brawl/index.html)

## Story

You rent a tiny flat in an Eastern-European-style block of flats. You want to visit your girlfriend but you see gang members on the road. They take your money and food every time. Today you won't let this happen!!!

## Development

The development of Brawl started in 2004 after I checked out Keith Peters' (bit-101.com) awesome inverse kinematics experiments written in flash.

I immediately imagined a side scroller game where actors can punch and kick each other and they react and fall very lifelike thanks to inverse kinematics.

The first prototype was ready in December 2004, I immediately uploaded it to gotoandplay.it, [it is still here](http://gotoandplay.it/_articles/2004/11/ik_engine.php).

Then in 2010 I picked up the project again and created a prototype for iOS. It remained a prototype until 2018 when I finally finished it in C/OpenGL and released it for iOS, Android, Mac, Windows, Linux and HTML5.

Then in 2020 I rewrote it in clojure/script and I'm so satisfied with the language that this will be the maintained/live version of the game.

## Clojure/Script

There are not many games written in clojure, especially not real-time action games with WebGL, UI/font rendering, mass point dynamics, inverse kinematics and skeletal animation but I had to make one. 

I was curious about two things :
- how do you structure a complex game like this in clojure, a super compact stateless lisp language?
- will it be fast enough? javascript is already slow, how will clojure slow down things?

Speed

ClojureScript is fast enough but converting clojure types to javascript types is really slow. Conversion should be avoided but if it's unavoidable you better use javascript types from the beginning. I had to do it with vertex collection into js arraybuffers.

Clojure code Compared to the C equivalent of the game :

C - 18500 lines, 740000 characters ( excluding the low level frameworks that are present in html5/javascript)

Clojure - 7000 lines,  210000 characters

The clojure code size is roughly the third of the C code size.

Most common pattern dilemmas

1. One complex reducer function or multiple simple threaded reducers ( aka transducers )?

   The second solution wins in clarity and readability altough it needs multiple iterations instead of one. If you also filter out items with every step then the speed should be almost identical.

2. Extract sub-data for a function or pass the whole dataset and let the function extract and re-insert data for itself

   It's hard to decide. In higher parts of a program the second version results in a cleaner code, caller functions are cleaner and callees are all right but in case of lower parts it is overkill, for example you don't want to extract x and y components of vectors from the whole state in an add-vector function.

## Frameworks used in the game

* cljs_webgl - for clojure-style opengl state handling
* tubax - for svg/xml parsing

## Related projects

The UI renderer and the Mass Point Dynamics Engine I wrote for the game are also open-sourced separately :

* [UI Renderer](https://github.com/milgra/cljs-gui)
* [MPD Engine](https://github.com/milgra/cljs-mpd)


## How to run/develop the game

* Install clojure and shadow-cljs
* Check out the repo
* Start the app with :

```shadow-cljs watch app```

## Todo

* add punhc/kick timeout to normalize maximum hit count
* increase enemy count/ai toughness after first walkthrough
* longer kick
* better ai proximity
* K.O. by XY on wasted screen