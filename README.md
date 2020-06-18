# BRAWL

A physics based 2D side scrolling action game written in Clojurescript/WebGL.

[PLAY](https://milgra.github.io/cljs-brawl/index.html) ( Keys : ARROWS SPACE F D S )

## Story

You rent a tiny flat in an Eastern-European-style block of flats. You want to visit your girlfriend but you see gang members on the road. They take your money and food every time. Today you won't let this happen!!! 

## Development

The development of Brawl started in 2004 after I checked out Keith Peters' (bit-101.com) awesome inverse kinematic experiments written in flash.

I immediately imagined a side scroller game where actors can punch and kick each other and they react and fall very lifelike thanks to inverse kinematics.

The first prototype was ready in December 2004, I immediately uploaded it to gotoandplay.it, [it is still here](http://gotoandplay.it/_articles/2004/11/ik_engine.php).

Then in 2010 I picked up the project again and created a prototype for iOS. It remained a prototype until 2018 when I finally finished it in C/OpenGL and released it for iOS, Android, Mac, Windows, Linux and HTML5.

Then in 2020 I rewrote it in clojure/script and I'm so satisfied with the language that this will be the maintained/live version of the game.

## Clojure/Script

I was curious about two things with clojure :
- how do you structure a complex game like this in clojure ( physics simulation, ui/font rendering, ai, etc), in this super compact stateless lisp language?
- will it be fast enough? javascript is already slow, how will clojure slow down things?


modify mutliple data types from one data type - multiple maps instead of one reduce with complex functions
where to extract data
eric chachi virtual machine easy to port
clojureis like a virtual languageü runs on everything
chain small steps instead of doing something with a complicated reduce function

c - cljs code size comparison

### How to run/develop the game

Download/checkout the repo and start it up with :

```shadow-cljs watch app```

## Todo

* death if position is bigger than 3000
* hosszabb lab/kez utes
* hero halala utan nyugodjon meg az ellen
* harmadik palya lepcso alatt ongyilkossag
* pisztolyt elveszik
* improve gameplay
* killed by XY from names