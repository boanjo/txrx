# txrx

Arduino UNO & Raspberry PI  based 433MHz RX/TX for controlling Switches &amp; Sensors. An tellstick or rfxcom replacement if you want. I have chosen to work only with Oregon Scientific sensors (present in most countries and good specs). 

If you want to use the txrx as a part of your homeautomation project i recommend you to clone the https://github.com/epkboan/homeautomation project! Then the txrx will be automatically downloaded as dependency.

- The UNO handles the real-time critical parts to encode/decode the ASK signals

- The raspberry PI is the interface to control the UNO (to develop/build/upload new UNO firmware using the ino build system). It is also used together with the https://github.com/epkboan/homeautomation project which runs a web server to display sensors, turn on/off/listen electrical power outlet devices. But any device that can run Erlang and has a usb interface and network connection will do the trick (the PI is spot on).

## Sensors
* PCR800 (But if you want a tipping bucket rain gausge that actually shows the right amounts you have to build it yourself: https://github.com/epkboan/rain_sensor)
* THGR810 (temperature and humidity)

## Remote Controlled Outlets
* PROOVE (NEXA, KAKU all share similar protocols)

## txrx Hardware
* Raspberry PI (or equivalent)
* Arduino UNO (or equivalent)
* 433MHz superhetrodyne transmitter
* 433MHz superhetrodyne receiver
* LEDs and ~0,68k resistors(for visual feedback, so not really needed)

Below are some pictures from the prototyping, enjoy!

![Schematic of txrx hardware](https://github.com/epkboan/boanjo.github.io/blob/master/txrx_schem.png?raw=true "Schematic: txrx Hardware")



![Wiring of txrx hardware](https://github.com/epkboan/boanjo.github.io/blob/master/txrx_bb.png?raw=true "Breadboard: txrx Hardware")

### Screenshot of the homeautomation control center
![ss1](https://github.com/epkboan/boanjo.github.io/blob/master/homeautomation_1.JPG "The control centre")

