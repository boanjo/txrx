txrx
====

Arduino UNO & Raspberry PI  based 433MHz RX/TX for controlling Switches &amp; Sensors. An tellstick or rfxcom replacement if you want.

- The UNO handles the real-time critical parts to encÃode/decod theÂ ASKsignals

- The raspberry PI is the interface to control the UNO (to develop/build/upload new UNO firmware using the ino build system). It is also used together with the homeautomation project which runs a web server to display sensors, turn on/off/listen electrical power outlet devices. 

Hardware
Raspberry PI  
433MHz superhetroÂdyne transmitter/receive
LEDs (for visual feedback, so not really needed)