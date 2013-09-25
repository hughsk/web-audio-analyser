# web-audio-analyser [![experimental](http://hughsk.github.io/stability-badges/dist/experimental.svg)](http://github.com/hughsk/stability-badges) #

A thin wrapper around the Web Audio API that takes an `<audio>` element and
gives you its waveform/frequency data in return.

[![web-audio-analyser](https://nodei.co/npm/web-audio-analyser.png?mini=true)](https://nodei.co/npm/web-audio-analyser)

## Usage ##

#### `analyser = require('web-audio-analyser')(audio[, ctx])` ####

Takes an `<audio>` element as its first argument - optionally, you can pass
your own `AudioContext` instance too.

#### `analyser.waveform([ui8array])` ####

Copies the audio's current time-domain data into a `Uint8Array`. If you don't
pass your own in, one will be created for you: this will be reused, so it's
safe to call in an animation loop.

#### `analyser.frequencies([ui8array])` ####

Much like `analyser.waveform`, with the exception of copying the audio's
currrent frequency data into a `Uint8Array`.
