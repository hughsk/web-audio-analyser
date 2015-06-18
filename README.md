# web-audio-analyser [![experimental](http://hughsk.github.io/stability-badges/dist/experimental.svg)](http://github.com/hughsk/stability-badges) #

A thin wrapper around the Web Audio API that lets you take some audio and get
its waveform/frequency data in return.

**Note:** If you are streaming from SoundCloud or a similar API, you may need to set up CORS on your Audio source like so:

```js
var audio = new Audio()
audio.crossOrigin = 'Anonymous'
audio.src = url
``` 

[![web-audio-analyser](https://nodei.co/npm/web-audio-analyser.png?mini=true)](https://nodei.co/npm/web-audio-analyser)

## Usage ##

#### `analyser = require('web-audio-analyser')(audio[, ctx][, opts])` ####

Takes some form of `audio` as the first argument. This may be one of the
following:

* An `<audio>` element.
* A `MediaStream` object created by calling `getUserMedia`.
* Any kind of `AudioSourceNode`.

Optionally, you can pass in your own `AudioContext` instance too. **Note** there
may only be one instance of this per page, and if not supplied one will be
created for you.

You can also use the following options:

* `stereo`: retrieve separate data from the left and right audio channels.
* `audible`: whether or not to play the audio you're analysing. Defaults to
  `true`, but set this to `false` to disable. You can use `analyser.output`
  to pass the resulting output elsewhere.


#### `analyser.waveform([ui8array][, channel])` ####

Copies the audio's current time-domain data into a `Uint8Array`. If you don't
pass your own in, one will be created for you: this will be reused, so it's
safe to call in an animation loop.

If you've enabled the `stereo` option, you should pass `channel` as either
`0` or `1` to retrieve output from the left and right channels respectively.

#### `analyser.frequencies([ui8array][, channel])` ####

Much like `analyser.waveform`, with the exception of copying the audio's
currrent frequency data into a `Uint8Array`.
