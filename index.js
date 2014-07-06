var AudioContext = window.AudioContext || window.webkitAudioContext

module.exports = WebAudioAnalyser

function WebAudioAnalyser(audio, ctx) {
  if (!(this instanceof WebAudioAnalyser)) return new WebAudioAnalyser(audio, ctx)

  this.ctx = ctx = ctx || new AudioContext

  if (!(audio instanceof AudioNode)) {
    audio = audio instanceof Audio
      ? ctx.createMediaElementSource(audio)
      : ctx.createMediaStreamSource(audio)
  }

  this.analyser = ctx.createAnalyser()
  this.wavedata = null
  this.freqdata = null
  this.source   = audio

  this.source.connect(this.analyser)
  this.analyser.connect(ctx.destination)
}

WebAudioAnalyser.prototype.waveform = function(output) {
  if (!output) output = this.wavedata || (
    this.wavedata = new Uint8Array(this.analyser.frequencyBinCount)
  )
  this.analyser.getByteTimeDomainData(output)
  return output
}

WebAudioAnalyser.prototype.frequencies = function(output) {
  if (!output) output = this.freqdata || (
    this.freqdata = new Uint8Array(this.analyser.frequencyBinCount)
  )
  this.analyser.getByteFrequencyData(output)
  return output
}
