var song_url

var fs           = require('fs')
var analyse      = require('./')
var createCamera = require('game-shell-orbit-camera')
var createBuffer = require('gl-buffer')
var createShader = require('gl-shader')
var GLMatrix     = require('gl-matrix')
var createVAO    = require('gl-vao')
var glnow        = require('gl-now')
var mat4         = GLMatrix.mat4
var shell

require('soundcloud-badge')({
    client_id: 'ded451c6d8f9ff1c62f72523f49dab68'
  , song: 'https://soundcloud.com/moon_music/hydrogen'
  , dark: false
  , getFonts: true
}, function(err, src, json, div) {
  if (err) throw err
  song_url = src

  shell = glnow({ clearColor: [0, 0, 0, 1] })
  shell.on('gl-render', render)
  shell.on('gl-init', init)
  camera = createCamera(shell)
})

var lines = new Float32Array(1024 * 3)
var sides = new Float32Array(1024 * 3)
var analyser
var shader
var mesh
var gl

function init() {
  var audio  = new Audio
  audio.crossOrigin = 'Anonymous'
  audio.src = song_url
  audio.loop = true
  audio.addEventListener('canplay', function() {
    console.log('playing!')
    analyser = analyse(audio, { audible: true, stereo: false })
    audio.play()
  })

  gl = shell.gl
  mesh = createMesh(gl)
  shader = createShader(gl
    , fs.readFileSync(__dirname + '/shaders/basic.vert')
    , fs.readFileSync(__dirname + '/shaders/basic.frag')
  )
}

var projection = new Float32Array(16)
var model = new Float32Array(16)

function render() {
  if (analyser) {
    var waveform = analyser.waveform()
    var size = waveform.length
    for (var i = 0, n = 0; i < size; i += 1, n += 3) {
      lines[n  ] = i / size * 2 - 1
      lines[n+1] = waveform[i] - 128
      lines[n+2] = 0
      if (i) {
        if (i+1 < size) sides[n] = (waveform[i-1] - 128) / 0.175
        else            sides[n] = (waveform[i] - 128) / 0.175
      } else {
        sides[n] = waveform[0] - 128
      }
    }
  }

  var view = camera.view()
  mat4.perspective(projection, 0.25*Math.PI, shell.width/shell.height, 0.05, 1000)
  mat4.identity(model)

  gl.enable(gl.CULL_FACE)
  gl.enable(gl.DEPTH_TEST)

  shader.bind()
  shader.attributes.position.location = 0
  shader.attributes.sides.location = 1

  shader.uniforms.projection = projection
  shader.uniforms.model = model
  shader.uniforms.view = view

  mesh.buffer.update(lines)
  mesh.sides.update(sides)
  mesh.vao.bind()
  gl.lineWidth(4)
  gl.drawArrays(gl.LINE_STRIP, 0, mesh.length)
  mesh.vao.unbind()
}

function createMesh(gl) {
  var vertBuffer = createBuffer(gl, gl.ARRAY_BUFFER, lines)
  var sideBuffer = createBuffer(gl, gl.ARRAY_BUFFER, sides)
  var vao = createVAO(gl, null, [{
    buffer: vertBuffer
    , type: gl.FLOAT
    , size: 3
    , offset: 0
    , stride: 0
    , normalized: false
  }, {
    buffer: sideBuffer
    , type: gl.FLOAT
    , size: 3
    , offset: 0
    , stride: 0
    , normalized: false
  }])

  return {
      vao: vao
    , length: vertBuffer.length / 3
    , buffer: vertBuffer
    , sides: sideBuffer
  }
}
