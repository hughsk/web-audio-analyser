precision highp float;

varying float tSide;

void main() {
  gl_FragColor = vec4(1.0 - tSide * 0.5, 1.0, 1.0 - tSide * 0.4, 1.0);
}
