precision highp float;

attribute vec3 position;
attribute vec3 sides;

varying float tSide;

uniform mat4 projection;
uniform mat4 model;
uniform mat4 view;

void main() {
  tSide = abs(sides.x - position.y) * 0.01;
  gl_Position = projection * view * model * vec4(position.x * 1.5, position.y * 0.0015, position.z, 1.0);
}
