#version 120

attribute vec3 a_position;
attribute vec2 a_texCoord0;

uniform mat4 u_MVP;

varying vec2 v_texCoord0;

void main() {
    gl_Position = u_MVP * vec4(a_position, 1);
    v_texCoord0 = a_texCoord0;
}
