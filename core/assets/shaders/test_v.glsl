#version 120

attribute vec3 a_position;
attribute vec4 a_color;
attribute vec2 a_texCoord0;

uniform mat4 u_worldTrans;
uniform mat4 u_projViewTrans;

varying vec2 v_texCoord0;
varying vec4 v_color;

// scene vertex shader
void main() {
    v_texCoord0 = a_texCoord0;
    v_color = a_color;
    gl_Position = u_projViewTrans * u_worldTrans * vec4(a_position, 1.0);
}
