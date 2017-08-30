#version 120

attribute vec3 a_position;
attribute vec4 a_color;
attribute vec2 a_texCoord0;

uniform mat4 u_worldTrans;
uniform mat4 u_projViewTrans;

varying vec4 v_position;

void main() {
    v_position = u_worldTrans * vec4(a_position, 1.0);
    gl_Position = u_projViewTrans * v_position;
}