#version 120

attribute vec3 a_position;
attribute vec4 a_color;
attribute vec2 a_texCoord0;

uniform mat4 u_worldTrans;
uniform mat4 u_projViewTrans;
uniform mat4 u_lightTrans;

varying vec2 v_texCoord0;
varying vec4 v_position;
varying vec4 v_positionLightTrans;

void main() {
    v_position = u_worldTrans * vec4(a_position, 1.0);
    v_positionLightTrans = u_lightTrans * v_position;
    gl_Position = u_projViewTrans * v_position;

    v_texCoord0 = a_texCoord0;
}
