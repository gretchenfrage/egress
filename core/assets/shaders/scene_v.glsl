#version 120

attribute vec3 a_position;
attribute vec4 a_color;
attribute vec2 a_texCoord0;

uniform mat4 u_worldTrans;
uniform mat4 u_projViewTrans;
uniform mat4 u_shadowProjViewTrans;

varying vec2 v_texCoord0;
varying vec4 v_color;
varying vec4 v_shadowCoord;

void main() {
    mat4 biasMatrix = mat4(
        0.5, 0.0, 0.0, 0.0,
        0.0, 0.5, 0.0, 0.0,
        0.0, 0.0, 0.5, 0.0,
        0.5, 0.5, 0.5, 1.0
    );

    v_texCoord0 = a_texCoord0;
    v_color = a_color;

    gl_Position = u_projViewTrans * u_worldTrans * vec4(a_position, 1.0);
    v_shadowCoord = biasMatrix * u_shadowProjViewTrans * u_worldTrans * vec4(a_position, 1.0);
}
