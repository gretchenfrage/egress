#version 120

attribute vec3 a_position;
attribute vec4 a_color;
attribute vec2 a_texCoord0;
attribute vec3 a_normal;

uniform mat4 u_worldTrans;
uniform mat4 u_viewTrans;
uniform mat4 u_projTrans;
uniform mat4 u_shadowProjViewTrans;
uniform vec3 u_lightPos;

varying vec3 v_pos;

varying vec2 v_texCoord0;
varying vec4 v_color;
varying vec4 v_shadowCoord;

varying vec3 v_normalCamSpace;
varying vec3 v_lightDirCamSpace;
varying vec3 v_camDirCamSpace;

void main() {
    // precomputations
    mat4 biasMatrix = mat4(
        0.5, 0.0, 0.0, 0.0,
        0.0, 0.5, 0.0, 0.0,
        0.0, 0.0, 0.5, 0.0,
        0.5, 0.5, 0.5, 1.0
    );

    mat4 mvp = u_projTrans * u_viewTrans * u_worldTrans;

    // for basic passthrough
    v_pos = (u_worldTrans * vec4(a_position, 1)).xyz;
    v_texCoord0 = a_texCoord0;
    v_color = a_color;
    gl_Position = mvp * vec4(a_position, 1.0);

    // for shadow mapping
    v_shadowCoord = biasMatrix * u_shadowProjViewTrans * u_worldTrans * vec4(a_position, 1.0);

    // for normal shading
    // only valid if world trans matrix does not scale the model
    v_normalCamSpace = (u_viewTrans * u_worldTrans * vec4(a_normal, 0)).xyz;
    vec3 vertPosCamSpace = (u_viewTrans * u_worldTrans * vec4(a_position, 1)).xyz;
    vec3 lightPosCamSpace = (u_viewTrans * vec4(u_lightPos, 1)).xyz;
    v_camDirCamSpace = -vertPosCamSpace;
    v_lightDirCamSpace = lightPosCamSpace + v_camDirCamSpace;


}
