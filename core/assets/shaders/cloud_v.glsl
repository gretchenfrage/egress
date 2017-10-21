#version 150

in vec3 a_position;
in vec3 a_normal;

uniform mat4 u_worldTrans;
uniform mat4 u_viewTrans;
uniform mat4 u_projTrans;
uniform vec3 u_lightPos;

out vec3 v_pos;

out vec3 v_normalWorldSpace;
out vec3 v_normalCamSpace;
out vec3 v_lightDirCamSpace;
out vec3 v_eyeDirCamSpace;

mat4 mvp = u_projTrans * u_viewTrans * u_worldTrans;

void main() {
    // for basic passthrough
    v_pos = (u_worldTrans * vec4(a_position, 1)).xyz;
    gl_Position = mvp * vec4(a_position, 1.0);

    // for normal shading
    // only valid if world trans matrix does not scale the model
    v_normalWorldSpace = (u_worldTrans * vec4(a_normal, 0)).xyz;
    v_normalCamSpace = (u_viewTrans * u_worldTrans * vec4(a_normal, 0)).xyz;
    vec3 vertPosCamSpace = (u_viewTrans * u_worldTrans * vec4(a_position, 1)).xyz;
    vec3 lightPosCamSpace = (u_viewTrans * vec4(u_lightPos, 1)).xyz;
    v_eyeDirCamSpace = -vertPosCamSpace;
    v_lightDirCamSpace = lightPosCamSpace + v_eyeDirCamSpace;
}
