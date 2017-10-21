#version 150

layout(triangles) in;
layout(triangle_strip, max_vertices = 6) out;

in vec3 v_pos[3];

in vec3 v_normalWorldSpace[3];
in vec3 v_normalCamSpace[3];
in vec3 v_lightDirCamSpace[3];
in vec3 v_eyeDirCamSpace[3];

uniform mat4 u_worldTrans;
uniform mat4 u_viewTrans;

out vec2 f_texCoord0;

out vec3 f_normalWorldSpace;
out vec3 f_normalCamSpace;
out vec3 f_lightDirCamSpace;
out vec3 f_eyeDirCamSpace;

void main() {
    f_texCoord0 =                   vec2(0, 0);
    f_normalWorldSpace = v_normalWorldSpace[0];
    f_normalCamSpace =     v_normalCamSpace[0];
    f_lightDirCamSpace = v_lightDirCamSpace[0];
    f_eyeDirCamSpace =     v_eyeDirCamSpace[0];
    gl_Position =                     gl_in[0].gl_Position;
    EmitVertex();

    f_texCoord0 =                   vec2(1, 0);
    f_normalWorldSpace = v_normalWorldSpace[1];
    f_normalCamSpace =     v_normalCamSpace[1];
    f_lightDirCamSpace = v_lightDirCamSpace[1];
    f_eyeDirCamSpace =     v_eyeDirCamSpace[1];
    gl_Position =                     gl_in[1].gl_Position;
    EmitVertex();

    f_texCoord0 =                   vec2(0, 1);
    f_normalWorldSpace = v_normalWorldSpace[2];
    f_normalCamSpace =     v_normalCamSpace[2];
    f_lightDirCamSpace = v_lightDirCamSpace[2];
    f_eyeDirCamSpace =     v_eyeDirCamSpace[2];
    gl_Position =                     gl_in[2].gl_Position;
    EmitVertex();

    EndPrimitive();
}
