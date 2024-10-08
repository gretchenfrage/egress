#version 150

layout(triangles) in;
layout(triangle_strip, max_vertices = 6) out;

// inputs
in vec3 v_pos[3];

in vec2 v_texCoord0[3];
in vec4 v_color[3];
in vec4 v_shadowCoord[3];

in vec3 v_normalWorldSpace[3];
in vec3 v_normalCamSpace[3];
in vec3 v_lightDirCamSpace[3];
in vec3 v_eyeDirCamSpace[3];

// uniforms
uniform mat4 u_worldTrans;
uniform mat4 u_viewTrans;

// outputs
out vec3 f_pos;

out vec2 f_texCoord0;
out vec4 f_color;
out vec4 f_shadowCoord;

out vec3 f_normalWorldSpace;
out vec3 f_normalCamSpace;
out vec3 f_lightDirCamSpace;
out vec3 f_eyeDirCamSpace;

void main() {
    f_normalWorldSpace = normalize((v_normalWorldSpace[0] + v_normalWorldSpace[1] + v_normalWorldSpace[2]) / 3);
    f_normalCamSpace = normalize((v_normalCamSpace[0] + v_normalCamSpace[1] + v_normalCamSpace[2]) / 3);

    f_pos =                           v_pos[0];
    f_texCoord0 =               v_texCoord0[0];
    f_color =                 vec4(1, 0, 0, 1);
    f_shadowCoord =           v_shadowCoord[0];
    //f_normalWorldSpace = v_normalWorldSpace[0];
    //f_normalCamSpace =     v_normalCamSpace[0];
    f_lightDirCamSpace = v_lightDirCamSpace[0];
    f_eyeDirCamSpace =     v_eyeDirCamSpace[0];
    gl_Position =                     gl_in[0].gl_Position;
    EmitVertex();

    f_pos =                           v_pos[1];
    f_texCoord0 =               v_texCoord0[0] + vec2(0, 1.0 / 16.0);
    f_color =                 vec4(0, 1, 0, 1);
    f_shadowCoord =           v_shadowCoord[1];
    //f_normalWorldSpace = v_normalWorldSpace[1];
    //f_normalCamSpace =     v_normalCamSpace[1];
    f_lightDirCamSpace = v_lightDirCamSpace[1];
    f_eyeDirCamSpace =     v_eyeDirCamSpace[1];
    gl_Position =                     gl_in[1].gl_Position;
    EmitVertex();

    f_pos =                           v_pos[2];
    f_texCoord0 =               v_texCoord0[0] + vec2(1.0 / 16.0, 0);
    f_color =                 vec4(0, 0, 1, 1);
    f_shadowCoord =           v_shadowCoord[2];
    //f_normalWorldSpace = v_normalWorldSpace[2];
    //f_normalCamSpace =     v_normalCamSpace[2];
    f_lightDirCamSpace = v_lightDirCamSpace[2];
    f_eyeDirCamSpace =     v_eyeDirCamSpace[2];
    gl_Position =                     gl_in[2].gl_Position;
    EmitVertex();

    EndPrimitive();
}
