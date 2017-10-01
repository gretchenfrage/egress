#version 150

layout(triangles) in;
layout(triangle_strip, max_vertices = 3) out;

// inputs
in vec3 v_pos[3];

in vec2 v_texCoord0[3];
in vec4 v_color[3];
in vec4 v_shadowCoord[3];

in vec3 v_normalWorldSpace[3];
in vec3 v_normalCamSpace[3];
in vec3 v_lightDirCamSpace[3];
in vec3 v_camDirCamSpace[3];

// outputs
out vec3 f_pos;

out vec2 f_texCoord0;
out vec4 f_color;
out vec4 f_shadowCoord;

out vec3 f_normalWorldSpace;
out vec3 f_normalCamSpace;
out vec3 f_lightDirCamSpace;
out vec3 f_camDirCamSpace;

void main() {
    for (int i = 0; i < 3; ++i) {
        f_pos = v_pos[i];

        f_texCoord0 = v_texCoord0[i];
        f_color = v_color[i];
        f_shadowCoord = v_shadowCoord[i];

        f_normalWorldSpace = v_normalWorldSpace[i];
        f_normalCamSpace = v_normalCamSpace[i];
        f_lightDirCamSpace = v_lightDirCamSpace[i];
        f_camDirCamSpace = v_camDirCamSpace[i];

        gl_Position = gl_in[i].gl_Position;

        EmitVertex();
    }
    EndPrimitive();
}
