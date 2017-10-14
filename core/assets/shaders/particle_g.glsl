#version 150

layout(points) in;
layout(triangle_strip, max_vertices = 6) out;

in vec2 v_texCoord0[1];
in vec2 v_texCoord1[1];
in float v_size[1];
in vec4 v_color[1];

uniform mat4 u_P;

out vec2 f_texCoord0;
out vec4 f_color;

void main() {
    f_color = v_color[0];
    vec4 pos = gl_in[0].gl_Position;

    //  3 ____________ 2
    //   |            /|6
    //   |          /  |
    //   |        /    |
    //   |      /      |
    //   |    /        |
    //   |  /          |
    //  1|/____________|
    //    4             5

    // 1
    gl_Position = u_P * vec4(pos.xy + vec2(-0.5, -0.5) * v_size[0], pos.zw);
    f_texCoord0 = v_texCoord0[0];
    EmitVertex();
    gl_Position = u_P * vec4(pos.xy + vec2(+0.5, +0.5) * v_size[0], pos.zw);
    f_texCoord0 = v_texCoord1[0];
    EmitVertex();
    gl_Position = u_P * vec4(pos.xy + vec2(-0.5, +0.5) * v_size[0], pos.zw);
    f_texCoord0 = vec2(v_texCoord0[0].x, v_texCoord1[0].y);
    EmitVertex();
    EndPrimitive();

    gl_Position = u_P * vec4(pos.xy + vec2(-0.5, -0.5) * v_size[0], pos.zw);
    f_texCoord0 = v_texCoord0[0];
    EmitVertex();
    gl_Position = u_P * vec4(pos.xy + vec2(+0.5, -0.5) * v_size[0], pos.zw);
    f_texCoord0 = vec2(v_texCoord1[0].x, v_texCoord0[0].y);
    EmitVertex();
    gl_Position = u_P * vec4(pos.xy + vec2(+0.5, +0.5) * v_size[0], pos.zw);
    f_texCoord0 = v_texCoord1[0];
    EmitVertex();
    EndPrimitive();
/*
    gl_Position = gl_in[0].gl_Position + vec4(-v_size[0], -v_size[0], 0, 0);
    f_texCoord0 = v_texCoord0[0];
    EmitVertex();
    gl_Position = gl_in[0].gl_Position + vec4(+v_size[0], +v_size[0], 0, 0);
    f_texCoord0 = v_texCoord1[0];
    EmitVertex();
    gl_Position = gl_in[0].gl_Position + vec4(-v_size[0], +v_size[0], 0, 0);
    f_texCoord0 = vec2(v_texCoord0[0].x, v_texCoord1[0].y);
    EmitVertex();
    EndPrimitive();

    gl_Position = gl_in[0].gl_Position + vec4(-v_size[0], -v_size[0], 0, 0);
    f_texCoord0 = v_texCoord0[0];
    EmitVertex();
    gl_Position = gl_in[0].gl_Position + vec4(+v_size[0], -v_size[0], 0, 0);
    f_texCoord0 = vec2(v_texCoord1[0].x, v_texCoord0[0].y);
    EmitVertex();
    gl_Position = gl_in[0].gl_Position + vec4(+v_size[0], +v_size[0], 0, 0);
    f_texCoord0 = v_texCoord1[0];
    EmitVertex();
    EndPrimitive();
    */
}
