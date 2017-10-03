#version 150

layout(triangles) in;
layout(triangle_strip, max_vertices = 6) out;

out float f_isBackfaceColluder;

void main() {
    f_isBackfaceColluder = 0;
    gl_Position = gl_in[0].gl_Position;
    EmitVertex();
    gl_Position = gl_in[1].gl_Position;
    EmitVertex();
    gl_Position = gl_in[2].gl_Position;
    EmitVertex();
    EndPrimitive();

    f_isBackfaceColluder = 1;
    gl_Position = gl_in[2].gl_Position;
    EmitVertex();
    gl_Position = gl_in[1].gl_Position;
    EmitVertex();
    gl_Position = gl_in[0].gl_Position;
    EmitVertex();
    EndPrimitive();
}
