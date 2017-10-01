#version 150

layout(points) in;
layout(triangle_strip, max_vertices = 3) out;

in vec4 v_color[1];

out vec4 f_color;

float d = 0.01;

void main() {
    f_color = v_color[0];

    gl_Position = gl_in[0].gl_Position;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + vec4(d, 0, 0, 0);
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + vec4(0, d, 0, 0);
    EmitVertex();

    EndPrimitive();
}
