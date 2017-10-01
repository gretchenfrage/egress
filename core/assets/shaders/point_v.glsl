#version 150

in vec3 a_position;
in vec4 a_color;

uniform mat4 u_worldTrans;
uniform mat4 u_projViewTrans;

out vec4 v_color;

void main() {
    v_color = a_color;
    gl_Position = u_projViewTrans * u_worldTrans * vec4(a_position, 1.0);
}
