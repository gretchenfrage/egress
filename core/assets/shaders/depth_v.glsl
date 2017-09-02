#version 120

attribute vec3 a_position;

uniform mat4 u_combinedTrans;

void main() {
    gl_Position = u_combinedTrans * vec4(a_position, 1.0);
}
