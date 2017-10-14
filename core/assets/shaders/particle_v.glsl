#version 150

in vec3 a_position;
in vec4 a_color;
in float a_size;
in vec2 a_texCoord0;
in vec2 a_texCoord1;

uniform mat4 u_MV;

out vec2 v_texCoord0;
out vec2 v_texCoord1;
out float v_size;
out vec4 v_color;

void main() {
    gl_Position = u_MV * vec4(a_position, 1);
    v_texCoord0 = a_texCoord0;
    v_texCoord1 = a_texCoord1;
    v_size = a_size;
    v_color = a_color;
}
