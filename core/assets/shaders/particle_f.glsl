#version 150

out vec2 f_texCoord0;
out vec4 f_color;

uniform sampler2D u_texture;

void main() {
    vec4 color = texture2D(u_texture, f_texCoord0) * f_color;
    if (color.a == 0) {
        discard;
    }
    gl_FragColor = color;
}
