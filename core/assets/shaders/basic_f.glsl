#version 120

varying vec2 v_texCoord0;

uniform sampler2D u_texture;

void main() {
    vec4 color = texture2D(u_texture, v_texCoord0);
    if (color.a == 0) {
        discard;
    }
    gl_FragColor = color;
}
