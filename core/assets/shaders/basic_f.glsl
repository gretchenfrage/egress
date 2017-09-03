#version 120

varying vec2 v_texCoord0;

uniform sampler2D u_texture;

void main() {
    gl_FragColor = texture2D(u_texture, v_texCoord0);
}
