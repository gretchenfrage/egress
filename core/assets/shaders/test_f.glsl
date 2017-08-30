#version 120

varying vec2 v_texCoord0;
varying vec4 v_color;

uniform sampler2D u_texture;

void main() {
    gl_FragColor = texture2D(u_texture, v_texCoord0);

    //gl_FragColor = v_color;
}
