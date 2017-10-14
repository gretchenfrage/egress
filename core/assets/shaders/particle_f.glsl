#version 150

in vec2 f_texCoord0;
in vec4 f_color;

uniform sampler2D u_texture;

void main() {
    vec4 color = texture2D(u_texture, f_texCoord0) * f_color;
    if (color.a == 0) {
        discard;
    }
    //vec4 color = vec4(f_texCoord0, 0, 1);
    //vec4 color = texture2D(u_texture, f_texCoord0) * f_color;
    //vec4 color = f_color;
    gl_FragColor = color;
}
