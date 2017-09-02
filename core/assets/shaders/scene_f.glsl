#version 120

varying vec2 v_texCoord0;
varying vec4 v_color;
varying vec4 v_shadowCoord;

uniform sampler2D u_texture;
uniform sampler2D u_depthMap;

void main() {
    float visibility = 1.0;
    if (texture2D(u_depthMap, v_shadowCoord.xy).a < v_shadowCoord.z) {
        visibility = 0.4;
    }
    gl_FragColor = texture2D(u_texture, v_texCoord0) * visibility;

    //gl_FragColor = texture2D(u_texture, v_texCoord0);
}
