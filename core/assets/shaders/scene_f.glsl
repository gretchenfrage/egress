#version 120

varying vec2 v_texCoord0;
varying vec4 v_color;
varying vec4 v_shadowCoord;

uniform sampler2D u_texture;
uniform sampler2D u_depthMap;

void main() {
    float bias = 0.005;
    float visibility = 1.0;
    if (v_shadowCoord.z < 0) {
        visibility = 0.4;
    } else if ((v_shadowCoord.x < 0) || (v_shadowCoord.y < 0) || (v_shadowCoord.x >= 1) || (v_shadowCoord.y >= 1)) {
        visibility = 0.4;
    } else if (texture2D(u_depthMap, v_shadowCoord.xy).a < v_shadowCoord.z - bias) {
        visibility = 0.4;
    }
    gl_FragColor = texture2D(u_texture, v_texCoord0) * visibility;
}
