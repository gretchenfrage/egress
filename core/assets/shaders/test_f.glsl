#version 120

varying vec2 v_texCoord0;
varying vec4 v_position;
varying vec4 v_positionLightTrans;

uniform sampler2D u_texture;
uniform sampler2D u_depthMap;
uniform float u_cameraFar;
uniform vec3 u_lightPosition;

void main() {
    vec4 col = texture2D(u_texture, v_texCoord0);
    col = vec4(1.0, 0.0, 0.0, 1.0);

/*
    vec3 depth = (v_positionLightTrans.xyz / v_positionLightTrans.w) * 0.5 + 0.5;
    if (v_positionLightTrans.z >= 0.0 &&
            (depth.x >= 0.0 && depth.x <= 1.0) &&
            (depth.y >= 0.0 && depth.y <= 1.0)) {

        float lenToLight = length(v_position.xyz - u_lightPosition) / u_cameraFar;
        float lenDepthMap = texture2D(u_depthMap, depth.xy).a;
        if (lenDepthMap < lenToLight - 0.005) {
            col.rgb *= 0.4;
        } else {
            col.rgb *= 0.4 + 0.6 * (1.0 - lenToLight);
        }

    } else {
        col.rgb *= 0.4;
    }
*/

    gl_FragColor = col;
}
