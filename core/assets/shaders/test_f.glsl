#version 120

varying vec2 v_texCoord0;
varying vec4 v_position;
varying vec4 v_positionLightTrans;

uniform sampler2D u_texture;
uniform samplerCube u_depthMap;
uniform float u_cameraFar;
uniform vec3 u_lightPosition;

void main() {
    vec4 col = texture2D(u_texture, v_texCoords0);

    vec3 lightDir = v_position.xyz - u_lightPosition;
    float fragDepth = length(lightDir) / u_cameraFar;
    float depthSample = textureCube(u_depthMap, lightDir).a;
    if (v_positionLightTrans.z >= 0.0 &&
            (depth.x >= 0.0 && detph.x <= 1.0) &&
            (depth.y >= 0.0 && depth.y <= 1.0)) {

        if (deptSample < fragDepth - 0.005) {
            col.rgb *= 0.4;
        } else {
            col.rgb *= 0.4 + 0.6 * (1.0 - fragDepth);
        }

    } else {
        col.rgb *= 0.4;
    }

    gl_FragColor = finalColor;
}
