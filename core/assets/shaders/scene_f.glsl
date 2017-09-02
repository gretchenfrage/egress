#version 120

varying vec3 v_pos;
varying vec2 v_texCoord0;
varying vec4 v_color;
varying vec4 v_shadowCoord;

varying vec3 v_normalCamSpace;
varying vec3 v_lightDirCamSpace;
varying vec3 v_camDirCamSpace;

uniform sampler2D u_texture;
uniform sampler2D u_depthMap;
uniform vec3 lightPos;


void main() {
    // constants
    vec3 lightCol = vec3(1, 1, 1);
    float lightPow = 50;
    float bias = 0.005;

    // material properties
    vec3 diffuseCol = texture2D(u_texture, v_texCoord0).rgb;
    vec3 ambientCol = vec3(0.95, 0.95, 0.95) * diffuseCol;
    vec3 specularCol = vec3(0.05, 0.05, 0.05);

    // distance to light
    float distance = length(lightPos - v_pos);

    // cos of the angle between the normal and light directions clamped above 0
    vec3 n = normalize(v_normalCamSpace);
    vec3 l = normalize(v_lightDirCamSpace);
    float cosTheta = clamp(dot(n, l), -1, 1);
    if (cosTheta < 0) {
        cosTheta /= 4;
    }

    // cos of the angle between the cam dir vector and the reflect vector
    vec3 e = normalize(v_camDirCamSpace);
    vec3 r = reflect(-l, n);
    float cosAlpha = clamp(dot(e, r), 0, 1);

    // temporary, neutralize the distance
    distance = 1;
    lightPow = 0.5;

    // compute visibility
    float visibility = 1.0;
    if (v_shadowCoord.z < 0) {
        visibility = 0.2;
    } else if ((v_shadowCoord.x < 0) || (v_shadowCoord.y < 0) || (v_shadowCoord.x >= 1) || (v_shadowCoord.y >= 1)) {
        visibility = 0.2;
    } else if (texture2D(u_depthMap, v_shadowCoord.xy).a < v_shadowCoord.z - bias) {
        visibility = 0.2;
    }

    // compute the color
    vec3 col =
        ambientCol +
        visibility * diffuseCol * lightCol * lightPow * cosTheta / (distance * distance) +
        visibility * specularCol * lightCol * lightPow * pow(cosAlpha, 5) / (distance * distance);

    gl_FragColor = vec4(col, 1);
}
