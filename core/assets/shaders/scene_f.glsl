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

    // material properties
    vec3 diffuseCol = texture2D(u_texture, v_texCoord0).rgb;
    vec3 ambientCol = vec3(0.4) * diffuseCol;
    vec3 specularCol = vec3(0.1);

    // cos of the angle between the normal and light directions clamped above 0
    vec3 n = normalize(v_normalCamSpace);
    vec3 l = normalize(v_lightDirCamSpace);
    float cosTheta = clamp(dot(n, l), 0, 1);

    // cos of the angle between the cam dir vector and the reflect vector
    vec3 e = normalize(v_camDirCamSpace);
    vec3 r = reflect(-l, n);
    float cosAlpha = clamp(dot(e, r), 0, 1);

    // begin strength variables
    float diffuseStrength = cosTheta;
    float specularStrength = pow(cosAlpha, 5);

    // compute visibility
    float bias = 0.005 * tan(acos(cosTheta));
    bool visible = true;
    if (v_shadowCoord.z < 0) {
        visible = false;
    } else if ((v_shadowCoord.x < 0) || (v_shadowCoord.y < 0) || (v_shadowCoord.x >= 1) || (v_shadowCoord.y >= 1)) {
        visible = false;
    } else if (texture2D(u_depthMap, v_shadowCoord.xy).a < v_shadowCoord.z - bias) {
        visible = false;
    }

    // apply visibility to light strength
    if (!visible) {
        diffuseStrength = 0;
        specularStrength = 0;
    }

    // compute the color
    vec3 col =
        ambientCol +
        diffuseCol * lightCol * diffuseStrength +
        specularCol * lightCol * specularStrength;

    gl_FragColor = vec4(col, 1);
}
