#version 150

in vec3 f_pos;

in vec2 f_texCoord0;
in vec4 f_color;
in vec4 f_shadowCoord;

in vec3 f_normalWorldSpace;
in vec3 f_normalCamSpace;
in vec3 f_lightDirCamSpace;
in vec3 f_camDirCamSpace;

uniform sampler2D u_texture;
uniform sampler2D u_depthMap;
uniform vec3 u_lightPos;
uniform float u_lightPow;

vec4 encodeFloatRGBA(float n) {
    vec4 enc = vec4(1.0, 255.0, 65025.0, 160581375.0) * n;
    enc = fract(enc);
    enc -= enc.yzww * vec4(1.0 / 255.0, 1.0 / 255.0, 1.0 / 255.0, 0.0);
    return enc;
}

float decodeFloatRGBA(vec4 rgba) {
    return dot(rgba, vec4(1.0, 1 / 255.0, 1 / 65025.0, 1 / 160581375.0));
}

void main() {
    // constants
    vec3 lightCol = vec3(1, 1, 1);

    // material properties
    vec3 diffuseCol = texture2D(u_texture, f_texCoord0).rgb;
    //vec3 diffuseCol = vec3(v_texCoord0, 1);
    vec3 ambientCol = vec3(0.3) * diffuseCol;
    vec3 specularCol = vec3(1, 1, 1) * 0.05;

    // cos of the angle between the normal and light directions clamped above 0
    vec3 n = normalize(f_normalCamSpace);
    vec3 l = normalize(f_lightDirCamSpace);
    float cosTheta = clamp(dot(n, l), 0, 1);

    // cos of the angle between the cam dir vector and the reflect vector
    vec3 e = normalize(f_camDirCamSpace);
    vec3 r = reflect(-l, n);
    float cosAlpha = clamp(dot(e, r), 0, 1);

    // begin strength variables
    float diffuseStrength = cosTheta * u_lightPow;
    float specularStrength = pow(cosAlpha, 5) * u_lightPow;

    // compute visibility
    float bias = 0.005 * tan(acos(cosTheta));
    bool visible = true;
    if (f_shadowCoord.z < 0) {
        visible = false;
    } else if ((f_shadowCoord.x < 0) || (f_shadowCoord.y < 0) || (f_shadowCoord.x >= 1) || (f_shadowCoord.y >= 1)) {
        visible = false;
    } else if (dot(f_normalWorldSpace, u_lightPos - f_pos) < 0) {
        visible = false;
    } else if (decodeFloatRGBA(texture2D(u_depthMap, f_shadowCoord.xy)) < f_shadowCoord.z - bias) {
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

    gl_FragColor = vec4(col, 1);// * f_color;
}
