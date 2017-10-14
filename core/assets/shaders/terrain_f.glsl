#version 150

in vec3 f_pos;

in vec2 f_texCoord0;
in vec4 f_color;
in vec4 f_shadowCoord;

in vec3 f_normalWorldSpace;
in vec3 f_normalCamSpace;
in vec3 f_lightDirCamSpace;
in vec3 f_eyeDirCamSpace;

uniform vec3 u_lightPos;
uniform float u_lightPow;
uniform sampler2D u_texture;

void main() {
    // constants
    vec3 lightCol = vec3(1, 1, 1);

    // material properties
    vec3 diffuseCol = texture2D(u_texture, f_texCoord0).rgb;
    //vec3 diffuseCol = vec3(v_texCoord0, 1);
    vec3 ambientCol = vec3(0.1) * diffuseCol;
    vec3 specularCol = vec3(1, 1, 1) * 0.05;

    // cos of the angle between the normal and light directions clamped above 0
    vec3 n = normalize(f_normalCamSpace);
    vec3 l = normalize(f_lightDirCamSpace);
    float cosTheta = clamp(dot(n, l), 0, 1);

    // cos of the angle between the cam dir vector and the reflect vector
    vec3 e = normalize(f_eyeDirCamSpace);
    vec3 r = reflect(-l, n);
    float cosAlpha = clamp(dot(e, r), 0, 1);

    // begin strength variables
    float diffuseStrength = cosTheta * u_lightPow;
    float specularStrength = pow(cosAlpha, 5) * u_lightPow;

    // compute the color
    vec3 col =
        ambientCol +
        diffuseCol * lightCol * diffuseStrength +
        specularCol * lightCol * specularStrength;

    gl_FragColor = vec4(col, 1);// * f_color;
}
