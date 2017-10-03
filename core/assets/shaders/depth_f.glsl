#version 150

in float f_isBackfaceColluder;

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
    gl_FragColor = encodeFloatRGBA(gl_FragCoord.z * (1 - f_isBackfaceColluder));
}
