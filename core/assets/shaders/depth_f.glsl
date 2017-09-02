#version 120

uniform float u_lightFar;
uniform vec3 u_lightPos;

void main() {
    gl_FragColor = vec4(gl_FragCoord.z);
}
