#version 120

uniform float u_lightFar;
uniform vec3 u_lightPos;

varying vec4 v_realPos;

void main() {
    //gl_FragColor = vec4(gl_FragCoord.z / u_lightFar);
    //gl_FragColor = vec4(vec3(length(v_realPos.xyz - u_lightPos) / u_lightFar), 1);
    gl_FragColor = vec4(gl_FragCoord.z);
}
