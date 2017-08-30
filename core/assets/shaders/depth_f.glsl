#version 120

uniform float u_cameraFar;
uniform vec3 u_lightPosition;

varying vec4 v_position;

// depth fragment shader
void main() {
    gl_FragColor = vec4(length(v_position.xyz - u_lightPosition) / u_cameraFar);
    //gl_FragColor = vec4(1, 0, 0, 1);
}