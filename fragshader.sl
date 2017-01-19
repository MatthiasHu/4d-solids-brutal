varying vec3 vNormal;
varying float vDepth;

void main() {
  vec3 lightVector = normalize(vec3(0.1, -0.2, 1.0));
  float bright = min(1.0, abs(dot(vNormal, lightVector)));
  bright = pow(bright, 1.5);
  gl_FragColor = bright * vec4(1.0, 1.0, 0.0, 1.0);
  gl_FragDepth = vDepth;
}
