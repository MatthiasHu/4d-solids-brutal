varying vec3 vNormal;
varying float vDepth;

void main() {
  float bright = min(1.0, abs(vNormal.z));
  bright = pow(bright, 1.5);
  gl_FragColor = bright * vec4(1.0, 1.0, 0.0, 1.0);
  gl_FragDepth = vDepth;
}
