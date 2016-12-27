attribute vec3 aNormal;
varying vec3 vNormal;
varying float vDepth;

void main() {
  vec4 pos = gl_Vertex + vec4(0.0, 0.0, 5.0, 0.0);

  float near = 0.01;
  float far = 100.0;
  float depth = (pos.z-near)/(far-near);
  gl_Position = vec4(pos.xy, depth, 0.5*pos.z);
  vDepth = depth;
  vNormal = aNormal;
}
