#include <R.h>

static double parms[4];
#define A parms[0]
#define B parms[1]
#define alpha parms[2]
#define NN parms[3]

void initmod(void (*odeparms)(int *, double *)) {
  int n = 4;
  odeparms(&n, parms);
}

/* zero-flux boundaries: the neighbour outside the grid is mirrored */
static int mirror(int i, int N) {
  if (i < 0) return 1;
  if (i >= N) return N - 2;
  return i;
}

void derivs(int *neq, double *t, double *y, double *ydot, double *yout, int *ip) {
  int N = (int) NN;
  double inv_h2 = (N - 1.0) * (N - 1.0);
  const double *u = y, *v = y + N * N;
  for (int j = 0; j < N; j++) {
    for (int i = 0; i < N; i++) {
      int k = j * N + i;
      int w = j * N + mirror(i - 1, N), e = j * N + mirror(i + 1, N);
      int s = mirror(j - 1, N) * N + i, n = mirror(j + 1, N) * N + i;
      double lap_u = (u[w] + u[e] + u[s] + u[n] - 4.0 * u[k]) * inv_h2;
      double lap_v = (v[w] + v[e] + v[s] + v[n] - 4.0 * v[k]) * inv_h2;
      double uuv = u[k] * u[k] * v[k];
      ydot[k] = A + uuv - (B + 1.0) * u[k] + alpha * lap_u;
      ydot[N * N + k] = B * u[k] - uuv + alpha * lap_v;
    }
  }
}
