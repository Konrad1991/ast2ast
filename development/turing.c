#include <R.h>

static double parms[5];
#define A parms[0]
#define B parms[1]
#define Du parms[2]
#define Dv parms[3]
#define NN parms[4]

void initmod(void (*odeparms)(int *, double *)) {
  int n = 5;
  odeparms(&n, parms);
}

/* zero-flux boundaries: the neighbour outside the grid is mirrored */
static int mirror(int i, int N) {
  if (i < 0) return 1;
  if (i >= N) return N - 2;
  return i;
}

/* grid spacing 1 */
void derivs(int *neq, double *t, double *y, double *ydot, double *yout, int *ip) {
  int N = (int) NN;
  const double *u = y, *v = y + N * N;
  for (int j = 0; j < N; j++) {
    for (int i = 0; i < N; i++) {
      int k = j * N + i;
      int w = j * N + mirror(i - 1, N), e = j * N + mirror(i + 1, N);
      int s = mirror(j - 1, N) * N + i, n = mirror(j + 1, N) * N + i;
      double lap_u = u[w] + u[e] + u[s] + u[n] - 4.0 * u[k];
      double lap_v = v[w] + v[e] + v[s] + v[n] - 4.0 * v[k];
      double uuv = u[k] * u[k] * v[k];
      ydot[k] = A + uuv - (B + 1.0) * u[k] + Du * lap_u;
      ydot[N * N + k] = B * u[k] - uuv + Dv * lap_v;
    }
  }
}
