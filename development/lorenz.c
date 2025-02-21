/* file lorenz.c */
#include <R.h>
static double parms[3];
#define a parms[0]
#define b parms[1]
#define c parms[2]

/* initializer  */
void initmod(void (*odeparms)(int *, double *)) {
  int N = 3;
  odeparms(&N, parms);
}

/* Derivatives */
void derivs(int *neq, double *t, double *y, double *ydot, double *yout,
            int *ip) {
  ydot[0] = a * y[0] + y[1] * y[2];
  ydot[1] = b * (y[1] - y[2]);
  ydot[2] = -y[0] * y[1] + c * y[1] - y[2];
}
