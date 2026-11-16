# Laplace approximation (random effects), 1.3

- theta = fixed parameters (hyperparameters, e.g. group sd)
- u = random effects (e.g. one offset per group, Statistical Rethinking ch. 13)
- user writes the joint negative log-likelihood `f(theta, u)` in R

## Integrating out u

p(y | theta) = integral p(y | u, theta) * p(u | theta) du  (marginal likelihood)

- not optimising u directly: no pooling, overfitting, sd collapses to 0
- integral is high-dimensional, no closed form -> Laplace:
  f around u_hat ~ parabola, integrand ~ Gaussian, integral = height x width

```
evaluate L(theta):              = -log integral exp(-f(theta,u)) du
├─ 1. find u_hat                -> where is the peak?
├─ 2. H, chol, log det H        -> how wide is the bell?
└─ 3. f(theta,u_hat)            -> height x width = integral
      + 1/2 log det H
      - (n_u / 2) log(2 pi)
```

## Call tree

```
laplace(f, theta0, u0, data)
└─ outer optimiser (lbfgsb) over theta
   └─ evaluate L(theta):           = -log integral exp(-f(theta,u)) du
      ├─ 1. inner optimisation: u_hat(theta) = argmin_u f(theta, u)
      │     Newton loop:                          -> peak
      │     ├─ g = grad_u f       (reverse tape)
      │     ├─ H = hess_u f       (hessian, forward-over-reverse)
      │     └─ u -= solve(H, g) until g ~ 0
      ├─ 2. H at u_hat -> chol(H)                 -> width
      │     log det H = 2 * sum(log(diag(chol)))
      └─ 3. L(theta) = f(theta, u_hat)            -> height x width
                       + 1/2 log det H
                       - (n_u / 2) log(2 pi)
   └─ dL/dtheta for lbfgsb (see below)
-> result: theta_hat, u_hat (per-group estimates), SEs from the Hessian of L
```

No separate integration loop: steps 1-3 replace the integral.

## Outer gradient dL/dtheta

- f(theta, u_hat): first order only (envelope theorem, grad_u f = 0)
- 1/2 log det H: 1/2 tr(H^-1 dH/dtheta)
  - dH/dtheta contains du_hat/dtheta = -H^-1 H_u,theta (IFT)
  - derivative of a Hessian = 3rd order; AD max is 2nd
- start: finite differences on L(theta) (2 n_theta extra L evaluations,
  each with an inner optimisation); with ODEs FD only (no 3rd order through cvode)

## Sparse

- H often sparse (independent groups -> block diagonal + border)
- needs sparse Cholesky for log det H
- tr(H^-1 dH): only the entries of H^-1 on the pattern of H, from the Cholesky factor (TMB);
  no AD through the factorisation
- start dense (chol/det exist)

## Simple case (no random effects), example only

```
lbfgsb(f) -> theta_hat -> hessian(f, theta_hat) -> Sigma = solve(H)
-> posterior ~ N(theta_hat, Sigma)
```
