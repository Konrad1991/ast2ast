RhpcBLASctl::blas_set_num_threads(1)
library(ast2ast)

# Kaggle digit-recognizer: label + 784 pixel columns
df <- read.csv("./development/mnist/train.csv")
set.seed(1234)
train_idx <- sample(nrow(df), floor(3 / 4 * nrow(df)))

# samples as columns: one image is contiguous in memory
x_train <- t(as.matrix(df[train_idx, -1L])) / 255
x_test <- t(as.matrix(df[-train_idx, -1L])) / 255
y_train <- as.integer(df[train_idx, 1L])
y_test <- as.integer(df[-train_idx, 1L])

glorot_uniform <- function(fan_out, fan_in) {
  limit <- sqrt(6 / (fan_in + fan_out))
  matrix(runif(fan_out * fan_in, -limit, limit), fan_out, fan_in)
}

types_f <- function() {
  new_type(Params, slots(
    W1 |> type(mat(double)),
    b1 |> type(mat(double)),
    W2 |> type(mat(double)),
    b2 |> type(mat(double))
  ))
  new_type(Adam, slots(
    m |> type(Params),
    v |> type(Params),
    step |> type(int)
  ))
  new_type(TrainResult, slots(
    params |> type(Params),
    losses |> type(vec(double))
  ))
}

train <- function(X, y, perm, p, epochs, batch_size, lr) {
  argtypes(
    X |> type(borrow_mat(double)),
    y |> type(vec(int)),
    perm |> type(mat(int)),
    p |> type(Params),
    epochs |> type(int),
    batch_size |> type(int),
    lr |> type(double)
  )
  returntype(TrainResult)

  zeros_like <- function(p) {
    argtypes(p |> type(Params) |> ref() |> const())
    returntype(Params)
    out |> type(Params)
    out$W1 <- matrix(0.0, nrow(p$W1), ncol(p$W1))
    out$b1 <- matrix(0.0, nrow(p$b1), ncol(p$b1))
    out$W2 <- matrix(0.0, nrow(p$W2), ncol(p$W2))
    out$b2 <- matrix(0.0, nrow(p$b2), ncol(p$b2))
    return(out)
  }

  dense <- function(W, b, x) {
    argtypes(
      W |> type(mat(double)) |> ref() |> const(),
      b |> type(mat(double)) |> ref() |> const(),
      x |> type(mat(double)) |> ref() |> const()
    )
    returntype(mat(double))
    out <- W %*% x
    for (j in 1L:ncol(out)) {
      for (i in 1L:nrow(out)) {
        out[i, j] <- out[i, j] + b[i, 1L]
      }
    }
    return(out)
  }

  relu <- function(x) {
    argtypes(x |> type(mat(double)) |> ref())
    returntype(void)
    for (i in 1L:length(x)) {
      if (x[[i]] < 0.0) x[[i]] <- 0.0
    }
  }

  relu_backward <- function(dx, x) {
    argtypes(
      dx |> type(mat(double)) |> ref(),
      x |> type(mat(double)) |> ref() |> const()
    )
    returntype(void)
    for (i in 1L:length(x)) {
      if (x[[i]] <= 0.0) dx[[i]] <- 0.0
    }
  }

  row_sums <- function(x) {
    argtypes(x |> type(mat(double)) |> ref() |> const())
    returntype(mat(double))
    out <- matrix(0.0, nrow(x), 1L)
    for (j in 1L:ncol(x)) {
      for (i in 1L:nrow(x)) {
        out[i, 1L] <- out[i, 1L] + x[i, j]
      }
    }
    return(out)
  }

  # returns summed loss; dz = gradient of the mean loss w.r.t. logits
  softmax_cross_entropy <- function(z, labels, dz) {
    argtypes(
      z |> type(mat(double)) |> ref() |> const(),
      labels |> type(vec(int)) |> ref() |> const(),
      dz |> type(mat(double)) |> ref()
    )
    returntype(double)
    nb <- ncol(z)
    loss <- 0.0
    for (j in 1L:nb) {
      zmax <- max(z[, j])
      s <- 0.0
      for (i in 1L:nrow(z)) {
        s <- s + exp(z[i, j] - zmax)
      }
      for (i in 1L:nrow(z)) {
        dz[i, j] <- exp(z[i, j] - zmax) / s / nb
      }
      k <- labels[[j]] + 1L
      dz[k, j] <- dz[k, j] - 1.0 / nb
      loss <- loss + log(s) + zmax - z[k, j]
    }
    return(loss)
  }

  get_batch <- function(X, y, perm, e, start, xb, yb) {
    argtypes(
      X |> type(borrow_mat(double)) |> ref() |> const(),
      y |> type(vec(int)) |> ref() |> const(),
      perm |> type(mat(int)) |> ref() |> const(),
      e |> type(int),
      start |> type(int),
      xb |> type(mat(double)) |> ref(),
      yb |> type(vec(int)) |> ref()
    )
    returntype(void)
    for (j in 1L:ncol(xb)) {
      sample_index <- perm[start + j - 1L, e]
      yb[[j]] <- y[[sample_index]]
      for (i in 1L:nrow(X)) {
        xb[i, j] <- X[i, sample_index]
      }
    }
  }

  forward <- function(p, xb, h, z) {
    argtypes(
      p |> type(Params) |> ref() |> const(),
      xb |> type(mat(double)) |> ref() |> const(),
      h |> type(mat(double)) |> ref(),
      z |> type(mat(double)) |> ref()
    )
    returntype(void)
    h <- dense(p$W1, p$b1, xb)
    relu(h)
    z <- dense(p$W2, p$b2, h)
  }

  dense_param_grads <- function(x, dout, gW, gb) {
    argtypes(
      x |> type(mat(double)) |> ref() |> const(),
      dout |> type(mat(double)) |> ref() |> const(),
      gW |> type(mat(double)) |> ref(),
      gb |> type(mat(double)) |> ref()
    )
    returntype(void)
    gW <- dout %*% t(x)
    gb <- row_sums(dout)
  }

  dense_input_grad <- function(W, dout) {
    argtypes(
      W |> type(mat(double)) |> ref() |> const(),
      dout |> type(mat(double)) |> ref() |> const()
    )
    returntype(mat(double))
    return(t(W) %*% dout)
  }

  # NOTE: no dense_input_grad for layer 1; dX is unused and costs a 784 x nb matmul
  backward <- function(p, xb, h, dz, g) {
    argtypes(
      p |> type(Params) |> ref() |> const(),
      xb |> type(mat(double)) |> ref() |> const(),
      h |> type(mat(double)) |> ref() |> const(),
      dz |> type(mat(double)) |> ref() |> const(),
      g |> type(Params) |> ref()
    )
    returntype(void)
    dense_param_grads(h, dz, g$W2, g$b2)
    dh <- dense_input_grad(p$W2, dz)
    relu_backward(dh, h)
    dense_param_grads(xb, dh, g$W1, g$b1)
  }

  adam_step <- function(p, g, m, v, step, lr) {
    argtypes(
      p |> type(mat(double)) |> ref(),
      g |> type(mat(double)) |> ref() |> const(),
      m |> type(mat(double)) |> ref(),
      v |> type(mat(double)) |> ref(),
      step |> type(int),
      lr |> type(double)
    )
    returntype(void)
    beta1 <- 0.9
    beta2 <- 0.999
    eps <- 1e-7
    correction1 <- 1.0 - beta1^step
    correction2 <- 1.0 - beta2^step
    for (i in 1L:length(p)) {
      m[[i]] <- beta1 * m[[i]] + (1.0 - beta1) * g[[i]]
      v[[i]] <- beta2 * v[[i]] + (1.0 - beta2) * g[[i]]^2
      p[[i]] <- p[[i]] - lr * (m[[i]] / correction1) /
        (sqrt(v[[i]] / correction2) + eps)
    }
  }

  adam_init <- function(p) {
    argtypes(p |> type(Params) |> ref() |> const())
    returntype(Adam)
    adam |> type(Adam)
    adam$m <- zeros_like(p)
    adam$v <- zeros_like(p)
    adam$step <- 0L
    return(adam)
  }

  adam_update <- function(p, g, adam, lr) {
    argtypes(
      p |> type(Params) |> ref(),
      g |> type(Params) |> ref() |> const(),
      adam |> type(Adam) |> ref(),
      lr |> type(double)
    )
    returntype(void)
    adam$step <- adam$step + 1L
    adam_step(p$W1, g$W1, adam$m$W1, adam$v$W1, adam$step, lr)
    adam_step(p$b1, g$b1, adam$m$b1, adam$v$b1, adam$step, lr)
    adam_step(p$W2, g$W2, adam$m$W2, adam$v$W2, adam$step, lr)
    adam_step(p$b2, g$b2, adam$m$b2, adam$v$b2, adam$step, lr)
  }

  # returns the mean loss of epoch e
  train_epoch <- function(X, y, perm, e, p, g, adam, batch_size, lr) {
    argtypes(
      X |> type(borrow_mat(double)) |> ref() |> const(),
      y |> type(vec(int)) |> ref() |> const(),
      perm |> type(mat(int)) |> ref() |> const(),
      e |> type(int),
      p |> type(Params) |> ref(),
      g |> type(Params) |> ref(),
      adam |> type(Adam) |> ref(),
      batch_size |> type(int),
      lr |> type(double)
    )
    returntype(double)
    n <- ncol(X)
    loss <- 0.0
    start <- 1L
    while (start <= n) {
      nb <- n - start + 1L
      if (nb > batch_size) nb <- batch_size

      xb <- matrix(0.0, nrow(X), nb)
      yb <- integer(nb)
      get_batch(X, y, perm, e, start, xb, yb)

      h <- matrix(0.0, nrow(p$W1), nb)
      z <- matrix(0.0, nrow(p$W2), nb)
      forward(p, xb, h, z)

      dz <- matrix(0.0, nrow(p$W2), nb)
      loss <- loss + softmax_cross_entropy(z, yb, dz)

      backward(p, xb, h, dz, g)
      adam_update(p, g, adam, lr)

      start <- start + nb
    }
    return(loss / n)
  }

  adam <- adam_init(p)
  g <- zeros_like(p)
  losses <- numeric(epochs)
  for (e in 1L:epochs) {
    losses[[e]] <- train_epoch(X, y, perm, e, p, g, adam, batch_size, lr)
  }

  res |> type(TrainResult)
  res$params <- p
  res$losses <- losses
  return(res)
}

train_cpp <- translate(train, types_f = types_f, debug = FALSE)

epochs <- 5L
params <- structure(
  list(
    W1 = glorot_uniform(128L, 784L),
    b1 = matrix(0.0, 128L, 1L),
    W2 = glorot_uniform(10L, 128L),
    b2 = matrix(0.0, 10L, 1L)
  ),
  class = "Params"
)
perm <- replicate(epochs, sample(ncol(x_train)))

timing <- system.time(
  res <- train_cpp(x_train, y_train, perm, params, epochs, 32L, 0.001)
)
res$losses
timing

predict_labels <- function(X, p) {
  h <- pmax(p$W1 %*% X + as.vector(p$b1), 0)
  z <- p$W2 %*% h + as.vector(p$b2)
  max.col(t(z)) - 1L
}
mean(predict_labels(x_test, res$params) == y_test)
