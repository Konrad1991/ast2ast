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

train <- function(X, y, perm, W1, b1, W2, b2, epochs, batch_size, lr) {
  argtypes(
    X |> type(borrow_mat(double)),
    y |> type(vec(int)),
    perm |> type(mat(int)),
    W1 |> type(borrow_mat(double)),
    b1 |> type(borrow_mat(double)),
    W2 |> type(borrow_mat(double)),
    b2 |> type(borrow_mat(double)),
    epochs |> type(int),
    batch_size |> type(int),
    lr |> type(double)
  )
  returntype(vec(double))

  dense <- function(W, b, x) {
    argtypes(
      W |> type(borrow_mat(double)) |> ref() |> const(),
      b |> type(borrow_mat(double)) |> ref() |> const(),
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

  adam_step <- function(p, g, m, v, step, lr) {
    argtypes(
      p |> type(borrow_mat(double)) |> ref(),
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

  forward <- function(W1, b1, W2, b2, xb, h, z) {
    argtypes(
      W1 |> type(borrow_mat(double)) |> ref() |> const(),
      b1 |> type(borrow_mat(double)) |> ref() |> const(),
      W2 |> type(borrow_mat(double)) |> ref() |> const(),
      b2 |> type(borrow_mat(double)) |> ref() |> const(),
      xb |> type(mat(double)) |> ref() |> const(),
      h |> type(mat(double)) |> ref(),
      z |> type(mat(double)) |> ref()
    )
    returntype(void)
    h <- dense(W1, b1, xb)
    relu(h)
    z <- dense(W2, b2, h)
  }

  backward <- function(W2, xb, h, dz, g_W1, g_b1, g_W2, g_b2) {
    argtypes(
      W2 |> type(borrow_mat(double)) |> ref() |> const(),
      xb |> type(mat(double)) |> ref() |> const(),
      h |> type(mat(double)) |> ref() |> const(),
      dz |> type(mat(double)) |> ref() |> const(),
      g_W1 |> type(mat(double)) |> ref(),
      g_b1 |> type(mat(double)) |> ref(),
      g_W2 |> type(mat(double)) |> ref(),
      g_b2 |> type(mat(double)) |> ref()
    )
    returntype(void)
    g_W2 <- dz %*% t(h)
    g_b2 <- row_sums(dz)
    dh <- t(W2) %*% dz
    relu_backward(dh, h)
    g_W1 <- dh %*% t(xb)
    g_b1 <- row_sums(dh)
  }

  m_W1 <- matrix(0.0, nrow(W1), ncol(W1))
  v_W1 <- matrix(0.0, nrow(W1), ncol(W1))
  m_b1 <- matrix(0.0, nrow(b1), ncol(b1))
  v_b1 <- matrix(0.0, nrow(b1), ncol(b1))
  m_W2 <- matrix(0.0, nrow(W2), ncol(W2))
  v_W2 <- matrix(0.0, nrow(W2), ncol(W2))
  m_b2 <- matrix(0.0, nrow(b2), ncol(b2))
  v_b2 <- matrix(0.0, nrow(b2), ncol(b2))

  g_W1 <- matrix(0.0, nrow(W1), ncol(W1))
  g_b1 <- matrix(0.0, nrow(b1), ncol(b1))
  g_W2 <- matrix(0.0, nrow(W2), ncol(W2))
  g_b2 <- matrix(0.0, nrow(b2), ncol(b2))

  n <- ncol(X)
  losses <- numeric(epochs)
  step <- 0L
  for (e in 1L:epochs) {
    start <- 1L
    while (start <= n) {
      nb <- n - start + 1L
      if (nb > batch_size) nb <- batch_size

      xb <- matrix(0.0, nrow(X), nb)
      yb <- integer(nb)
      get_batch(X, y, perm, e, start, xb, yb)

      h <- matrix(0.0, nrow(W1), nb)
      z <- matrix(0.0, nrow(W2), nb)
      forward(W1, b1, W2, b2, xb, h, z)

      dz <- matrix(0.0, nrow(W2), nb)
      losses[[e]] <- losses[[e]] + softmax_cross_entropy(z, yb, dz)

      backward(W2, xb, h, dz, g_W1, g_b1, g_W2, g_b2)

      step <- step + 1L
      adam_step(W1, g_W1, m_W1, v_W1, step, lr)
      adam_step(b1, g_b1, m_b1, v_b1, step, lr)
      adam_step(W2, g_W2, m_W2, v_W2, step, lr)
      adam_step(b2, g_b2, m_b2, v_b2, step, lr)

      start <- start + nb
    }
    losses[[e]] <- losses[[e]] / n
  }
  return(losses)
}

train_cpp <- translate(train)

epochs <- 5L
# borrowed: train_cpp updates the weights in place
W1 <- glorot_uniform(128L, 784L)
b1 <- matrix(0.0, 128L, 1L)
W2 <- glorot_uniform(10L, 128L)
b2 <- matrix(0.0, 10L, 1L)
perm <- replicate(epochs, sample(ncol(x_train)))

timing <- system.time(
  losses <- train_cpp(x_train, y_train, perm, W1, b1, W2, b2, epochs, 32L, 0.001)
)
losses
timing

predict_labels <- function(X, W1, b1, W2, b2) {
  h <- pmax(W1 %*% X + as.vector(b1), 0)
  z <- W2 %*% h + as.vector(b2)
  max.col(t(z)) - 1L
}
mean(predict_labels(x_test, W1, b1, W2, b2) == y_test)
