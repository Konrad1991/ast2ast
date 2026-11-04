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

train <- function(X, y, perm, theta, epochs, batch_size, lr) {
  argtypes(
    X |> type(borrow_mat(double)),
    y |> type(vec(int)),
    perm |> type(mat(int)),
    theta |> type(borrow_vec(double)),
    epochs |> type(int),
    batch_size |> type(int),
    lr |> type(double)
  )
  returntype(vec(double))

  dense <- function(W, b, x) {
    argtypes(
      W |> type(mat(double)) |> ref() |> const(),
      b |> type(vec(double)) |> ref() |> const(),
      x |> type(mat(double)) |> ref() |> const()
    )
    returntype(mat(double))
    out <- W %*% x
    for (j in 1L:ncol(out)) {
      for (i in 1L:nrow(out)) {
        out[i, j] <- out[i, j] + b[[i]]
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

  mean_cross_entropy <- function(z, labels) {
    argtypes(
      z |> type(mat(double)) |> ref() |> const(),
      labels |> type(mat(double)) |> ref() |> const()
    )
    returntype(double)
    total <- 0.0
    for (j in 1L:ncol(z)) {
      zmax <- z[1L, j]
      for (i in 1L:nrow(z)) {
        if (z[i, j] > zmax) zmax <- z[i, j]
      }
      s <- 0.0
      for (i in 1L:nrow(z)) {
        s <- s + exp(z[i, j] - zmax)
      }
      k <- as.integer(labels[[j]]) + 1L
      total <- total + log(s) + zmax - z[k, j]
    }
    return(total / ncol(z))
  }

  # theta = c(W1, b1, W2, b2); batch row 1 = label, rows 2.. = pixels
  loss <- function(theta, batch) {
    argtypes(
      theta |> type(vec(double)),
      batch |> type(mat(double))
    )
    returntype(double)
    n_in <- nrow(batch) - 1L
    n_hidden <- 128L
    n_out <- 10L
    end_W1 <- n_hidden * n_in
    end_b1 <- end_W1 + n_hidden
    end_W2 <- end_b1 + n_out * n_hidden
    W1 <- matrix(theta[1L:end_W1], n_hidden, n_in)
    b1 <- theta[(end_W1 + 1L):end_b1]
    W2 <- matrix(theta[(end_b1 + 1L):end_W2], n_out, n_hidden)
    b2 <- theta[(end_W2 + 1L):length(theta)]

    # NOTE: no empty index (batch[1L, ]); the colouring walk in gradient fails on it
    x <- batch[2L:nrow(batch), 1L:ncol(batch)]
    h <- dense(W1, b1, x)
    relu(h)
    z <- dense(W2, b2, h)
    return(mean_cross_entropy(z, batch[1L, 1L:ncol(batch)]))
  }

  get_batch <- function(X, y, perm, e, start, nb) {
    argtypes(
      X |> type(borrow_mat(double)) |> ref() |> const(),
      y |> type(vec(int)) |> ref() |> const(),
      perm |> type(mat(int)) |> ref() |> const(),
      e |> type(int),
      start |> type(int),
      nb |> type(int)
    )
    returntype(mat(double))
    batch <- matrix(0.0, nrow(X) + 1L, nb)
    for (j in 1L:nb) {
      sample_index <- perm[start + j - 1L, e]
      batch[1L, j] <- as.numeric(y[[sample_index]])
      for (i in 1L:nrow(X)) {
        batch[i + 1L, j] <- X[i, sample_index]
      }
    }
    return(batch)
  }

  adam_step <- function(p, g, m, v, step, lr) {
    argtypes(
      p |> type(borrow_vec(double)) |> ref(),
      g |> type(vec(double)) |> ref() |> const(),
      m |> type(vec(double)) |> ref(),
      v |> type(vec(double)) |> ref(),
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

  m <- numeric(length(theta))
  v <- numeric(length(theta))

  n <- ncol(X)
  losses <- numeric(epochs)
  step <- 0L
  for (e in 1L:epochs) {
    start <- 1L
    while (start <= n) {
      nb <- n - start + 1L
      if (nb > batch_size) nb <- batch_size

      batch <- get_batch(X, y, perm, e, start, nb)
      vg <- value_and_gradient(loss, theta, batch)
      losses[[e]] <- losses[[e]] + vg$value * nb

      step <- step + 1L
      adam_step(theta, vg$gradient, m, v, step, lr)

      start <- start + nb
    }
    losses[[e]] <- losses[[e]] / n
  }
  return(losses)
}

train_cpp <- translate(train, debug = FALSE)

unpack <- function(theta, n_in = 784L, n_hidden = 128L, n_out = 10L) {
  end_W1 <- n_hidden * n_in
  end_b1 <- end_W1 + n_hidden
  end_W2 <- end_b1 + n_out * n_hidden
  list(
    W1 = matrix(theta[1:end_W1], n_hidden, n_in),
    b1 = theta[(end_W1 + 1):end_b1],
    W2 = matrix(theta[(end_b1 + 1):end_W2], n_out, n_hidden),
    b2 = theta[(end_W2 + 1):length(theta)]
  )
}

epochs <- 5L
# same draw order as mnist.R: W1, W2, then perm
W1 <- glorot_uniform(128L, 784L)
W2 <- glorot_uniform(10L, 128L)
# borrowed: train_cpp updates theta in place
theta <- c(W1, numeric(128L), W2, numeric(10L))
perm <- replicate(epochs, sample(ncol(x_train)))

timing <- system.time(
  losses <- train_cpp(x_train, y_train, perm, theta, epochs, 32L, 0.001)
)
losses
timing

predict_labels <- function(X, p) {
  h <- pmax(p$W1 %*% X + p$b1, 0)
  z <- p$W2 %*% h + p$b2
  max.col(t(z)) - 1L
}
mean(predict_labels(x_test, unpack(theta)) == y_test)
