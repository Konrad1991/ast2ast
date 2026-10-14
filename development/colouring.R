# steps 1-3 live in R/Colouring.R now; this file keeps ast_of + the examples
files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

ast_of <- function(f, r_fct = TRUE) {
  env <- new.env(parent = emptyenv())
  env$r_fct <- r_fct
  env$real_type <- "etr::Double"
  parse_body(
    body(f), env, ast2ast:::function_registry_global
  )
}

f <- function(ydot, y) {
  a <- 1
  b <- a + 2
  ydot[[1]] <- y[[1]]*2
  ydot[[2]] <- y[[2]]*2 + y[[1]]
  for (i in seq_len(length(y))) {
    ydot[[i]] <- y[[i]] + 1
  }
}

ast <- ast_of(f)
rd <- row_deps(ast, state = "y", target = "ydot")
materialize(rd, n = 4)
# expected: 1 1 1 1 (diagonal -> one sweep)
color_columns(materialize(rd, n = 4), n = 4)

diffusion <- function(ydot, y) {
  n <- length(y)
  ydot[[1]] <- y[[2]] - y[[1]]
  for (i in 2:(n - 1)) {
    ydot[[i]] <- y[[i - 1]] - 2 * y[[i]] + y[[i + 1]]
  }
  ydot[[n]] <- y[[n - 1]] - y[[n]]
}

rd <- row_deps(ast_of(diffusion), state = "y", target = "ydot")
materialize(rd, n = 6)
# expected: 1 2 3 1 2 3
color_columns(materialize(rd, n = 6), n = 6)

# bound is an input arg -> not known at compile time -> NULL (dense)
arg_bound <- function(ydot, y, k) {
  for (i in 1:k) {
    ydot[[i]] <- y[[i]]
  }
}
rd <- row_deps(ast_of(arg_bound), state = "y", target = "ydot")
materialize(rd, n = 4)

# expected: {1} {1}
read_target <- function(ydot, y) {
  ydot[[1]] <- y[[1]]
  ydot[[2]] <- ydot[[1]] * 2
}
rd <- row_deps(ast_of(read_target), state = "y", target = "ydot")
materialize(rd, n = 2)

# expected: {1,2} {1,2,3} {2,3,4} {3,4}
accumulate <- function(ydot, y) {
  n <- length(y)
  for (i in 1:n) {
    ydot[[i]] <- 0
    ydot[[i]] <- ydot[[i]] - y[[i]]
  }
  for (i in 2:n) {
    ydot[[i]] <- ydot[[i]] + y[[i - 1]]
  }
  for (i in 1:(n - 1)) {
    ydot[[i]] <- ydot[[i]] + y[[i + 1]]
  }
}
rd <- row_deps(ast_of(accumulate), state = "y", target = "ydot")
materialize(rd, n = 4)

# unwritten row read -> NULL (dense)
read_unwritten <- function(ydot, y) {
  ydot[[1]] <- ydot[[2]] + y[[1]]
  ydot[[2]] <- y[[2]]
}
rd <- row_deps(ast_of(read_unwritten), state = "y", target = "ydot")
materialize(rd, n = 2)

# expected: {2, 3} {2} {3}
with_if <- function(ydot, y, k) {
  ydot[[1]] <- y[[2]]
  ydot[[2]] <- y[[2]]
  ydot[[3]] <- y[[3]]
  if (y[[1]] > k) {
    ydot[[1]] <- y[[3]]
  }
}
rd <- row_deps(ast_of(with_if), state = "y", target = "ydot")
materialize(rd, n = 3)
