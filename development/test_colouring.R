# run from the package root
source("development/colouring.R")
set.seed(63)

# the DSL fns are plain R: append ydot so they return it
as_rhs <- function(f) {
  g <- f
  body(g) <- call("{", body(f), quote(ydot))
  g
}

eval_rhs <- function(g, y, args) {
  do.call(g, c(list(ydot = numeric(length(y)), y = y), args))
}

fd_jac <- function(g, y, args, h = 1e-6) {
  f0 <- eval_rhs(g, y, args)
  J <- matrix(0, length(f0), length(y))
  for (j in seq_along(y)) {
    yp <- y
    yp[j] <- yp[j] + h
    J[, j] <- (eval_rhs(g, yp, args) - f0) / h
  }
  J
}

pattern_matrix <- function(rows, n) {
  P <- matrix(FALSE, n, n)
  for (i in seq_len(n)) P[i, rows[[i]]] <- TRUE
  P
}

# step 4 rehearsal: one FD sweep per colour, then scatter
compressed_jac <- function(g, y, args, P, color, h = 1e-6) {
  n <- length(y)
  f0 <- eval_rhs(g, y, args)
  J <- matrix(0, n, n)
  for (c in seq_len(max(color))) {
    v <- as.numeric(color == c)
    w <- (eval_rhs(g, y + h * v, args) - f0) / h
    for (j in which(color == c)) {
      i <- which(P[, j])
      J[i, j] <- w[i]
    }
  }
  J
}

check_case <- function(name, f, n, args = list(), expect = "sparse", rows = NULL,
                       draws = 20) {
  res <- function(status, msg = "") data.frame(case = name, n = n, status = status, msg = msg)
  rd <- tryCatch(row_deps(ast_of(f), state = "y", target = "ydot"),
                 error = function(e) e)
  if (inherits(rd, "error")) return(res("FAIL", paste("row_deps:", conditionMessage(rd))))
  pat <- materialize(rd, n)
  if (expect == "dense") {
    if (is.null(pat)) return(res("ok", "dense"))
    return(res("FAIL", "expected dense, got a pattern"))
  }
  # "any": dense is fine, a pattern must be sound
  if (is.null(pat)) {
    if (expect == "any") return(res("ok", "dense"))
    return(res("FAIL", "unexpected dense"))
  }
  if (!is.null(rows) && !identical(lapply(pat, as.integer), lapply(rows, as.integer))) {
    got <- paste(vapply(pat, function(r) paste0("{", paste(r, collapse = ","), "}"), ""),
                 collapse = " ")
    return(res("FAIL", paste("rows differ, got", got)))
  }
  color <- color_columns(pat, n)
  for (r in pat) {
    if (anyDuplicated(color[r])) return(res("FAIL", "invalid colouring"))
  }
  P <- pattern_matrix(pat, n)
  g <- as_rhs(f)
  seen <- matrix(FALSE, n, n)
  for (d in seq_len(draws)) {
    y <- rnorm(n)
    J <- tryCatch(fd_jac(g, y, args), error = function(e) e)
    if (inherits(J, "error")) return(res("FAIL", paste("R eval:", conditionMessage(J))))
    if (nrow(J) != n) return(res("FAIL", "ydot length != n"))
    nz <- abs(J) > 1e-6
    missing <- which(nz & !P, arr.ind = TRUE)
    if (nrow(missing) > 0) {
      ij <- paste0("(", missing[, 1], ",", missing[, 2], ")", collapse = " ")
      return(res("FAIL", paste("WRONG pattern, missing", ij)))
    }
    seen <- seen | nz
    Jc <- compressed_jac(g, y, args, P, color)
    if (any(abs(Jc - J) > 1e-4 * (1 + abs(J)))) return(res("FAIL", "compressed J != full J"))
  }
  extra <- sum(P & !seen)
  msg <- paste0(max(color), " colours")
  if (extra > 0) msg <- paste0(msg, "; ", extra, " extra entries (not seen numerically)")
  res("ok", msg)
}

cases <- list(
  list("diag", f, 4, rows = list(1, 2, 3, 4)),
  list("diffusion", diffusion, 6, rows = list(1:2, 1:3, 2:4, 3:5, 4:6, 5:6)),
  list("diffusion", diffusion, 3, rows = list(1:2, 1:3, 2:3)),
  list("arg_bound", arg_bound, 4, args = list(k = 4), expect = "dense"),
  list("read_target", read_target, 2, rows = list(1, 1)),
  list("accumulate", accumulate, 4, rows = list(1:2, 1:3, 2:4, 3:4)),
  list("read_unwritten", read_unwritten, 2, expect = "dense"),
  list("with_if", with_if, 3, args = list(k = 0), rows = list(2:3, 2, 3)),

  list("else_if", function(ydot, y) {
    if (y[[1]] > 0) {
      ydot[[1]] <- y[[1]]
    } else if (y[[2]] > 0) {
      ydot[[1]] <- y[[2]]
    } else {
      ydot[[1]] <- y[[3]]
    }
    ydot[[2]] <- y[[2]]
    ydot[[3]] <- y[[3]]
  }, 3, rows = list(1:3, 2, 3)),

  list("nested_if", function(ydot, y) {
    ydot[[1]] <- y[[1]]
    ydot[[2]] <- y[[2]]
    if (y[[1]] > 0) {
      if (y[[2]] > 0) {
        ydot[[2]] <- y[[1]] * y[[2]]
      }
    } else {
      ydot[[1]] <- y[[2]]
    }
  }, 2, rows = list(1:2, 1:2)),

  list("if_without_prior_write", function(ydot, y) {
    if (y[[1]] > 0) {
      ydot[[1]] <- y[[2]]
    }
    ydot[[2]] <- y[[2]]
  }, 2, rows = list(2, 2)),

  list("if_intermediate", function(ydot, y) {
    a <- y[[1]]
    if (y[[2]] > 0) {
      a <- y[[2]]
    }
    ydot[[1]] <- a
    ydot[[2]] <- y[[2]]
  }, 2, rows = list(1:2, 2)),

  list("unary_paren", function(ydot, y) {
    ydot[[1]] <- -(y[[1]] + y[[2]])
    ydot[[2]] <- -y[[2]]
  }, 2, rows = list(1:2, 2)),

  list("functions", function(ydot, y) {
    ydot[[1]] <- sin(y[[1]]) * exp(y[[2]])
    ydot[[2]] <- y[[2]]^2
  }, 2, rows = list(1:2, 2)),

  list("intermediate_chain", function(ydot, y) {
    a <- y[[1]] * 2
    b <- a + y[[3]]
    ydot[[1]] <- b
    ydot[[2]] <- a
    ydot[[3]] <- y[[2]]
  }, 3, rows = list(c(1, 3), 1, 2)),

  list("seq_along", function(ydot, y) {
    for (i in seq_along(y)) {
      ydot[[i]] <- y[[i]]^2
    }
  }, 5, rows = list(1, 2, 3, 4, 5)),

  list("seq_along_n1", function(ydot, y) {
    for (i in seq_along(y)) {
      ydot[[i]] <- y[[i]]^2
    }
  }, 1, rows = list(1)),

  list("loop_intermediate", function(ydot, y) {
    n <- length(y)
    ydot[[1]] <- y[[1]]
    for (i in 2:(n - 1)) {
      flux <- y[[i + 1]] - y[[i - 1]]
      ydot[[i]] <- flux * y[[i]]
    }
    ydot[[n]] <- y[[n]]
  }, 5, rows = list(1, 1:3, 2:4, 3:5, 5)),

  list("loop_outer_intermediate", function(ydot, y) {
    s <- y[[1]] * 3
    for (i in seq_along(y)) {
      ydot[[i]] <- y[[i]] + s
    }
  }, 4, rows = list(1, 1:2, c(1, 3), c(1, 4))),

  list("loop_fixed_index", function(ydot, y) {
    for (i in seq_along(y)) {
      ydot[[i]] <- y[[i]] * y[[2]]
    }
  }, 4, rows = list(1:2, 2, 2:3, c(2, 4))),

  list("forward_read", function(ydot, y) {
    n <- length(y)
    for (i in seq_along(y)) {
      ydot[[i]] <- y[[i]]
    }
    for (i in 1:(n - 1)) {
      ydot[[i]] <- ydot[[i]] + ydot[[i + 1]]
    }
  }, 4, rows = list(1:2, 2:3, 3:4, 4)),

  list("two_bands_one_loop", function(ydot, y) {
    for (i in seq_along(y)) {
      ydot[[i]] <- y[[i]]
      ydot[[i]] <- ydot[[i]] * y[[1]]
    }
  }, 3, rows = list(1, 1:2, c(1, 3))),

  # 2:(n-1) with n = 2 is c(2, 1) in R: loop runs backwards
  list("descending_seq", function(ydot, y) {
    n <- length(y)
    ydot[[1]] <- y[[1]]
    ydot[[2]] <- y[[2]]
    for (i in 2:(n - 1)) {
      ydot[[i]] <- y[[i]] + y[[1]]
    }
  }, 2, rows = list(1, 1:2)),

  # 0:n would hit ydot[[0]] -> dense
  list("out_of_range_seq", function(ydot, y) {
    n <- length(y)
    for (i in 0:n) {
      ydot[[i]] <- y[[i]]
    }
  }, 3, expect = "dense"),

  list("lhs_not_loop_var", function(ydot, y) {
    for (i in 2:length(y)) {
      ydot[[i - 1]] <- y[[i]]
    }
  }, 3, expect = "dense"),

  list("reassigned_n", function(ydot, y) {
    n <- length(y)
    n <- 2
    for (i in 1:n) {
      ydot[[i]] <- y[[i]]
    }
  }, 2, expect = "dense"),

  list("arg_index", function(ydot, y, k) {
    ydot[[1]] <- y[[k]]
  }, 1, args = list(k = 1), expect = "dense"),

  list("bare_state", function(ydot, y) {
    ydot[[1]] <- sum(y)
    ydot[[2]] <- y[[2]]
  }, 2, expect = "dense"),

  list("while", function(ydot, y) {
    ydot[[1]] <- y[[1]]
    while (FALSE) {
      ydot[[1]] <- y[[1]]
    }
  }, 1, expect = "dense"),

  list("if_in_loop", function(ydot, y) {
    for (i in seq_along(y)) {
      if (y[[i]] > 0) {
        ydot[[i]] <- y[[i]]
      }
    }
  }, 2, expect = "dense"),

  # field a must not pick up the intermediate a
  list("dollar_no_collision", function(ydot, y, params) {
    a <- y[[1]]
    ydot[[1]] <- params$a * y[[2]]
    ydot[[2]] <- a
  }, 2, args = list(params = list(a = 2)), rows = list(2, 1)),

  list("diffusion_large", diffusion, 100),
  list("accumulate_large", accumulate, 50),
  list("loop_intermediate_large", function(ydot, y) {
    n <- length(y)
    ydot[[1]] <- y[[1]]
    for (i in 2:(n - 1)) {
      flux <- y[[i + 1]] - y[[i - 1]]
      ydot[[i]] <- flux * y[[i]]
    }
    ydot[[n]] <- y[[n]]
  }, 80)
)

out <- do.call(rbind, lapply(cases, function(cs) {
  args <- cs[-(1:3)]
  do.call(check_case, c(list(name = cs[[1]], f = cs[[2]], n = cs[[3]]), args))
}))
print(out, right = FALSE)
cat(sum(out$status == "ok"), "/", nrow(out), "ok\n")

# fuzzing: random RHS, only soundness matters (dense allowed)
fuzz_n <- 6
fuzz_vars <- c("t1", "t2", "t3")

gen_leaf <- function(in_loop) {
  opts <- c("y", "ydot", "lit", "var")
  if (in_loop) opts <- c(opts, "y_i", "ydot_i")
  switch(sample(opts, 1),
    y = sprintf("y[[%d]]", sample(fuzz_n, 1)),
    ydot = sprintf("ydot[[%d]]", sample(fuzz_n, 1)),
    lit = sprintf("%d", sample(1:3, 1)),
    var = sample(fuzz_vars, 1),
    y_i = sprintf("y[[%s]]", sample(c("i - 1", "i", "i + 1"), 1)),
    ydot_i = sprintf("ydot[[%s]]", sample(c("i - 1", "i", "i + 1"), 1))
  )
}

gen_expr <- function(depth, in_loop) {
  if (depth == 0 || runif(1) < 0.3) return(gen_leaf(in_loop))
  a <- gen_expr(depth - 1, in_loop)
  b <- gen_expr(depth - 1, in_loop)
  switch(sample(c("+", "-", "*", "sin", "neg", "paren"), 1),
    "+" = paste(a, "+", b),
    "-" = paste(a, "-", b),
    "*" = paste0("(", a, ") * (", b, ")"),
    sin = paste0("sin(", a, ")"),
    neg = paste0("-(", a, ")"),
    paren = paste0("(", a, ")")
  )
}

# branches only reassign existing intermediates: R errors on undefined vars otherwise
gen_stmt <- function(depth) {
  kind <- sample(c("var", "row", "loop", "if"), 1, prob = c(3, 3, 2, if (depth > 0) 2 else 0))
  switch(kind,
    var = paste(sample(fuzz_vars, 1), "<-", gen_expr(3, FALSE)),
    row = sprintf("ydot[[%d]] <- %s", sample(fuzz_n, 1), gen_expr(3, FALSE)),
    loop = sprintf("for (i in 2:(n - 1)) {\n ydot[[i]] <- %s\n}", gen_expr(3, TRUE)),
    "if" = sprintf("if (y[[%d]] > 0) {\n%s\n} else {\n%s\n}", sample(fuzz_n, 1),
                   paste(replicate(2, gen_stmt(depth - 1)), collapse = "\n"),
                   paste(replicate(2, gen_stmt(depth - 1)), collapse = "\n"))
  )
}

gen_rhs <- function() {
  head <- c(
    "n <- length(y)",
    paste(fuzz_vars, "<- 0"),
    "for (i in seq_along(y)) {\n ydot[[i]] <- y[[i]]\n}"
  )
  stmts <- replicate(sample(2:6, 1), gen_stmt(2))
  paste0("function(ydot, y) {\n", paste(c(head, stmts), collapse = "\n"), "\n}")
}

codes <- replicate(300, gen_rhs())
fuzz <- do.call(rbind, lapply(seq_along(codes), function(k) {
  f <- eval(parse(text = codes[[k]]))
  check_case(paste0("fuzz_", k), f, fuzz_n, expect = "any", draws = 5)
}))
cat("fuzz:", sum(fuzz$status == "ok"), "/", nrow(fuzz), "ok,",
    sum(fuzz$msg == "dense"), "dense\n")
for (k in which(fuzz$status != "ok")) {
  cat("\n---", fuzz$case[k], ":", fuzz$msg[k], "\n", codes[[k]], "\n")
}
