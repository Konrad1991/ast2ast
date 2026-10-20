library(tinytest)
library(ast2ast)

# --- pattern analysis only (no compile) -----------------------------------
ast_of <- function(f) {
  env <- new.env(parent = emptyenv())
  env$r_fct <- TRUE
  env$real_type <- "etr::Double"
  ast2ast:::parse_body(body(f), env, ast2ast:::function_registry_global)
}
pattern <- function(f, n) {
  ast2ast:::materialize(ast2ast:::row_deps(ast_of(f), state = "y", target = "out"), n)
}
# a struct built from the state: s$a depends on all of y -> dense (NULL),
# not an empty row
f <- function(out, y) {
  s <- unpack(y)
  out[[1L]] <- s$a * 2
  out[[2L]] <- y[[2L]]
}
expect_null(pattern(f, 2L))
# a parameter struct does not depend on y
f <- function(out, y) {
  out[[1L]] <- p$a * y[[1L]]
  out[[2L]] <- y[[2L]]
}
expect_equal(pattern(f, 2L), list(1L, 2L))
# integer literal indices
f <- function(out, y) {
  out[[1L]] <- y[[2L]]
  out[[2L]] <- y[[1L]] + y[[2L]]
}
expect_equal(pattern(f, 2L), list(2L, 1:2))

# dense rows: same greedy split as the runtime colouring
arrow <- c(list(1:6), as.list(2:6))
expect_equal(ast2ast:::dense_row_split(arrow, 6L), list(k = 1L, sweeps = 2L))
expect_equal(ast2ast:::dense_row_split(list(1L, 1:2, 2:3), 3L), list(k = 0L, sweeps = 2L))
expect_equal(ast2ast:::dense_row_split(list(1:4, 1:4), 4L), list(k = 2L, sweeps = 2L))
# static colouring stays forward-only: dense rows -> hint to "runtime"
f <- function(x) {
  argtypes(x |> type(vec(double)))
  h <- fn(argtypes(y |> type(vec(double))), return(vec(double)), {
    return(c(y[[1L]] + y[[2L]] + y[[3L]] + y[[4L]], y[[2L]] * y[[2L]], sin(y[[3L]]), y[[4L]]))
  })
  return(jacobian(h, x, colouring = "static", n = 4L))
}
expect_warning(
  ast2ast::translate(f, derivative = "forward", getsource = TRUE),
  "1 dense row; colouring = \"runtime\" needs 2 sweeps instead of 4", fixed = TRUE
)

# Skipped on CRAN: forward AD compiles are expensive.
if (!at_home()) exit_file("slow: AD compiles; runs at home / CI only")

# jacobian(f, x, colouring = "static", n = <literal>): sparsity pattern and column
# colouring of f worked out at translation for length(x) == n; one forward
# sweep per colour. Results must equal the plain jacobian.

f <- function(test, x, s) {
  argtypes(test |> type(int), x |> type(vec(double)), s |> type(double))
  diffusion <- fn(
    argtypes(y |> type(vec(double))),
    return(vec(double)),
    {
      n <- length(y)
      out <- numeric(n)
      out[[1L]] <- y[[2L]] - y[[1L]]
      for (i in 2L:(n - 1L)) {
        out[[i]] <- y[[i - 1L]] - 2 * y[[i]] * y[[i]] + y[[i + 1L]]
      }
      out[[n]] <- y[[n - 1L]] - y[[n]]
      return(out)
    }
  )
  small <- fn(
    argtypes(y |> type(vec(double))),
    return(vec(double)),
    { return(c(y[[1L]] * y[[2L]], y[[2L]] + y[[3L]] * y[[3L]], sin(y[[4L]]))) }
  )
  scaled <- fn(
    argtypes(y |> type(vec(double)), k |> type(double) |> const() |> ref()),
    return(vec(double)),
    { return(c(k * y[[1L]], k * y[[2L]] * y[[3L]], y[[3L]])) }
  )
  if (test == 1L) return(jacobian(diffusion, x, colouring = "static", n = 8L))
  if (test == 2L) return(jacobian(diffusion, x))
  if (test == 3L) return(jacobian(small, x, colouring = "static", n = 4L))
  if (test == 4L) return(jacobian(small, x))
  if (test == 5L) return(jacobian(scaled, x, s, colouring = "static", n = 3L))
  return(jacobian(scaled, x, s))
}

# pattern data is emitted once per call: tridiagonal -> 3 colours
code <- ast2ast::translate(f, derivative = "forward", getsource = TRUE)
cols <- regmatches(code, gregexpr("etr::Colouring\\{[0-9]+UL, [0-9]+UL, [0-9]+UL", code))[[1]]
expect_equal(length(cols), 3L)
expect_true("etr::Colouring{8UL, 8UL, 3UL" %in% cols)
expect_true("etr::Colouring{4UL, 3UL, 2UL" %in% cols)

fcpp <- ast2ast::translate(f, derivative = "forward")
x8 <- seq(0.1, 0.8, length.out = 8)
expect_equal(fcpp(1L, x8, 0), fcpp(2L, x8, 0))
x4 <- c(0.3, 0.7, 1.1, 0.4)
expect_equal(fcpp(3L, x4, 0), fcpp(4L, x4, 0))
x3 <- c(0.3, 0.7, 1.1)
expect_equal(fcpp(5L, x3, 2.5), fcpp(6L, x3, 2.5))
# length(x) has to match n
expect_error(fcpp(1L, x4, 0), "length\\(x\\) differs")

# pattern not resolvable (result reassigned as a whole) -> warning, plain jacobian
g <- function(x) {
  argtypes(x |> type(vec(double)))
  h <- fn(
    argtypes(y |> type(vec(double))),
    return(vec(double)),
    {
      out <- numeric(length(y))
      for (i in seq_along(y)) out[[i]] <- y[[i]] * y[[i]]
      out <- out + y[[1L]]
      return(out)
    }
  )
  return(jacobian(h, x, colouring = "static", n = 3L))
}
expect_warning(gcpp <- ast2ast::translate(g, derivative = "forward"), "colouring not used")
expect_equal(gcpp(c(1, 2, 3)), diag(2 * c(1, 2, 3)) + outer(rep(1, 3), c(1, 0, 0)))

# argument checks
expect_error(ast2ast::translate(g, derivative = "reverse"), "derivative = \"forward\"")
g2 <- function(x) {
  argtypes(x |> type(vec(double)))
  h <- fn(argtypes(y |> type(vec(double))), return(vec(double)), { return(y) })
  return(jacobian(h, x, colouring = "static"))
}
expect_error(ast2ast::translate(g2, derivative = "forward"), "needs the length of x")

# --- jacobian(f, x, colouring = "runtime") ---------------------------------
# pattern from one reverse-mode run of f (functor instantiated with
# ReverseDouble); must equal the
# plain jacobian
r <- function(test, x, s) {
  argtypes(test |> type(int), x |> type(vec(double)), s |> type(double))
  lap <- fn(
    argtypes(y |> type(vec(double))),
    return(vec(double)),
    {
      n <- length(y)
      out <- numeric(n)
      out[[1L]] <- y[[2L]] - 2 * y[[1L]]
      for (i in 2L:(n - 1L)) out[[i]] <- y[[i - 1L]] - 2 * y[[i]] + y[[i + 1L]]
      out[[n]] <- y[[n - 1L]] - 2 * y[[n]]
      return(out)
    }
  )
  rhs <- fn(argtypes(
    y |> type(vec(double))
  ),
    return(vec(double)), { return(lap(y) + y * y) })
  fill <- fn(
    argtypes(out |> type(vec(double)) |> ref(), y |> type(vec(double)) |> ref() |> const()),
    return(void),
    { for (i in seq_len(length(y))) out[[i]] <- exp(y[[i]]) }
  )
  via_ref <- fn(argtypes(y |> type(vec(double))), return(vec(double)), {
    out <- numeric(length(y))
    fill(out, y)
    return(out)
  })
  branch <- fn(argtypes(y |> type(vec(double))), return(vec(double)), {
    if (y[[1L]] > 0) return(c(y[[1L]] * y[[2L]], y[[3L]]))
    return(c(y[[1L]], y[[2L]] * y[[3L]]))
  })
  nested <- fn(argtypes(y |> type(vec(double))), return(vec(double)), {
    sq <- fn(argtypes(v |> type(double) |> const()), return(double), { return(v * v) })
    return(c(sq(y[[1L]]), y[[2L]] + sq(y[[3L]])))
  })
  scaled <- fn(
    argtypes(y |> type(vec(double)), k |> type(double) |> const() |> ref()),
    return(vec(double)),
    { return(c(k * y[[1L]], k * y[[2L]] * y[[3L]], y[[3L]])) }
  )
  if (test == 1L) return(jacobian(rhs, x, colouring = "runtime"))
  if (test == 2L) return(jacobian(rhs, x))
  if (test == 3L) return(jacobian(via_ref, x, colouring = "runtime"))
  if (test == 4L) return(jacobian(via_ref, x))
  if (test == 5L) return(jacobian(branch, x, colouring = "runtime"))
  if (test == 6L) return(jacobian(branch, x))
  if (test == 7L) return(jacobian(nested, x, colouring = "runtime"))
  if (test == 8L) return(jacobian(nested, x))
  if (test == 9L) return(jacobian(scaled, x, s, colouring = "runtime"))
  return(jacobian(scaled, x, s))
}
code <- ast2ast::translate(r, derivative = "forward", getsource = TRUE)
expect_true(grepl("etr::runtime_colouring(r_impl::rhs_t<typename etr::dependent_type<etr::ReverseDouble, ETR_REAL>::type>{})",
                  code, fixed = TRUE))

rcpp <- ast2ast::translate(r, derivative = "forward")
x8 <- seq(0.1, 0.8, length.out = 8)
expect_equal(rcpp(1L, x8, 0), rcpp(2L, x8, 0))
expect_equal(rcpp(3L, x8, 0), rcpp(4L, x8, 0))
# if on x: pattern per branch
x3p <- c(0.3, 0.7, 1.1)
x3n <- c(-0.3, 0.7, 1.1)
expect_equal(rcpp(5L, x3p, 0), rcpp(6L, x3p, 0))
expect_equal(rcpp(5L, x3n, 0), rcpp(6L, x3n, 0))
expect_equal(rcpp(7L, x3p, 0), rcpp(8L, x3p, 0))
expect_equal(rcpp(9L, x3p, 2.5), rcpp(10L, x3p, 2.5))

# extra data as vector: Array<Dual> -> Array<ReverseDouble> for the tape pass
rw <- function(test, x, w) {
  argtypes(test |> type(int), x |> type(vec(double)), w |> type(vec(double)))
  weighted <- fn(
    argtypes(y |> type(vec(double)), v |> type(vec(double)) |> const() |> ref()),
    return(vec(double)),
    { return(v * y * y) }
  )
  if (test == 1L) return(jacobian(weighted, x, w, colouring = "runtime"))
  return(jacobian(weighted, x, w))
}
rwcpp <- ast2ast::translate(rw, derivative = "forward")
w3 <- c(2, 3, 4)
expect_equal(rwcpp(1L, x3p, w3), rwcpp(2L, x3p, w3))

# bidirectional: dense rows via reverse sweeps, rest coloured
rd <- function(test, x, s) {
  argtypes(test |> type(int), x |> type(vec(double)), s |> type(double))
  # row 1 depends on all of y, the rest is diagonal
  dense_row <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(vec(double))
    out <- y * y
    out[[1L]] <- sum(y)
    return(out)
  }
  # only dense rows: no forward sweep left
  two_rows <- function(y, k) {
    argtypes(y |> type(vec(double)), k |> type(double) |> const() |> ref())
    returntype(vec(double))
    return(c(k * sum(y), sum(y * y)))
  }
  if (test == 1L) return(jacobian(dense_row, x, colouring = "runtime"))
  if (test == 2L) return(jacobian(dense_row, x))
  if (test == 3L) return(jacobian(two_rows, x, s, colouring = "runtime"))
  return(jacobian(two_rows, x, s))
}
rdcpp <- ast2ast::translate(rd, derivative = "forward")
x20 <- seq(0.1, 2, length.out = 20)
expect_equal(rdcpp(1L, x20, 0), rdcpp(2L, x20, 0))
expect_equal(rdcpp(1L, x20, 0), diag(2 * x20) + rbind(c(1 - 2 * x20[[1L]], rep(1, 19)), matrix(0, 19, 20)))
expect_equal(rdcpp(3L, x20, 2.5), rdcpp(4L, x20, 2.5))
expect_equal(rdcpp(3L, x20, 2.5), rbind(rep(2.5, 20), 2 * x20))

# argument checks
r2 <- function(x) {
  argtypes(x |> type(vec(double)))
  h <- fn(argtypes(y |> type(vec(double))), return(vec(double)), { return(y) })
  return(jacobian(h, x, colouring = "runtime", n = 3L))
}
expect_error(ast2ast::translate(r2, derivative = "forward"), "n is only used")
r3 <- function(x) {
  argtypes(x |> type(vec(double)))
  return(jacobian(
    fn(
      argtypes(
        y |> type(vec(double))
      ),
      return(vec(double)),
      { return(y) }
    ),
    x, colouring = "runtime")
  )
}
expect_error(ast2ast::translate(r3, derivative = "forward"), "named fn")
r6 <- function(x) {
  argtypes(x |> type(vec(double)))
  h <- fn(argtypes(y |> type(vec(double))), return(vec(double)), { return(y) })
  return(jacobian(h, x, colouring = "runtime"))
}
expect_error(ast2ast::translate(r6, derivative = "reverse"), "derivative = \"forward\"")
# f is instantiated with ReverseDouble: forward-/reverse-only code is rejected
r7 <- function(x) {
  argtypes(x |> type(vec(double)))
  h <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(vec(double))
    seed(y, 1L)
    return(y)
  }
  return(jacobian(h, x, colouring = "runtime"))
}
expect_error(ast2ast::translate(r7, derivative = "forward"), "calls seed")
r8 <- function(x) {
  argtypes(x |> type(vec(double)))
  g <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(vec(double))
    return(get_dot(y))
  }
  h <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(vec(double))
    return(g(y))
  }
  return(jacobian(h, x, colouring = "runtime"))
}
expect_error(ast2ast::translate(r8, derivative = "forward"), "calls get_dot")
r9 <- function(x) {
  argtypes(x |> type(vec(double)))
  h <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(vec(double))
    inner <- function(v) {
      argtypes(v |> type(borrow_vec(double)))
      returntype(vec(double))
      return(v)
    }
    return(inner(y))
  }
  return(jacobian(h, x, colouring = "runtime"))
}
expect_error(ast2ast::translate(r9, derivative = "forward"), "borrow arguments")
# a fn not reachable from f may use them
r10 <- function(x) {
  argtypes(x |> type(vec(double)))
  g <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(vec(double))
    return(get_dot(y))
  }
  h <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(vec(double))
    return(y * y)
  }
  print(g(x))
  return(jacobian(h, x, colouring = "runtime"))
}
expect_true(is.character(ast2ast::translate(r10, derivative = "forward", getsource = TRUE)))
# custom types: struct built from the state + struct as extra data
# (Params<Dual> -> Params<ReverseDouble> via the converting ctor)
types_f_state <- function() {
  new_type(Halves, slots(a |> type(vec(double)), b |> type(vec(double))))
  new_type(Params, slots(k |> type(double)))
  new_type(Point, slots(x |> type(double), y |> type(double)))
}
st <- function(test, x, p) {
  argtypes(test |> type(int), x |> type(vec(double)), p |> type(Params))
  unpack <- fn(argtypes(y |> type(vec(double))), return(Halves), {
    h |> type(Halves)
    h$a <- y[1L:2L]
    h$b <- y[3L:4L]
    return(h)
  })
  rhs <- fn(
    argtypes(y |> type(vec(double)), q |> type(Params) |> const() |> ref()),
    return(vec(double)),
    {
      h <- unpack(y)
      return(c(q$k * h$a * h$b, h$a[[1L]] + h$b))
    }
  )
  # collection of structs inside f
  coll <- fn(argtypes(y |> type(vec(double))), return(vec(double)), {
    pts <- vector(mode = "Point", 2L)
    pts[[1L]]$x <- y[[1L]] * y[[2L]]
    pts[[2L]]$x <- y[[3L]]
    return(c(pts[[1L]]$x, pts[[2L]]$x + y[[4L]]))
  })
  # print of a struct inside f (also instantiated with ReverseDouble)
  printed <- fn(argtypes(y |> type(vec(double))), return(vec(double)), {
    h <- unpack(y)
    print(h)
    return(h$a * h$b)
  })
  if (test == 1L) return(jacobian(rhs, x, p, colouring = "runtime"))
  if (test == 2L) return(jacobian(rhs, x, p))
  if (test == 3L) return(jacobian(coll, x, colouring = "runtime"))
  if (test == 4L) return(jacobian(coll, x))
  return(jacobian(printed, x, colouring = "runtime"))
}
code <- ast2ast::translate(st, types_f = types_f_state, derivative = "forward", getsource = TRUE)
expect_true(grepl("Params(const Params<ETR_REAL_OTHER>& o)", code, fixed = TRUE))
stcpp <- ast2ast::translate(st, types_f = types_f_state, derivative = "forward")
p <- structure(list(k = 2.5), class = "Params")
x4 <- c(0.3, 0.7, 1.1, 0.4)
expect_equal(stcpp(1L, x4, p), stcpp(2L, x4, p))
expect_equal(stcpp(3L, x4, p), stcpp(4L, x4, p))
out <- capture.output(J <- stcpp(5L, x4, p))
expect_equal(J, rbind(c(x4[[3L]], 0, x4[[1L]], 0), c(0, x4[[4L]], 0, x4[[2L]])))
expect_true(any(grepl("Halves", out)))

# collection of structs as extra data: converted element-wise for the tape pass
sc <- function(test, x, pts) {
  argtypes(test |> type(int), x |> type(vec(double)), pts |> type(collection(Point)))
  shifted <- fn(
    argtypes(y |> type(vec(double)), ps |> type(collection(Point)) |> const() |> ref()),
    return(vec(double)),
    { return(c(ps[[1L]]$x * y[[1L]], ps[[2L]]$y * y[[2L]] * y[[3L]])) }
  )
  if (test == 1L) return(jacobian(shifted, x, pts, colouring = "runtime"))
  return(jacobian(shifted, x, pts))
}
sccpp <- ast2ast::translate(sc, types_f = types_f_state, derivative = "forward")
pts <- list(
  structure(list(x = 1.5, y = 2), class = "Point"),
  structure(list(x = 3, y = 0.5), class = "Point")
)
x3 <- c(0.3, 0.7, 1.1)
expect_equal(sccpp(1L, x3, pts), sccpp(2L, x3, pts))

# --- without translate(derivative = ...): f alone runs in forward mode --------
# the struct passed as data is converted once (Params<Double> -> Params<Dual>)
pl <- function(test, x, p) {
  argtypes(test |> type(int), x |> type(vec(double)), p |> type(Params))
  rhs <- function(y, q) {
    argtypes(y |> type(vec(double)), q |> type(Params) |> const() |> ref())
    returntype(vec(double))
    out <- q$k * y * y
    out[[1L]] <- sum(y)
    return(out)
  }
  if (test == 1L) return(jacobian(rhs, x, p))
  return(jacobian(rhs, x, p, colouring = "runtime"))
}
code <- ast2ast::translate(pl, types_f = types_f_state, getsource = TRUE)
expect_true(grepl("etr::jacobian(pl_impl::rhs_t<typename etr::dependent_type<etr::Dual, ETR_REAL>::type>{}",
                  code, fixed = TRUE))
plcpp <- ast2ast::translate(pl, types_f = types_f_state)
expected <- diag(2 * 2.5 * x4)
expected[1L, ] <- 1
expect_equal(plcpp(1L, x4, p), expected)
expect_equal(plcpp(2L, x4, p), expected)
# jacobian inside a fn functor: f_t<Dual> must not need f_t complete there
pw <- function(x) {
  argtypes(x |> type(vec(double)))
  rhs <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(vec(double))
    out <- y * y
    out[[1L]] <- sum(y)
    return(out)
  }
  wrap <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(mat(double))
    return(jacobian(rhs, y, colouring = "runtime") + jacobian(rhs, y))
  }
  return(wrap(x))
}
expected <- diag(2 * x4)
expected[1L, ] <- 1
expect_equal(ast2ast::translate(pw)(x4), 2 * expected)

# non-finite derivative although f is finite: error with the line where it starts
nf <- function(test, x) {
  argtypes(test |> type(int), x |> type(vec(double)))
  mm <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(vec(double))
    out <- y * y
    out[[1L]] <- 1 / (1 + 1 / y[[1L]])
    return(out)
  }
  undefined <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(vec(double))
    return(y / (y - y))
  }
  if (test == 1L) return(jacobian(mm, x))
  if (test == 2L) return(jacobian(mm, x, colouring = "runtime"))
  return(jacobian(undefined, x))
}
nfcpp <- ast2ast::translate(nf)
x_tiny <- c(1e-200, 0.5, 0.7)
expect_error(nfcpp(1L, x_tiny), "d f\\[1\\] / d x\\[1\\] is not finite.*out\\[\\[1L\\]\\] <- 1/\\(1 \\+ 1/y\\[\\[1L\\]\\]\\)")
expect_error(nfcpp(2L, x_tiny), "is not finite, although f\\[1\\] is")
# f itself NaN: the jacobian is NaN too, as in R
expect_true(all(is.nan(nfcpp(3L, c(0.3, 0.7)))))

pl_error <- function(body_f) {
  g <- function(x) {
    argtypes(x |> type(vec(double)))
    h <- H
    return(jacobian(h, x))
  }
  body(g) <- do.call(substitute, list(body(g), list(H = body_f)))
  g
}
h_borrow <- quote(function(y) {
  argtypes(y |> type(vec(double)))
  returntype(vec(double))
  inner <- function(v) {
    argtypes(v |> type(borrow_vec(double)))
    returntype(vec(double))
    return(v)
  }
  return(inner(y))
})
expect_error(ast2ast::translate(pl_error(h_borrow), getsource = TRUE), "has borrow arguments")
h_sort <- quote(function(y) {
  argtypes(y |> type(vec(double)))
  returntype(vec(double))
  return(sort(y))
})
expect_error(ast2ast::translate(pl_error(h_sort), getsource = TRUE), "calls sort")
r_inline <- function(x) {
  argtypes(x |> type(vec(double)))
  return(jacobian(fn(argtypes(y |> type(vec(double))), return(vec(double)), { return(y) }), x))
}
expect_error(ast2ast::translate(r_inline, getsource = TRUE), "has to be a named fn")

r5 <- function(x) {
  argtypes(x |> type(vec(double)))
  h <- fn(argtypes(y |> type(vec(double))), return(vec(double)), { return(y) })
  return(jacobian(h, x, colouring = "dynamic"))
}
expect_error(ast2ast::translate(r5, derivative = "forward"), "\"static\" or \"runtime\"")
