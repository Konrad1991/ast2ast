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

union_deps <- function(a, b) {
  if (anyNA(a) || anyNA(b)) return(NA_integer_)
  sort(union(a, b))
}

is_subset <- function(node) {
  inherits(node, "binary_node") && node$operator %in% c("[", "[[", "at")
}

is_var <- function(node, name) {
  inherits(node, "variable_node") && node$name == name
}

# traverse_ast but action returning TRUE also skips children of binary/function nodes
traverse_colouring <- function(node, action, ...) {
  if (inherits(node, "variable_node")) {
    action(node, ...)
  } else if (inherits(node, "pre_type_node")) {
    action(node, ...)
  } else if (inherits(node, "new_type_node")) {
    action(node, ...)
  } else if (inherits(node, "binary_node")) {
    handled <- isTRUE(action(node, ...))
    if (!handled) {
      traverse_colouring(node$left_node, action, ...)
      traverse_colouring(node$right_node, action, ...)
    }
  } else if (inherits(node, "unary_node")) {
    action(node, ...)
    traverse_colouring(node$obj, action, ...)
  } else if (inherits(node, "if_node")) {
    action(node, ...)
    traverse_colouring(node$condition, action, ...)
    traverse_colouring(node$true_node, action, ...)
    if (!is.null(node$false_node)) {
      traverse_colouring(node$false_node, action, ...)
    }
    if(!is.null(node$else_if_nodes)) {
      lapply(node$else_if_nodes, function(arg) { traverse_colouring(arg, action, ...) })
    }
  } else if (inherits(node, "block_node")) {
    action(node, ...)
    lapply(node$block, function(stmt) traverse_colouring(stmt, action, ...))
  } else if (inherits(node, "nullary_node")) {
    action(node, ...)
  } else if (inherits(node, "for_node")) {
    handled <- isTRUE(action(node, ...))
    traverse_colouring(node$i, action, ...)
    traverse_colouring(node$seq, action, ...)
    if (!handled) traverse_colouring(node$block, action, ...)
  } else if (inherits(node, "while_node")) {
    handled <- isTRUE(action(node, ...))
    traverse_colouring(node$condition, action, ...)
    if (!handled) traverse_colouring(node$block, action, ...)
  } else if (inherits(node, "repeat_node")) {
    handled <- isTRUE(action(node, ...))
    if (!handled) traverse_colouring(node$block, action, ...)
  } else if (inherits(node, "function_node")) {
    handled <- isTRUE(action(node, ...))
    if (!handled) lapply(node$args, function(arg) traverse_colouring(arg, action, ...))
  } else if (inherits(node, "fn_node")) {
    action(node, ...)
  } else if (inherits(node, "literal_node")) {
    action(node, ...)
  } else {
    stop("Unknown node type: ", class(node))
  }
}

action_deps <- function(node, acc, symbols, state, target) {
  if (is_subset(node) && is_var(node$left_node, state)) {
    if (inherits(node$right_node, "literal_node")) {
      acc$cols <- c(acc$cols, as.integer(node$right_node$name))
    } else {
      acc$na <- TRUE
    }
    return(TRUE)
  }
  # ydot[[j]]: reference, resolved in apply_entries against the rows at that point
  if (is_subset(node) && is_var(node$left_node, target)) {
    if (inherits(node$right_node, "literal_node")) {
      acc$refs <- c(acc$refs, lit_int(node$right_node))
    } else {
      acc$na <- TRUE
    }
    return(TRUE)
  }
  # $ skipped: field names must not collide with intermediates
  if (inherits(node, "binary_node") && node$operator == "$") return(TRUE)
  # matrix indexing of state/target not resolved
  if (inherits(node, "function_node") && node$operator %in% c("[", "[[", "at") &&
      (is_var(node$args[[1]], state) || is_var(node$args[[1]], target))) {
    acc$na <- TRUE
    return(TRUE)
  }
  if (inherits(node, "variable_node")) {
    if (node$name == state || node$name == target) {
      acc$na <- TRUE
    } else if (!is.null(symbols[[node$name]])) {
      d <- union_deps(acc$cols, symbols[[node$name]])
      if (anyNA(d)) acc$na <- TRUE else acc$cols <- d
    }
  }
  FALSE
}

deps <- function(node, symbols, state, target) {
  acc <- new.env(parent = emptyenv())
  acc$cols <- integer(0)
  acc$refs <- integer(0)
  acc$na <- FALSE
  traverse_colouring(node, action_deps, acc, symbols, state, target)
  cols <- if (acc$na) NA_integer_ else sort(unique(acc$cols))
  list(cols = cols, refs = unique(acc$refs))
}

# deparse gives "1L" for integer literals
lit_int <- function(node) as.integer(sub("L$", "", node$name))

is_assign <- function(node) {
  inherits(node, "binary_node") && node$operator %in% c("<-", "=")
}

# coeff * var + offset, or NULL
affine_in <- function(node, var) {
  if (inherits(node, "literal_node")) return(c(0, lit_int(node)))
  if (is_var(node, var)) return(c(1, 0))
  if (inherits(node, "unary_node") && node$operator == "(") return(affine_in(node$obj, var))
  if (inherits(node, "unary_node") && node$operator == "-") {
    a <- affine_in(node$obj, var)
    if (is.null(a)) return(NULL)
    return(-a)
  }
  if (!(inherits(node, "binary_node") && node$operator %in% c("+", "-", "*"))) return(NULL)
  l <- affine_in(node$left_node, var)
  r <- affine_in(node$right_node, var)
  if (is.null(l) || is.null(r)) return(NULL)
  if (node$operator == "+") return(l + r)
  if (node$operator == "-") return(l - r)
  # var * var not affine
  if (l[1] != 0 && r[1] != 0) return(NULL)
  c(l[1] * r[2] + r[1] * l[2], l[2] * r[2])
}

# length(state) or a variable assigned length(state)
is_len <- function(node, state, lens) {
  # name is a symbol; %in% needs character
  if (inherits(node, "variable_node")) return(as.character(node$name) %in% lens)
  inherits(node, "unary_node") && node$operator %in% c("length", "etr::length") &&
    is_var(node$obj, state)
}

# only literals and the state length n; anything else (e.g. input args) -> NULL
bound_fn <- function(node, state, lens) {
  if (inherits(node, "literal_node")) {
    v <- lit_int(node)
    return(function(n) v)
  }
  if (is_len(node, state, lens)) return(function(n) n)
  if (inherits(node, "unary_node") && node$operator == "(") return(bound_fn(node$obj, state, lens))
  if (inherits(node, "unary_node") && node$operator == "-") {
    b <- bound_fn(node$obj, state, lens)
    if (is.null(b)) return(NULL)
    return(function(n) -b(n))
  }
  if (!(inherits(node, "binary_node") && node$operator %in% c("+", "-"))) return(NULL)
  l <- bound_fn(node$left_node, state, lens)
  r <- bound_fn(node$right_node, state, lens)
  if (is.null(l) || is.null(r)) return(NULL)
  if (node$operator == "+") function(n) l(n) + r(n) else function(n) l(n) - r(n)
}

seq_bounds <- function(seq, state, lens) {
  if (inherits(seq, "unary_node") && seq$operator %in% c("seq_along", "etr::seq_along")) {
    if (!is_var(seq$obj, state)) return(NULL)
    return(list(lo = function(n) 1, hi = function(n) n))
  }
  if (inherits(seq, "unary_node") && seq$operator %in% c("seq_len", "etr::seq_len")) {
    hi <- bound_fn(seq$obj, state, lens)
    if (is.null(hi)) return(NULL)
    return(list(lo = function(n) 1, hi = hi))
  }
  if (inherits(seq, "binary_node") && seq$operator == ":") {
    lo <- bound_fn(seq$left_node, state, lens)
    hi <- bound_fn(seq$right_node, state, lens)
    if (is.null(lo) || is.null(hi)) return(NULL)
    # a:b counts down if a > b
    return(list(lo = lo, hi = hi, colon = TRUE))
  }
  NULL
}

# loop twin of action_deps: offsets k of y[[var + k]], fixed cols from outer symbols
action_stencil <- function(node, acc, var, symbols, loc, state, target) {
  if (is_subset(node) && is_var(node$left_node, state)) {
    a <- affine_in(node$right_node, var)
    if (is.null(a)) {
      acc$na <- TRUE
    } else if (a[1] == 1) {
      acc$offsets <- c(acc$offsets, a[2])
    } else if (a[1] == 0) {
      acc$cols <- c(acc$cols, a[2])
    } else {
      acc$na <- TRUE
    }
    return(TRUE)
  }
  # ydot[[var + k]] -> row offset k, ydot[[j]] -> fixed ref j
  if (is_subset(node) && is_var(node$left_node, target)) {
    a <- affine_in(node$right_node, var)
    if (is.null(a)) {
      acc$na <- TRUE
    } else if (a[1] == 1) {
      acc$row_offsets <- c(acc$row_offsets, a[2])
    } else if (a[1] == 0) {
      acc$refs <- c(acc$refs, a[2])
    } else {
      acc$na <- TRUE
    }
    return(TRUE)
  }
  if (inherits(node, "binary_node") && node$operator == "$") return(TRUE)
  if (inherits(node, "function_node") && node$operator %in% c("[", "[[", "at") &&
      (is_var(node$args[[1]], state) || is_var(node$args[[1]], target))) {
    acc$na <- TRUE
    return(TRUE)
  }
  if (inherits(node, "variable_node")) {
    if (node$name == state || node$name == target) {
      acc$na <- TRUE
    } else if (!is.null(loc[[node$name]])) {
      s <- loc[[node$name]]
      if (s$na) acc$na <- TRUE
      acc$offsets <- c(acc$offsets, s$offsets)
      acc$cols <- c(acc$cols, s$cols)
    } else if (!is.null(symbols[[node$name]])) {
      if (anyNA(symbols[[node$name]])) acc$na <- TRUE
      acc$cols <- c(acc$cols, symbols[[node$name]])
    }
  }
  FALSE
}

stencil <- function(node, var, symbols, loc, state, target) {
  acc <- new.env(parent = emptyenv())
  acc$offsets <- integer(0)
  acc$cols <- integer(0)
  acc$row_offsets <- integer(0)
  acc$refs <- integer(0)
  acc$na <- FALSE
  traverse_colouring(node, action_stencil, acc, var, symbols, loc, state, target)
  list(
    offsets = unique(acc$offsets), cols = unique(acc$cols),
    row_offsets = unique(acc$row_offsets), refs = unique(acc$refs), na = acc$na
  )
}

# NULL -> dense
for_bands <- function(stmt, symbols, lens, state, target) {
  sb <- seq_bounds(stmt$seq, state, lens)
  if (is.null(sb)) return(NULL)
  var <- stmt$i$name
  body <- if (inherits(stmt$block, "block_node")) stmt$block$block else list(stmt$block)
  loc <- list()
  bands <- list()
  for (s in body) {
    if (!is_assign(s)) return(NULL)
    lhs <- s$left_node
    st <- stencil(s$right_node, var, symbols, loc, state, target)
    if (inherits(lhs, "variable_node")) {
      # bounds were resolved with the old value
      if (as.character(lhs$name) %in% lens) return(NULL)
      # ydot refs in intermediates would need a snapshot of the rows
      if (length(st$row_offsets) > 0 || length(st$refs) > 0) st$na <- TRUE
      loc[[lhs$name]] <- st
    } else if (is_subset(lhs) && is_var(lhs$left_node, target) &&
               identical(affine_in(lhs$right_node, var), c(1, 0))) {
      bands[[length(bands) + 1]] <- st
    } else {
      return(NULL)
    }
  }
  # one entry per loop: bands applied per iteration, as at runtime
  list(list(type = "loop", lo = sb$lo, hi = sb$hi, colon = isTRUE(sb$colon), bands = bands))
}

# ctx: env with symbols, lens, entries; FALSE -> dense
# entries kept in source order: later writes overwrite earlier rows
walk_stmts <- function(stmts, ctx, state, target) {
  for (stmt in stmts) {
    if (inherits(stmt, "for_node")) {
      bands <- for_bands(stmt, ctx$symbols, ctx$lens, state, target)
      if (is.null(bands)) return(FALSE)
      ctx$entries <- c(ctx$entries, bands)
      next
    }
    if (inherits(stmt, "if_node")) {
      if (!walk_if(stmt, ctx, state, target)) return(FALSE)
      next
    }
    if (!is_assign(stmt)) return(FALSE)
    lhs <- stmt$left_node
    d <- deps(stmt$right_node, ctx$symbols, state, target)
    if (inherits(lhs, "variable_node")) {
      # ydot refs in intermediates would need a snapshot of the rows
      ctx$symbols[[lhs$name]] <- if (length(d$refs) > 0) NA_integer_ else d$cols
      if (is_len(stmt$right_node, state, ctx$lens)) {
        ctx$lens <- union(ctx$lens, as.character(lhs$name))
      } else {
        ctx$lens <- setdiff(ctx$lens, as.character(lhs$name))
      }
    } else if (is_subset(lhs) && is_var(lhs$left_node, target) &&
               inherits(lhs$right_node, "literal_node")) {
      ctx$entries[[length(ctx$entries) + 1]] <- list(
        type = "row", i = lit_int(lhs$right_node), cols = d$cols, refs = d$refs
      )
    } else if (is_subset(lhs) && is_var(lhs$left_node, target) &&
               is_len(lhs$right_node, state, ctx$lens) &&
               inherits(lhs$right_node, "variable_node")) {
      # ydot[[n]]: one-row band, stencil relative to n
      v <- lhs$right_node$name
      st <- stencil(stmt$right_node, v, ctx$symbols, list(), state, target)
      b <- bound_fn(lhs$right_node, state, ctx$lens)
      ctx$entries[[length(ctx$entries) + 1]] <- list(type = "loop", lo = b, hi = b, colon = TRUE, bands = list(st))
    } else {
      return(FALSE)
    }
  }
  TRUE
}

# condition ignored: pattern = union over all branches
walk_if <- function(stmt, ctx, state, target) {
  branches <- c(list(stmt$true_node), lapply(stmt$else_if_nodes, function(e) e$true_node))
  # no else: skipping all branches is a path too
  branches <- c(branches, list(stmt$false_node))
  results <- lapply(branches, function(b) {
    bctx <- new.env(parent = emptyenv())
    bctx$symbols <- ctx$symbols
    bctx$lens <- ctx$lens
    bctx$entries <- list()
    stmts <- if (is.null(b)) list() else b$block
    if (!walk_stmts(stmts, bctx, state, target)) return(NULL)
    bctx
  })
  if (any(vapply(results, is.null, logical(1)))) return(FALSE)
  nms <- unique(unlist(lapply(results, function(r) names(r$symbols))))
  for (nm in nms) {
    ds <- lapply(results, function(r) if (is.null(r$symbols[[nm]])) integer(0) else r$symbols[[nm]])
    ctx$symbols[[nm]] <- Reduce(union_deps, ds)
  }
  # length alias only if it holds on every path
  ctx$lens <- Reduce(intersect, lapply(results, function(r) r$lens))
  ctx$entries[[length(ctx$entries) + 1]] <- list(
    type = "if", branches = lapply(results, function(r) r$entries)
  )
  TRUE
}

row_deps <- function(ast, state, target) {
  ctx <- new.env(parent = emptyenv())
  ctx$symbols <- list()
  ctx$lens <- character(0)
  ctx$entries <- list()
  if (!walk_stmts(ast$block, ctx, state, target)) return(list(dense = TRUE))
  list(entries = ctx$entries, symbols = ctx$symbols, dense = FALSE)
}

# union of the current rows js; unwritten or out of range -> NA
resolve_refs <- function(rows, js, n) {
  out <- integer(0)
  for (j in js) {
    if (j < 1 || j > n || is.null(rows[[j]])) return(NA_integer_)
    out <- union_deps(out, rows[[j]])
  }
  out
}

apply_entries <- function(rows, entries, n) {
  for (e in entries) {
    if (e$type == "row") {
      if (e$i >= 1 && e$i <= n) {
        rows[e$i] <- list(union_deps(e$cols, resolve_refs(rows, e$refs, n)))
      }
    } else if (e$type == "if") {
      outs <- lapply(e$branches, function(b) apply_entries(rows, b, n))
      for (i in seq_len(n)) {
        rs <- lapply(outs, function(o) o[[i]])
        rs <- rs[!vapply(rs, is.null, logical(1))]
        if (length(rs) > 0) rows[i] <- list(Reduce(union_deps, rs))
      }
    } else {
      lo <- e$lo(n)
      hi <- e$hi(n)
      idx <- if (e$colon) lo:hi else if (hi >= lo) lo:hi else integer(0)
      # out-of-range iteration: runtime error / unknown write -> dense
      if (any(idx < 1 | idx > n)) {
        rows[] <- list(NA_integer_)
        next
      }
      for (i in idx) {
        for (b in e$bands) {
          if (b$na) {
            rows[i] <- list(NA_integer_)
            next
          }
          cols <- unique(c(i + b$offsets, b$cols))
          cols <- as.integer(cols[cols >= 1 & cols <= n])
          rows[i] <- list(union_deps(cols, resolve_refs(rows, c(b$refs, i + b$row_offsets), n)))
        }
      }
    }
  }
  rows
}

# NULL -> dense J
materialize <- function(rd, n) {
  if (rd$dense) return(NULL)
  rows <- apply_entries(vector("list", n), rd$entries, n)
  # unfilled row -> dense, never a wrong pattern
  if (any(vapply(rows, is.null, logical(1)))) return(NULL)
  if (any(vapply(rows, anyNA, logical(1)))) return(NULL)
  rows
}

# greedy, natural column order; NULL -> dense J
color_columns <- function(rows, n) {
  if (is.null(rows)) return(NULL)
  conflict <- vector("list", n)
  for (r in rows) {
    for (j in r) conflict[[j]] <- union(conflict[[j]], setdiff(r, j))
  }
  color <- integer(n)
  for (j in seq_len(n)) {
    used <- color[conflict[[j]]]
    k <- 1L
    while (k %in% used) k <- k + 1L
    color[j] <- k
  }
  color
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
