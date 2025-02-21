parms <- c(k1 = 0.04, k2 = 3e7, k3 = 1e4)
vRober_ode <- function(t, y, dy, parms, ps) {
  dy[1] <- -parms["k1"] * y[1] + parms["k3"] * y[2] * y[3]
  dy[2] <- parms["k1"] * y[1] - parms["k2"] * y[2] * y[2] - parms["k3"] * y[2] * y[3]
  dy[3] <- parms["k2"] * y[2] * y[2]
}

traverse <- function(code, string_idx_pairs) {
  if (!is.call(code)) {
    return(code)
  }
  code <- as.list(code)
  if (deparse(code[[1]]) == "[") {
    if (is.character(code[[3]])) {
      code[[3]] <- string_idx_pairs[string_idx_pairs$string == code[[3]], 2]
    }
  }
  lapply(code, function(x) {
    traverse(x, string_idx_pairs)
  })
}

create_calls <- function(code) {
  for (i in seq_along(code)) {
    if (is.list(code[[i]])) {
      code[[i]] <- create_calls(code[[i]])
    }
  }
  code <- as.call(code)
  return(code)
}

remove_strings <- function(f, parms) {
  string_idx_pairs <- data.frame(
    strings =
      names(parms), values = seq_along(parms)
  )
  b <- body(f)
  new_body <- lapply(b[-1], function(expr) {
    expr <- traverse(expr, string_idx_pairs)
    create_calls(expr)
  })
  new_body <- as.call(c(as.name("{"), new_body))
  body(f) <- new_body
  return(f)
}

remove_strings(vRober_ode, parms)
