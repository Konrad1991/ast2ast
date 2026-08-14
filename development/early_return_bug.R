files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

types_f <- function() {
  new_type(
    Result,
    slots(
      total_err |> type(double)
    )
  )
}

# ---- repro: struct field name collides with a later plain local ----------
colliding_fn <- function(x) {
  out |> type(Result)

  if (x < 0) {
    out$total_err <- -1
    return(out)
  }

  total_err <- 0 # first assignment of a PLAIN local named like the field,
  # after the early return() above
  total_err <- total_err + x

  out$total_err <- total_err
  return(out)
}

args_f_colliding <- function(x) {
  x |> type(double)
}

# ---- control: same shape, but no name collision ---------------------------
non_colliding_fn <- function(x) {
  out |> type(Result)

  if (x < 0) {
    out$total_err <- -1
    return(out)
  }

  acc <- 0 # same pattern, but named differently from the struct field
  acc <- acc + x

  out$total_err <- acc
  return(out)
}

args_f_non_colliding <- function(x) {
  x |> type(double)
}

cat("---- colliding case (expected to fail per hypothesis) ----\n")
colliding_a2a <- tryCatch(
  translate(colliding_fn, args_f = args_f_colliding, types_f = types_f),
  error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    NULL
  }
)
if (!is.null(colliding_a2a)) {
  cat("Translated fine. colliding_a2a(5) =", colliding_a2a(5)$total_err, "\n")
}

cat("\n---- control case (expected to work) ----\n")
non_colliding_a2a <- tryCatch(
  translate(non_colliding_fn, args_f = args_f_non_colliding, types_f = types_f),
  error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    NULL
  }
)
if (!is.null(non_colliding_a2a)) {
  cat("Translated fine. non_colliding_a2a(5) =", non_colliding_a2a(5)$total_err, "\n")
}
