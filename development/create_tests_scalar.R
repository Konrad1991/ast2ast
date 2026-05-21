types <- list(
  l1   = list(type = "Logical",       value = TRUE),
  l2   = list(type = "Logical",       value = NA),

  i1   = list(type = "Integer",       value = 2L),
  i2   = list(type = "Integer",       value = NA_integer_),

  d1   = list(type = "Double",        value = 3.0),
  d2   = list(type = "Double",        value = NA_real_),
  d3   = list(type = "Double",        value = NaN),

  dua1 = list(type = "Dual",          value = c(val = 3.0,      dot = 4.0)),
  dua2 = list(type = "Dual",          value = c(val = NA_real_, dot = NA_real_)),
  dua3 = list(type = "Dual",          value = c(val = NA_real_, dot = 4.0)),
  dua4 = list(type = "Dual",          value = c(val = 3.0,      dot = NA_real_)),

  revd1 = list(type = "ReverseDouble", value = 5.0),
  revd2 = list(type = "ReverseDouble", value = NA_real_),
  revd3 = list(type = "ReverseDouble", value = NaN),

  lr_owned1 = list(type = "Logical", value = TRUE),
  lr_owned2 = list(type = "Logical", value = NA),

  ir_owned1 = list(type = "Integer", value = 7L),
  ir_owned2 = list(type = "Integer", value = NA_integer_),

  dr_owned1 = list(type = "Double",  value = 20.0),
  dr_owned2 = list(type = "Double",  value = NA_real_),

  duar_owned1 = list(type = "Dual",  value = c(val = 2.0,      dot = 1.0)),
  duar_owned2 = list(type = "Dual",  value = c(val = NA_real_, dot = NA_real_)),
  duar_owned3 = list(type = "Dual",  value = c(val = 3.0,      dot = NA_real_)),
  duar_owned4 = list(type = "Dual",  value = c(val = NA_real_, dot = 1.0)),

  revdr_owned1 = list(type = "ReverseDouble", value = 9.0),
  revdr_owned2 = list(type = "ReverseDouble", value = NA_real_)
)

convert <- function(entry, res) {
  if (is.nan(res)) {
    res <- paste0(entry$type, "::NaN()")
  } else if (is.na(res)) {
    res <- paste0(entry$type, "::NA()")
  } else {
    res <- paste0("Double(", res, ")")
  }
  res
}

res_sin <- lapply(types, function(entry) {
  if (entry$type == "Dual") {
    val_na <- is.na(entry$value[["val"]])
    dot_na <- is.na(entry$value[["dot"]])
    v <- sin(entry$value[["val"]])
    if (val_na) {
      paste0(entry$type, "::NA()")
    } else if (dot_na) {
      paste0(entry$type, "(", v, ", std::numeric_limits<double>::quiet_NaN())")
    } else {
      paste0(entry$type, "(", v, ", ", cos(entry$value[["val"]]), ")")
    }
  } else {
    res <- sin(entry$value)
    res <- convert(entry, res)
  }
})
res <- paste("compare(sin(", names(types), "), ", res_sin, ")", sep = "", collapse = ";\n")
res <- paste0(res, ";\n")
cat(res)

convert <- function(res) {
  if (is.nan(res)) {
    res <- "Double::NaN()"
  } else if (is.na(res)) {
    res <- "Double::NA()"
  } else {
    res <- paste0("Double(", res, ")")
  }
  res
}
n <- names(types)
combis <- expand.grid(n, n)
# remove reverse double -- Dual combinations
res_mul <- lapply(seq_len(nrow(combis)), function(row_idx) {
  row <- combis[row_idx, ]
  li <- row[[1L]]
  ri <- row[[2L]]
  l <- types[[li]]
  r <- types[[ri]]
  if (l$type != "Dual" && r$type != "Dual") {
    res <- l$value * r$value
    res <- convert(res)
    res <- paste0("compare(",li, " * ", ri, ", ", res, ");\n")
    return(res)
  } else if (l$type == "Dual" && r$type != "Dual") {
    res_val <- l$value[["val"]] * r$value
    res_dot <- l$value[["dot"]] * r$value + l$value[["val"]] * 0.0
  } else if (l$type != "Dual" && r$type == "Dual") {
    res_val <- l$value * r$value[["val"]]
    res_dot <- 0.0 * r$value[["val"]] + l$value * r$value[["dot"]]
  } else if (l$type == "Dual" && r$type == "Dual") {
    res_val <- l$value[["val"]] * r$value[["val"]]
    res_dot <- l$value[["dot"]] * r$value[["val"]] + l$value[["val"]] * r$value[["dot"]]
  }
  if (is.na(res_val)) {
    res <- paste0("compare(",li, " * ", ri, ", ", "Dual::NA()", ");\n")
    return(res)
  }
  if (is.na(res_dot)) {
    res <- paste0("compare(",li, " * ", ri, ", ", "Dual::NA()", ");\n")
    return(res)
  }
  res <- paste0("compare(",li, " * ", ri, ", etr::Dual(", res_val, ", ", res_dot, "));\n")
  res
}) |> unlist()
cat(res_mul)
