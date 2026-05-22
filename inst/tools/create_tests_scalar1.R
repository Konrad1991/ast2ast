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
# cat(res)
