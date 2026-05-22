setClass(
  "Logical",
  representation(
    val = "logical",
    is_na = "logical"
  )
)
setClass(
  "Integer",
  representation(
    val = "integer",
    is_na = "logical"
  )
)
setClass(
  "Double",
  representation(
    val = "numeric",
    is_na = "logical"
  )
)
setClass(
  "Dual",
  representation(
    val = "numeric",
    dot = "numeric",
    is_na = "logical",
    is_na_dot = "logical"
  )
)
setClass(
  "ReverseDouble",
  representation(
    val = "numeric",
    is_na = "logical"
  )
)

.rank <- c(Logical = 1, Integer = 2, Double = 3, Dual = 4, ReverseDouble = 5)

promote <- function(x, type) {
  if (class(x) == type) return(x)
  v <- as.numeric(x@val)
  na <- x@is_na
  switch(type,
    Logical       = new("Logical", val = as.logical(v), is_na = na),
    Integer       = new("Integer", val = as.integer(v), is_na = na),
    Double        = new("Double", val = v, is_na = na),
    Dual          = new("Dual", val = v, dot = numeric(1L), is_na = na, is_na_dot = FALSE),
    ReverseDouble = new("ReverseDouble", val = v, is_na = na)
  )
}

cast <- function(l, r) {
  type <- names(.rank)[max(.rank[[class(l)]], .rank[[class(r)]])]
  list(promote(l, type), promote(r, type))
}

mul <- function(l, r) {
  ret_type <- names(.rank)[max(.rank[[class(l)]], .rank[[class(r)]])]
  args <- cast(l, r)
  l <- args[[1L]]
  r <- args[[2L]]
  if (ret_type == "Dual") {
    if (l@is_na || r@is_na) {
      new("Dual", val = NaN, dot = NaN, is_na = TRUE, is_na_dot = TRUE)
    } else {
      val <- l@val * r@val
      if (l@is_na_dot || r@is_na_dot) {
        new("Dual", val = val, dot = NaN, is_na = FALSE, is_na_dot = FALSE)
      } else {
        dot <- l@dot * r@val + l@val * r@dot
        new("Dual", val = val, dot = dot, is_na = FALSE, is_na_dot = FALSE)
      }
    }
  }
  else {
    if (l@is_na || r@is_na) {
      if (ret_type %in% c("Double", "ReverseDouble")) {
        val <- NaN
      } else if (ret_type == "Logical") {
        val <- TRUE
      } else if (ret_type == "Integer") {
        val <- 1L
      }
      new(ret_type, val = val, is_na = TRUE)
    } else {
      val <- l@val * r@val
      if (ret_type == "Logical") val <- as.logical(val)
      new(ret_type, val = val, is_na = FALSE)
    }
  }
}

string <- function(obj) {
  if (inherits(obj, "Logical")) {
    sprintf("create_logical(%s, %s)", tolower(obj@val), tolower(obj@is_na))
  }
  else if (inherits(obj, "Integer")) {
    sprintf("create_integer(%s, %s)", obj@val, tolower(obj@is_na))
  }
  else if (inherits(obj, "Double")) {
    sprintf("create_double(%.3f, %s)", obj@val, tolower(obj@is_na))
  }
  else if (inherits(obj, "ReverseDouble")) {
    sprintf("create_reverse_double(%.3f, %s)", obj@val, tolower(obj@is_na))
  }
  else if (inherits(obj, "Dual")) {
    sprintf("create_dual(%.3f, %s, %s, %s)", obj@val, obj@dot, tolower(obj@is_na), tolower(obj@is_na_dot))
  }
}

types <- list(
  l1 = new("Logical", val = TRUE, is_na = FALSE),
  l2 = new("Logical", val = TRUE, is_na = TRUE),
  i1 = new("Integer", val = 2L, is_na = FALSE),
  i2 = new("Integer", val = 1L, is_na = TRUE),
  d1 = new("Double", val = 3.0, is_na = FALSE),
  d2 = new("Double", val = 0.0, is_na = TRUE),
  d2 = new("Double", val = NaN, is_na = FALSE),
  dua1 = new("Dual", val = 3.0, dot = 4.0, is_na = FALSE, is_na_dot = FALSE),
  dua2 = new("Dual", val = 0.0, dot = 0.0, is_na = TRUE, is_na_dot = TRUE),
  dua3 = new("Dual", val = 0.0, dot = 4.0, is_na = TRUE, is_na_dot = FALSE),
  dua4 = new("Dual", val = 3.0, dot = 0.0, is_na = FALSE, is_na_dot = TRUE),
  revd1 = new("ReverseDouble", val = 5.0, is_na = FALSE),
  revd2 = new("ReverseDouble", val = 0.0, is_na = TRUE),
  revd3 = new("ReverseDouble", val = NaN, is_na = FALSE),

  lr_owned1 = new("Logical", val = TRUE, is_na = FALSE),
  lr_owned2 = new("Logical", val = FALSE, is_na = TRUE),
  ir_owned1 = new("Integer", val = 7L, is_na = FALSE),
  ir_owned2 = new("Integer", val = 1L, is_na = TRUE),
  dr_owned1 = new("Double", val = 20.0, is_na = FALSE),
  dr_owned2 = new("Double", val = 0.0, is_na = TRUE),
  duar_owned1 = new("Dual", val = 2.0, dot = 1.0, is_na = FALSE, is_na_dot = FALSE),
  duar_owned2 = new("Dual", val = 0.0, dot = 0.0, is_na = TRUE, is_na_dot = TRUE),
  duar_owned3 = new("Dual", val = 3.0, dot = 0.0, is_na = FALSE, is_na_dot = TRUE),
  duar_owned4 = new("Dual", val = 0.0, dot = 1.0, is_na = TRUE, is_na_dot = FALSE),
  revdr_owned1 = new("ReverseDouble", val = 9.0, is_na = FALSE),
  revdr_owned2 = new("ReverseDouble", val = 0.0, is_na = TRUE)
)
n <- names(types)
combis <- expand.grid(n, n)
indices <- !(grepl("dua", combis$Var1) & grepl("rev", combis$Var2)) &
  !(grepl("rev", combis$Var1) & grepl("dua", combis$Var2))
combis <- combis[indices, ]

string_res <- vapply(seq_len(nrow(combis)), function(r_idx) {
  l_name <- combis[r_idx, 1L, drop = TRUE] |> as.character()
  r_name <- combis[r_idx, 2L, drop = TRUE] |> as.character()
  l <- types[[l_name]]
  r <- types[[r_name]]
  got <- paste0(l_name, " * ", r_name)
  expected <- mul(l, r) |> string()
  sprintf("compare(%s, %s);", got, expected)
}, character(1L))
string_res <- paste0(string_res, collapse = "\n")

pipe <- pipe("xclip -selection clipboard", "w")
writeLines(string_res, pipe)
close(pipe)
