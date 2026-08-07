# Declarations
# ------------------------------------------
# basic data types: logical, integer, double
# data structures: scalar, vector, matrix, array
#
#
# Signatures for XPtrs or inner functions
# ------------------------------------------
# basic data types: logical, integer, double
# data structures: scalar, vector, matrix, array, borrow_vec, borrow_mat, and borrow_array
# const or mutable
# copy or by reference
#
#
# Signatures for R functions
# ------------------------------------------
# basic data types: logical, integer, double
# data structures: scalar, vector, matrix, array, borrow_vec, borrow_mat, and borrow_array
source("./development/TypeParsing/HelperTypeParser.R")
source("./development/TypeParsing/TypeNodes.R")
source("./development/TypeParsing/TypeParser.R")

f <- function() {
  new_type(
    "cool_type",
    slots(
      a |> type(int),
      b |> type(double),
      c |> type(vec(double))
    )
  )
  new_type(
    "awesome_type",
    slots(
      ct |> type(cool_type),
      m |> type(matrix(double))
    )
  )
  a |> type(cool_type)
  b |> type(collection(awesome_type))
  c |> type(int)
  d |> type(vec(int))
  e |> type(borrow_vec(int))
}
res <- parse_types(body(f)[-1], TRUE, TRUE, "etr::Double")
res$cool_type$define_type() |> cat("\n")
res$awesome_type$define_type() |> cat("\n")

res$a$declare("  ") |> cat("\n")
res$b$declare("  ") |> cat("\n")
res$c$declare("  ") |> cat("\n")
res$d$declare("  ") |> cat("\n")
res$d$signature("  ") |> cat("\n")
res$e$declare("  ") |> cat("\n")
