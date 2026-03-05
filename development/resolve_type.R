trash <- list.files("~/Documents/ast2ast/R", full.names = TRUE) |> lapply(source)

t <- quote(
  type(a, vec(int)) |> const() |> ref()
)
res <- parse_type_fct_input(t, r_fct = TRUE, real_type = "etr::Double")
res
res$data_struct
res$stringify_error("")
