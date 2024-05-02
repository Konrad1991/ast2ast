library(ast2ast)
library(tinytest)

result <- Match("+") |>
  when("-", "minus") |>
  when(is.numeric, "bla") |> 
  when(is.character, "plus") |> 
  when(NULL, "not found")
expect_equal(result, "plus")

result <- Match(1L) |>
  when("-", "minus") |>
  when(is.character, "plus") |> 
  when(NULL, "not found")
expect_equal(result, "not found")

predicate <- function(a) {
  return(a)
}
expect_warning(result <- Match("+") |>
  when("-", "minus") |>
  when(is.character, "predicate", TRUE) |> 
  when(NULL, "not found"))

predicate <- function(a) {
  return(paste0(a, "extension"))
}
result <- Match("+") |>
  when("-", "minus") |>
  when(is.character, predicate, TRUE) |> 
  when(NULL, "not found")
expect_equal(result, "+extension")
