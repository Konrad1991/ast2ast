#rm(list = ls())
#rm(list = c("extractast"))

test_that("seperation of code works", {
  code <- "y <- 1
           x <- y + 1"
  output <- list()
  output[[1]] <- rlang::expr(y <- 1)
  output[[2]] <- rlang::expr(x <- y + 1)
  expect_equal(AstToAst::sep_expressions(code), output)
})



test_that("AST assignment fct", {
  code <- "y <- 1"
  output <- list()
  output[[1]] <- rlang::sym("<-")
  output[[2]] <- rlang::sym("y")
  output[[3]] <- 1
  wrapper <- list()
  wrapper[[1]] <- output
  seperated_code <- AstToAst::sep_expressions(code)
  expect_equal(AstToAst::extractast(seperated_code), wrapper)
})

