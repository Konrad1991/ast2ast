## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)

## ---- eval = TRUE-------------------------------------------------------------
f <- function(a) {
  b <- a + 2
  return(b)
}  
library(ast2ast)
f_cpp <- translate(f)

## ---- eval = TRUE-------------------------------------------------------------
call_package(f_cpp)

## ---- eval = TRUE-------------------------------------------------------------
trash <- fct()

## ---- eval = TRUE-------------------------------------------------------------
trash <- fct()

## ---- eval = TRUE-------------------------------------------------------------
trash <- fct()

## ---- eval = TRUE-------------------------------------------------------------
f <- function(a) {
  a <- a + 2
}

library(ast2ast)
fa2a <- translate(f, reference = TRUE)
trash <- call_package(fa2a)

