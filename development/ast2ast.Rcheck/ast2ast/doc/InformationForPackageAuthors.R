## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)

## ---- eval = FALSE, echo = TRUE, attr.source='.numberLines'-------------------
#  
#  bubbleSort <- function(a) {
#  
#    size <- length(a)
#      for (i in 1:size) {
#        for (j in 1:(size - 1)) {
#          if (a[j] > a[j + 1]) {
#            temp <- a[j]
#            a[j] <- a[j + 1]
#            a[j + 1] <- temp
#          }
#        }
#      }
#    return(a)
#  }

## ---- eval = TRUE, echo = TRUE------------------------------------------------
callFct()

## ---- eval = TRUE, echo = TRUE------------------------------------------------
callFct()

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
call_fct()

## ---- eval = TRUE, cache = TRUE-----------------------------------------------
trash <- fct()

## ---- eval = TRUE, cache = TRUE-----------------------------------------------
trash <- fct()

## ---- eval = TRUE, cache = TRUE-----------------------------------------------
f <- function(a) {
  b <- a + 2
  return(b)
}  
library(ast2ast)
f_cpp <- translate(f, output = "XPtr", handle_input = "")

## ---- eval = TRUE-------------------------------------------------------------
call_package(f_cpp)

