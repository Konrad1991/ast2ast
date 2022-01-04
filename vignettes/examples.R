## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
add_two <- function(a) {
  return(a + 2)
}  
pointer_to_f_cpp <- ast2ast::translate(add_two)

