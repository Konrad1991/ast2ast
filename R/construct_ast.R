library(rlang)


# ===============================================================
# find T or F

expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

switch_expr <- function(x, ...) {
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}


logical_abbr_rec <- function(x) {

  switch_expr(x,
              # Base cases
              constant = FALSE,
              symbol = as_string(x) %in% c("F", "T"),
              
              # Recursive cases
              call = ,
              pairlist = purrr::some(x, logical_abbr_rec)
  )
}
logical_abbr <- function(x) {
  logical_abbr_rec(enexpr(x))
}



# ===============================================================
# find assignement

find_assign_rec <- function(x) {
  switch_expr(x,
              constant = ,
              symbol = character()
  )
}
find_assign <- function(x) find_assign_rec(enexpr(x))

find_assign("x")
#> character(0)
find_assign(x)
#> character(0)


flat_map_chr <- function(.x, .f, ...) {
  purrr::flatten_chr(purrr::map(.x, .f, ...))
}

flat_map_chr(letters[1:3], ~ rep(., sample(3, 1)))
#> [1] "a" "b" "b" "b" "c" "c" "c"

find_assign_rec <- function(x) {
  switch_expr(x,
              # Base cases
              constant = ,
              symbol = character(),
              
              # Recursive cases
              pairlist = flat_map_chr(as.list(x), find_assign_rec),
              call = {
                if (is_call(x, "<-")) {
                  as_string(x[[2]])
                } else {
                  flat_map_chr(as.list(x), find_assign_rec)
                }
              }
  )
}

find_assign(a <- 1)
#> [1] "a"
find_assign({
  a <- 1
  {
    b <- 2
  }
})
#> [1] "a" "b"


# ===============================================================
# find plus

find_plus_rec <- function(x) {
  switch_expr(x,
              constant = ,
              symbol = character()
  )
}
find_plus <- function(x) find_plus_rec(enexpr(x))

find_plus("x")
#> character(0)
find_plus(x)
#> character(0)


flat_map_chr <- function(.x, .f, ...) {
  purrr::flatten_chr(purrr::map(.x, .f, ...))
}

flat_map_chr(letters[1:3], ~ rep(., sample(3, 1)))
#> [1] "a" "b" "b" "b" "c" "c" "c"

find_plus_rec <- function(x) {
  switch_expr(x,
              # Base cases
              constant = ,
              symbol = character(),
              
              # Recursive cases
              pairlist = flat_map_chr(as.list(x), find_plus_rec),
              call = {
                if (is_call(x, "+")) {
                  #as_string(x[[2]])
                  if(is_double(x[[2]]) ) {
                    x[[2]] <- as.character(x[[2]])
                  } else if(is_double(x[[3]])) {
                    x[[3]] <- as.character(x[[3]])
                  }
                  
                  left <- as_string(x[[2]])
                  right <- as_string(x[[3]])
                  c(left, right)
                } else {
                  flat_map_chr(as.list(x), find_plus_rec)
                }
              }
  )
}

a <- find_assign(a + b)
a

find_assign({
  a <- b + d -3 - 5
  {
    b <- c + 3
  }
})

# Construct function which returns ast --> each function has to be found!
# Furthermore, the order of each function call has to be determined for each expression

# checks needed for entire program (all expressions)?

# scope: one fct. User has to define type of each argument. (... is not possible)
# scope: datatypes allowed: double, logical, character, integer as scalar, vector or matrix. 
# scope: later type data.frame and list
# scope: later function definition within function