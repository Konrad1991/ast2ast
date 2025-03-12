# TODO: Tests
# Tests for iterations
# Tests for all binary nodes
# Tests for all unary nodes
# Tests for all nullary nodes
# Tests for parsing function inputs
# TODO:
# Handle arguments of function in variables
# --> Prepare Variables instance for that
library(ast2ast)

test_function_inputs <- function() {
  fct <- function(a, b, c) {
    a |> type(double_vector)
    a <- c(1, 2, 3)
    a[b] <- c
  }

  res <- translate(
    f = fct,
    args = c(
      test,
      a |> type(double_vector) |> copy(),
      b |> type(logical) |> reference(),
      c
    )
  )
  pa <- parse_input_args(res[[1]])
  check_parsed_args(pa, TRUE)

  res <- translate(
    f = {
      a |> type(double_vector)
      a <- c(1, 2, 3)
      a[b] <- c
    },
    args = c(
      a |> type(double_vector) |> copy() |> const(),
      b |> type(logical) |> reference() |> const(),
      c
    )
  )
  pa <- parse_input_args(res[[1]])
  str(pa)
  check_parsed_args(pa, FALSE)
}

test_for_loop <- function() {
  print("test for loop")
  fct <- function() {
    for (i in 1:10) {
      print(i)
    }
  }
  ast2ast:::expect_correct(
    ast2ast:::translate_internally(fct),
    "for(auto& i : etr::colon(1, 10)) {\n  etr::print(i);\n}\n\n\n"
  )
  fct <- function() {
    for (i in 1:10) {
      i <- 1
      print(i)
    }
  }
}
test_for_loop()

test_if_blocks <- function() {
  print("test if blocks ")
  fct <- function() {
    if (a == 5) {
      print(a)
    }
  }
  ast2ast:::expect_correct(
    ast2ast:::translate_internally(fct),
    "if (a == 5.0) {\n  etr::print(a);\n}\n\n"
  )
  fct <- function() {
    if (a == 5) {
      print(a)
    } else {
      print(b)
    }
  }
  ast2ast:::expect_correct(
    ast2ast:::translate_internally(fct),
    "if (a == 5.0) {\n  etr::print(a);\n} else {\n  etr::print(b);\n}\n\n"
  )
  fct <- function() {
    if (a == 5) {
      print(a)
    } else if (a > 2) {
      print(b)
      print(c)
    }
  }
  ast2ast:::expect_correct(
    ast2ast:::translate_internally(fct),
    "if (a == 5.0) {\n  etr::print(a);\n} else if (a > 2.0) {\n  print(b);\n  print(c);\n}\n\n"
  )
  fct <- function() {
    if (a == 5) {
      print(a)
    } else if (a > 2) {
      print(b)
      print(c)
    } else {
      print(1234)
    }
  }
  ast2ast:::expect_correct(
    ast2ast:::translate_internally(fct),
    "if (a == 5.0) {\n  etr::print(a);\n} else if (a > 2.0) {\n  print(b);\n  print(c);\n} else {\n  etr::print(1234);\n}\n\n"
  )
}
test_if_blocks()

test_literal_translation <- function() {
  print("test literal translation")
  stopifnot(ast2ast:::determine_literal_type("1") == "numeric")
  stopifnot(ast2ast:::determine_literal_type("1.0") == "numeric")
  stopifnot(ast2ast:::determine_literal_type("1E04") == "scientific")
  stopifnot(ast2ast:::determine_literal_type("1e04") == "scientific")
  stopifnot(ast2ast:::determine_literal_type("1e4") == "scientific")
  stopifnot(ast2ast:::determine_literal_type("1e-4") == "scientific")
  stopifnot(ast2ast:::determine_literal_type("1E-04") == "scientific")
  stopifnot(ast2ast:::determine_literal_type("1L") == "integer")
  stopifnot(ast2ast:::determine_literal_type("TRUE") == "logical")
  stopifnot(ast2ast:::determine_literal_type("T") == "logical")
  stopifnot(ast2ast:::determine_literal_type("FALSE") == "logical")
  stopifnot(ast2ast:::determine_literal_type("F") == "logical")
  stopifnot(ast2ast:::determine_literal_type("text") == "character")
  stopifnot(ast2ast:::determine_literal_type("1.2.3") == "character")
  stopifnot(ast2ast:::determine_literal_type("1e--4") == "character")
  stopifnot(ast2ast:::determine_literal_type("2e") == "character")
}
test_literal_translation()

test_invalid_variable_names <- function() {
  print("test invalid variable names")
  # Dot in variable name
  fct <- function() {
    a.a <- 1
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    "a.a <- 1.0; ",
    "Invalid variable name: contains forbidden character --> . found in a.a"
  )
}
test_invalid_variable_names()

test_cpp_reserved_keywords <- function() {
  print("test cpp keywords usage")
  fct <- function() {
    virtual <- 1
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    "virtual <- 1.0; ",
    "Invalid variable name: is a C++ keyword --> virtual"
  )

  fct <- function() {
    class <- 5
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    "class <- 5.0; ",
    "Invalid variable name: is a C++ keyword --> class"
  )
}
test_cpp_reserved_keywords()

test_correct_assignments <- function() {
  print("test correct assignments")
  fct <- function() {
    x <- 5
  }
  ast2ast:::expect_correct(ast2ast:::translate_internally(fct), "x = 5.0;\n\n")

  fct <- function() {
    y <- 10
  }
  ast2ast:::expect_correct(ast2ast:::translate_internally(fct), "y = 10.0;\n\n")

  fct <- function() {
    sum <- 3 + 5
  }
  ast2ast:::expect_correct(ast2ast:::translate_internally(fct), "sum = 3.0 + 5.0;\n\n")

  fct <- function() {
    quotient <- 9 / 3
  }
  ast2ast:::expect_correct(ast2ast:::translate_internally(fct), "quotient = 9.0 / 3.0;\n\n")
}
test_correct_assignments()

test_type_declarations <- function() {
  print("test type declarations")
  # Valid Type Declarations
  fct <- function() {
    x |> type(integer)
  }
  ast2ast:::expect_correct(ast2ast:::translate_internally(fct), "\n\n")

  fct <- function() {
    x |> type(double) <- 1
  }
  ast2ast:::expect_correct(ast2ast:::translate_internally(fct), "x = 1.0;\n\n")

  # Multiple type declarations
  fct <- function() {
    x |> type(double)
    x |> type(double)
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    character(),
    c(
      "The variable x is declared with multiple types: ",
      "type(x, double);", "type(x, double);. ",
      "A variable can only have one type declaration."
    )
  )

  fct <- function() {
    x |> type(double)
    x |> type(integer)
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    character(),
    c(
      "The variable x is declared with multiple types: ",
      "type(x, double);", "type(x, integer);. ",
      "A variable can only have one type declaration."
    )
  )

  # Invalid types
  fct <- function() {
    x |> type(d1)
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    "type(x, d1); ",
    c("Invalid type declaration: d1 for variable x")
  )
  fct <- function() {
    x |> type(print(a))
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    "type(x, print(a)); ",
    c(
      "Invalid type declaration: print(a)"
    )
  )

  fct <- function() {
    print(a) |> type(integer)
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    "type(print(a), integer); ",
    c(
      "Invalid type declaration for: print(a)"
    )
  )

  fct <- function() {
    print(a) |> type(print(a))
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    "type(print(a), print(a)); ",
    c(
      "Invalid type declaration for: print(a)"
    )
  )

  fct <- function() {
    1 |> type(2)
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    "type(1.0, 2.0); ",
    c(
      "Invalid type declaration for: 1.0"
    )
  )
}
test_type_declarations()

test_functions_operations <- function() {
  print("test function operations")
  fct <- function() {
    bla(10)
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    "bla(10.0); ",
    c(
      "Invalid function bla"
    )
  )

  fct <- function() {
    cmr(1)
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    "cmr(1.0); ",
    c(
      "Wrong number of arguments for: cmr"
    )
  )

  fct <- function() {
    vector(wrong_arg_name = 10, 2)
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    "vector(10.0, 2.0); ",
    c(
      "Found wrong named argument for: vector"
    )
  )

  fct <- function() {
    a[2] <- 3
  }
  ast2ast:::expect_correct(
    ast2ast:::translate_internally(fct),
    "etr::subset(a, 2) = 3.0;\n\n"
  )

  fct <- function() {
    print(a) <- 2
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    "print(a) <- 2.0; ",
    c(
      "Found invalid expression at left side of assignment: <-"
    )
  )

  fct <- function() {
    1 <- print(a)
  }
  expect_error(
    ast2ast:::translate_internally(fct),
    "1.0 <- print(a); ",
    c(
      "Found invalid expression at left side of assignment: <-"
    )
  )
}
test_functions_operations()
