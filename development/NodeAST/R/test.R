if (!grepl("R", getwd())) {
  setwd("./R/")
}
files <- list.files(path = ".", full.names = TRUE)
files <- files[files != "./test.R"]
trash <- lapply(files, source)

fct <- function() {
  for (i in 1:10) {
    i <- 2
    print(i)
  }
  for (i in 1:10) {
    print(i)
  }
}
translate(fct) |> cat()

fct <- function() {
  if (a == 1) {
    print(a + 1)
  } else if (a > 1) {
    print(a - 1)
  } else if (a > 2) {
    print(a / 2)
  } else if (a > 3) {
    print(a * 2)
  } else {
    print("bla")
  }
}
fcpp <- translate(fct)
cat(fcpp)
fct <- function() {
  if (a == 1) {
    print(a + 1)
  } else {
    if (a < 2) {
      print(a / 2)
    }
  }
}
fcpp <- translate(fct)
cat(fcpp)

test_literal_translation <- function() {
  stopifnot(determine_literal_type("1") == "numeric")
  stopifnot(determine_literal_type("1.0") == "numeric")
  stopifnot(determine_literal_type("1E04") == "scientific")
  stopifnot(determine_literal_type("1e04") == "scientific")
  stopifnot(determine_literal_type("1e4") == "scientific")
  stopifnot(determine_literal_type("1e-4") == "scientific")
  stopifnot(determine_literal_type("1E-04") == "scientific")
  stopifnot(determine_literal_type("1L") == "integer")
  stopifnot(determine_literal_type("TRUE") == "logical")
  stopifnot(determine_literal_type("T") == "logical")
  stopifnot(determine_literal_type("FALSE") == "logical")
  stopifnot(determine_literal_type("F") == "logical")
  stopifnot(determine_literal_type("text") == "character")
  stopifnot(determine_literal_type("1.2.3") == "character")
  stopifnot(determine_literal_type("1e--4") == "character")
  stopifnot(determine_literal_type("2e") == "character")
}
test_literal_translation()

test_invalid_variable_names <- function() {
  # Dot in variable name
  fct <- function() {
    a.a <- 1
  }
  expect_error(
    translate(fct),
    "a.a <- 1.0; ",
    "Invalid variable name: contains forbidden character --> . found in a.a"
  )
}
test_invalid_variable_names()

test_cpp_reserved_keywords <- function() {
  fct <- function() {
    virtual <- 1
  }
  expect_error(
    translate(fct),
    "virtual <- 1.0; ",
    "Invalid variable name: is a C++ keyword --> virtual"
  )

  fct <- function() {
    class <- 5
  }
  expect_error(
    translate(fct),
    "class <- 5.0; ",
    "Invalid variable name: is a C++ keyword --> class"
  )
}
test_cpp_reserved_keywords()

test_correct_assignments <- function() {
  fct <- function() {
    x <- 5
  }
  expect_correct(translate(fct), "x = 5.0;\n\n")

  fct <- function() {
    y <- 10
  }
  expect_correct(translate(fct), "y = 10.0;\n\n")

  fct <- function() {
    sum <- 3 + 5
  }
  expect_correct(translate(fct), "sum = 3.0 + 5.0;\n\n")

  fct <- function() {
    quotient <- 9 / 3
  }
  expect_correct(translate(fct), "quotient = 9.0 / 3.0;\n\n")
}
test_correct_assignments()

test_type_declarations <- function() {
  # Valid Type Declarations
  fct <- function() {
    x %type% integer
  }
  expect_correct(translate(fct), "\n\n")

  fct <- function() {
    x %type% double <- 1
  }
  expect_correct(translate(fct), "x = 1.0;\n\n")

  # Multiple type declarations
  fct <- function() {
    x %type% double
    x %type% double
  }
  expect_error(
    translate(fct),
    character(),
    c(
      "The variable x is declared with multiple types: ",
      "x %type% double;", "x %type% double;. ",
      "A variable can only have one type declaration."
    )
  )

  fct <- function() {
    x %type% double
    x %type% integer
  }
  expect_error(
    translate(fct),
    character(),
    c(
      "The variable x is declared with multiple types: ",
      "x %type% double;", "x %type% integer;. ",
      "A variable can only have one type declaration."
    )
  )

  # Invalid types
  fct <- function() {
    x %type% d1
  }
  expect_error(
    translate(fct),
    "x %type% d1; ",
    c("Invalid type declaration: d1 for variable x")
  )
  fct <- function() {
    x %type% print(a)
  }
  expect_error(
    translate(fct),
    "x %type% print(a); ",
    c(
      "Invalid type declaration: print(a)"
    )
  )

  fct <- function() {
    print(a) %type% integer
  }
  expect_error(
    translate(fct),
    "print(a) %type% integer; ",
    c(
      "Invalid type declaration for: print(a)"
    )
  )

  fct <- function() {
    print(a) %type% print(a)
  }
  expect_error(
    translate(fct),
    "print(a) %type% print(a); ",
    c(
      "Invalid type declaration for: print(a)"
    )
  )

  fct <- function() {
    1 %type% 2
  }
  expect_error(
    translate(fct),
    "1.0 %type% 2.0; ",
    c(
      "Invalid type declaration for: 1.0"
    )
  )
}
test_type_declarations()



test_functions_operations <- function() {
  fct <- function() {
    bla(10)
  }
  expect_error(
    translate(fct),
    "bla(10.0); ",
    c(
      "Invalid function bla"
    )
  )

  fct <- function() {
    cmr(1)
  }
  expect_error(
    translate(fct),
    "cmr(1.0); ",
    c(
      "Wrong number of arguments for: cmr"
    )
  )

  fct <- function() {
    vector(wrong_arg_name = 10, 2)
  }
  expect_error(
    translate(fct),
    "vector(10.0, 2.0); ",
    c(
      "Found wrong named argument for: vector"
    )
  )

  fct <- function() {
    a[2] <- 3
  }
  expect_correct(
    translate(fct),
    "etr::subset(a, 2) = 3.0;\n\n"
  )

  fct <- function() {
    print(a) <- 2
  }
  expect_error(
    translate(fct),
    "print(a) <- 2.0; ",
    c(
      "Found invalid expression at left side of assignment: <-"
    )
  )

  fct <- function() {
    1 <- print(a)
  }
  expect_error(
    translate(fct),
    "1.0 <- print(a); ",
    c(
      "Found invalid expression at left side of assignment: <-"
    )
  )
}
test_functions_operations()
