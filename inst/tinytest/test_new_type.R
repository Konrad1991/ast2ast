library(tinytest)
library(ast2ast)

# --- helpers -----------------------------------------------------------------
build_known_types <- function(types_f, r_fct = TRUE, real_type = "etr::Double") {
  if (is.null(types_f)) return(list())
  types_block <- as.list(ast2ast:::wrap_in_block(body(types_f)))[-1]
  ast2ast:::parse_types(types_block, fct_input = FALSE, r_fct, real_type)
}

get_types <- function(f, types_f = NULL, args_f = NULL, r_fct = TRUE, real_type = "etr::Double") {
  function_registry <- ast2ast:::function_registry_global$clone()
  known_types <- build_known_types(types_f, r_fct, real_type)
  env <- new.env(parent = emptyenv())
  env$r_fct <- r_fct
  env$real_type <- real_type
  env$known_types <- known_types
  ast <- ast2ast:::parse_body(body(f), env, function_registry)
  ast2ast:::update_function_registry(ast, function_registry)
  types <- ast2ast:::infer_types(ast, f, args_f, r_fct, real_type, function_registry, known_types)
  ast2ast:::traverse_ast(ast, ast2ast:::action_transpile_inner_functions, real_type)
  types
}

check_error <- function(f, types_f = NULL, args_f = NULL, pattern, r_fct = TRUE, real_type = "etr::Double") {
  e <- try(get_types(f, types_f, args_f, r_fct, real_type), silent = TRUE)
  expect_true(inherits(e, "try-error"))
  m <- attributes(e)[["condition"]]$message
  expect_true(grepl(pattern, m, fixed = TRUE))
}

check_no_error <- function(f, types_f = NULL, args_f = NULL, r_fct = TRUE, real_type = "etr::Double") {
  e <- try(get_types(f, types_f, args_f, r_fct, real_type), silent = TRUE)
  expect_false(inherits(e, "try-error"))
}

types_f_point <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
}
types_f_area <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
  new_type(Area, slots(x |> type(Point)))
}

# --- 1. struct misuse in binary/unary/function expressions, with and without being assigned ------------------------------------------------------
f <- function() {
  p |> type(Point)
  q <- p
  q + q
  return(q)
}
check_error(f, types_f_point, pattern = "unsupported left type")

f <- function() {
  p |> type(Point)
  q <- p
  z <- q + q
  return(z)
}
check_error(f, types_f_point, pattern = "unsupported left type")

f <- function() {
  p |> type(Point)
  q <- p
  sin(q)
  return(q)
}
check_error(f, types_f_point, pattern = "unsupported type")

f <- function() {
  p |> type(Point)
  q <- p
  z <- sin(q)
  return(z)
}
check_error(f, types_f_point, pattern = "unsupported type")

f <- function() {
  p |> type(Point)
  q <- p
  q$x <- 10
  q + q
  return(q)
}
check_error(f, types_f_point, pattern = "unsupported left type")

# --- 1b. $ misuse ------------------------------------------------------------
f <- function() {
  q <- 5
  return(q$x)
}
check_error(f, pattern = "unsupported left type")

# accessing a field that doesn't exist
f <- function() {
  p |> type(Point)
  return(p$z)
}
check_error(f, types_f_point, pattern = "has no field named z")

# --- 2. functional (compiled) tests for structs -------------------------------
f <- function(p) {
  args(p |> type(Point))
  p$x <- p$x + 10
  return(p)
}
fcpp <- ast2ast::translate(f, types_f = types_f_point)
p_in <- structure(list(x = 1, y = 2), class = "Point")
res <- fcpp(p_in)
expect_equal(res$x, 11)
expect_equal(res$y, 2)
expect_equal(class(res), "Point")

# 2.1 wrong class passed in must be rejected at runtime, not silently accepted
f <- function(p) {
  args(p |> type(Point))
  return(p$x)
}
fcpp <- ast2ast::translate(f, types_f = types_f_point)
wrong <- structure(list(x = 1, y = 2), class = "BLA")
expect_error(fcpp(wrong), pattern = "Expected an object of class 'Point'")

# 2.2 nested struct: the inner value has the wrong class
f <- function(a) {
  args(a |> type(Area))
  return(a$x$x)
}
fcpp <- ast2ast::translate(f, types_f = types_f_area)
bad_inner <- structure(list(x = structure(list(x = 1, y = 2), class = "NotPoint")), class = "Area")
expect_error(fcpp(bad_inner), pattern = "Expected an object of class 'Point'")

# 2.3 correct nested struct: retrieve the inner struct itself as the result
f <- function(a) {
  args(a |> type(Area))
  return(a$x)
}
fcpp <- ast2ast::translate(f, types_f = types_f_area)
good <- structure(list(x = structure(list(x = 3, y = 4), class = "Point")), class = "Area")
res <- fcpp(good)
expect_equal(res$x, 3)
expect_equal(res$y, 4)
expect_equal(class(res), "Point")

# --- 3. type inference for custom types ---------------------------------------
# 3.1 q <- p correctly infers q's type from p
f <- function() {
  p |> type(Point)
  q <- p
  return(q)
}
types <- get_types(f, types_f_point)
expect_true(inherits(types$q, "new_type_node"))
expect_equal(types$q$name, "Point")

# 3.2 once inferred as a custom type, reassigning to a different type is rejected
f <- function() {
  p |> type(Point)
  q <- p
  q <- 3.14
  return(q)
}
check_error(f, types_f_point, pattern = "Cannot reassign variable q")

# 3.3 the same the other way around: scalar first, then a struct
f <- function() {
  p |> type(Point)
  q <- 3.14
  q <- p
  return(q)
}
check_error(f, types_f_point, pattern = "Found incompatible types involving a custom type")

# --- 3b. field-assignment compatibility (q$x <- ...) --------------------------
# double into a double field: fine
f <- function() {
  p |> type(Point)
  p$x <- 3.14
  return(p)
}
check_no_error(f, types_f_point)

# int literal into a double field: fine (legal widening, same as plain declared doubles)
f <- function() {
  p |> type(Point)
  p$x <- 3L
  return(p)
}
check_no_error(f, types_f_point)

# character into a double field: rejected
f <- function() {
  p |> type(Point)
  p$x <- "hello"
  return(p)
}
check_error(f, types_f_point, pattern = "Cannot assign incompatible type to field x")

# a whole struct variable into a scalar field: rejected
f <- function() {
  p |> type(Point)
  p2 |> type(Point)
  p$x <- p2
  return(p)
}
check_error(f, types_f_point, pattern = "Cannot assign incompatible type to field x")

# nested struct field, matching type: fine
f <- function() {
  a |> type(Area)
  p |> type(Point)
  a$x <- p
  return(a)
}
check_no_error(f, types_f_area)

# nested struct field, scalar assigned to a struct-typed field: rejected
f <- function() {
  a |> type(Area)
  a$x <- 3.14
  return(a)
}
check_error(f, types_f_area, pattern = "Cannot assign incompatible type to field x")

# --- 4. const/ref combined with custom types -----------------------------------
# const + ref on a Point argument shows up correctly in the internal (XPtr) signature
fcpp_src <- ast2ast::translate(
  f <- function(p) {
    args(p |> type(Point) |> const() |> ref())
    return(p$x)
  },
  types_f = types_f_point,
  output = "XPtr",
  getsource = TRUE
)
expect_true(grepl("const Point&", fcpp_src, fixed = TRUE))

# a const Point argument rejects field mutation
# (check_fct-level validation, which only runs in the full translate()
# pipeline -- get_types()/check_error() above stop at infer_types() and never
# exercise it, matching test_infer_types.R's own get_types() convention)
f <- function(p) {
  args(p |> type(Point) |> const())
  p$x <- 5
  return(p$x)
}
expect_error(
  ast2ast::translate(
    f,
    types_f = types_f_point,
    getsource = TRUE
  ),
  pattern = "You cannot assign to a constant variable"
)

# a const Point argument also rejects reassignment of the whole variable, not
# just field mutation (same check_fct, different lhs shape)
f <- function(p) {
  args(p |> type(Point) |> const())
  p2 |> type(Point)
  p <- p2
  return(p)
}
expect_error(
  ast2ast::translate(
    f,
    types_f = types_f_point,
    getsource = TRUE
  ),
  pattern = "You cannot assign to a constant variable"
)

# copying a const Point argument into a fresh local must not propagate the
# const-ness to the copy -- previously new_type_node's first-assignment
# branch never reset const_or_mut, so the copy inherited "const" from the
# const argument and any field mutation on the copy was rejected
f <- function(p) {
  args(p |> type(Point) |> const())
  p2 <- p
  p2$x <- 99
  return(p2)
}
fcpp <- ast2ast::translate(f, types_f = types_f_point)
p_in <- structure(list(x = 1, y = 2), class = "Point")
res <- fcpp(p_in)
expect_equal(res$x, 99)
expect_equal(res$y, 2)

# and the copy must be independent -- mutating it must not affect the original
f <- function(p) {
  args(p |> type(Point) |> const())
  p2 <- p
  p2$x <- 99
  return(p)
}
fcpp <- ast2ast::translate(f, types_f = types_f_point)
res <- fcpp(p_in)
expect_equal(res$x, 1)
expect_equal(res$y, 2)

# --- 5. custom types combined with inner functions ------------------------------
# inner fn taking a const-ref Point argument, returning one of its fields
f <- function() {
  p |> type(Point)
  p$x <- 5
  p$y <- 6
  get_x <- fn(
    args(pt |> type(Point) |> const() |> ref()),
    return(double),
    {
      return(pt$x)
    }
  )
  return(get_x(p))
}
fcpp <- ast2ast::translate(f, types_f = types_f_point)
expect_equal(fcpp(), 5)

# inner fn returning a whole struct (not just a field) -- exercises the
# non-r_fct return path, which used to route through etr::Evaluate and would
# not compile for a plain struct; fixed to return the struct directly instead
f <- function() {
  p |> type(Point)
  p$x <- 7
  p$y <- 8
  make_it <- fn(
    args(pt |> type(Point) |> const() |> ref()),
    return(Point),
    {
      return(pt)
    }
  )
  return(make_it(p))
}
fcpp <- ast2ast::translate(f, types_f = types_f_point)
res <- fcpp()
expect_equal(res$x, 7)
expect_equal(res$y, 8)
expect_equal(class(res), "Point")

# length check on the constructor: too few fields must be rejected, not read
# out of bounds
f <- function(p) {
  args(p |> type(Point))
  return(p$x)
}
fcpp <- ast2ast::translate(f, types_f = types_f_point)
short <- structure(list(x = 1), class = "Point")
expect_error(fcpp(short), pattern = "with 2 fields")

# --- 6. for-loop over a struct or a function must be rejected cleanly ---------
f <- function() {
  p |> type(Point)
  s <- 0
  for (i in p) {
    s <- s + 1
  }
  return(s)
}
check_error(f, types_f_point, pattern = "unsupported type")

f <- function() {
  b <- fn(
    args(),
    return(int),
    {
      return(1L)
    }
  )
  s <- 0
  for (i in b) {
    s <- s + 1
  }
  return(s)
}
check_error(f, pattern = "unsupported type")

# --- 7. new_type / slot name validation ----------------------------------------
f <- function() {
  return(1)
}

# a new_type name containing a dot must be rejected
types_f_dotted_name <- function() {
  new_type(data.frame, slots(x |> type(double)))
}
check_error(f, types_f_dotted_name, pattern = "found in data.frame")

# a new_type name that is a C++ keyword must be rejected
types_f_keyword_name <- function() {
  new_type(class, slots(x |> type(double)))
}
check_error(f, types_f_keyword_name, pattern = "Invalid type name: is a C++ keyword")

# a slot name containing a dot must be rejected
types_f_dotted_slot <- function() {
  new_type(Point, slots(x.bad |> type(double)))
}
check_error(f, types_f_dotted_slot, pattern = "found in x.bad")

# a slot name that is a C++ keyword must be rejected
types_f_keyword_slot <- function() {
  new_type(Point, slots(class |> type(double)))
}
check_error(f, types_f_keyword_slot, pattern = "Invalid slot name: is a C++ keyword")

# --- 8. collection misuse -------------------------------------------------------
types_f_point_box <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
  new_type(Circle, slots(x |> type(double)))
  new_type(Box, slots(points |> type(collection(Point))))
}

# arithmetic/math/comparison/logical/reduction on a collection are all rejected
f <- function() {
  coll |> type(collection(Point))
  coll + coll
  return(1)
}
check_error(f, types_f_point_box, pattern = "Found unsupported left type")

f <- function() {
  coll |> type(collection(Point))
  z <- sin(coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unsupported type")

f <- function() {
  coll |> type(collection(Point))
  z <- -coll
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unsupported type")

f <- function() {
  coll |> type(collection(Point))
  z <- !coll
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unsupported type")

f <- function() {
  coll |> type(collection(Point))
  coll == coll
  return(1)
}
check_error(f, types_f_point_box, pattern = "Found unsupported left type")

f <- function() {
  coll |> type(collection(Point))
  coll && TRUE
  return(1)
}
check_error(f, types_f_point_box, pattern = "Found unsupported left type")

f <- function() {
  coll |> type(collection(Point))
  z <- min(coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

f <- function() {
  coll |> type(collection(Point))
  z <- sum(coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

f <- function() {
  coll |> type(collection(Point))
  z <- which.max(coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

# multi-arg (matrix-style) subsetting on a collection is rejected; [[ / at are the
# only permitted subsetting operators (checked elsewhere via the existing
# infer_subsetting binary_node branch)
f <- function() {
  coll |> type(collection(Point))
  z <- coll[1L, 1L]
  return(1)
}
check_error(f, types_f_point_box, pattern = "Found unsupported subsetting")

# reassigning a plain (undeclared) variable from a scalar to a collection is
# rejected -- mirrors test 3.3's struct case, exercised through the same
# common_type() merge path
f <- function() {
  coll |> type(collection(Point))
  q <- 3.14
  q <- coll
  return(q)
}
check_error(f, types_f_point_box, pattern = "Found incompatible types involving a collection")

# assigning a whole collection into a scalar struct field is rejected
f <- function() {
  coll |> type(collection(Point))
  p |> type(Point)
  p$x <- coll
  return(p)
}
check_error(f, types_f_point_box, pattern = "Cannot assign incompatible type to field x")

# assigning a collection of the wrong element type into a collection-typed
# field is rejected
f <- function() {
  wrong |> type(collection(Circle))
  b |> type(Box)
  b$points <- wrong
  return(b)
}
check_error(f, types_f_point_box, pattern = "Cannot assign incompatible type to field points")

# assigning a matching collection into a collection-typed field is fine
f <- function() {
  coll |> type(collection(Point))
  b |> type(Box)
  b$points <- coll
  return(b)
}
check_no_error(f, types_f_point_box)

# length() is allowed on a collection (etr has a dedicated Collection<T>
# overload); nrow/ncol/dim stay rejected since they're for array-likes only.
# The rejection/allowance both live in check_fct, so this has to go through
# the full translate() pipeline too (see the nrow test below).
f <- function() {
  coll |> type(collection(Point))
  n <- length(coll)
  return(n)
}
e <- try(ast2ast::translate(f, types_f = types_f_point_box, getsource = TRUE), silent = TRUE)
expect_false(inherits(e, "try-error"))

# nrow's rejection lives in check_fct, which only runs through the full
# translate() pipeline -- get_types()/check_error() stop at infer_types() and
# never exercise it (see the const-mutation tests above for the same note)
f <- function() {
  coll |> type(collection(Point))
  n <- nrow(coll)
  return(n)
}
expect_error(
  ast2ast::translate(f, types_f = types_f_point_box, getsource = TRUE),
  pattern = "You can only call nrow on variables of type array or matrix"
)

# construction + iteration + [[ ]] element access still work (existing valid
# uses, exercised through the same shared type-inference code paths touched
# above)
f <- function() {
  coll <- vector("Point", 3L)
  s <- 0
  for (pt in coll) {
    s <- s + pt$x
  }
  return(s + length(coll))
}
check_no_error(f, types_f_point_box)

# --- 9. collection misuse in the "systematic sweep" functions -------------------
# c(), :, rev(), rbind(), cbind() used to silently accept a collection and
# fold it into a bogus vector/matrix type instead of rejecting it
f <- function() {
  coll |> type(collection(Point))
  z <- c(coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unexpected type")

f <- function() {
  coll |> type(collection(Point))
  z <- coll:5L
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unsupported left type")

f <- function() {
  coll |> type(collection(Point))
  z <- rev(coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

f <- function() {
  coll |> type(collection(Point))
  z <- rbind(coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unexpected type")

f <- function() {
  coll |> type(collection(Point))
  z <- cbind(coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unexpected type")

# solve()/backsolve()/forwardsolve() used to crash (get_data_struct() fed
# straight into make_inferred_type()'s switch(), which has no "collection"
# case) instead of cleanly rejecting
f <- function() {
  coll |> type(collection(Point))
  z <- solve(coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

f <- function() {
  coll |> type(collection(Point))
  z <- backsolve(coll, coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

f <- function() {
  coll |> type(collection(Point))
  z <- forwardsolve(coll, coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

# cmr() and diag() were already safe from crashing (their check_fct already
# rejected non-matrix/vector types) but silently accepted a collection at
# the infer_fct level
f <- function() {
  coll |> type(collection(Point))
  z <- cmr(coll, coll, coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

f <- function() {
  coll |> type(collection(Point))
  z <- diag(coll, 3L, 3L)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

# matrix()/array() with a collection as the data argument
f <- function() {
  coll |> type(collection(Point))
  z <- matrix(coll, 3L, 3L)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

f <- function() {
  coll |> type(collection(Point))
  z <- array(coll, c(3L, 3L))
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

# as.numeric/as.integer/as.logical on a collection
f <- function() {
  coll |> type(collection(Point))
  z <- as.numeric(coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

f <- function() {
  coll |> type(collection(Point))
  z <- as.integer(coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

f <- function() {
  coll |> type(collection(Point))
  z <- as.logical(coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

# --- 10. remaining shared infer_fct guards not yet exercised --------------------
# infer_and_or_scalar's && was already tested in section 8; infer_and_or_vector
# (&) is a distinct registration and needs its own coverage
f <- function() {
  coll |> type(collection(Point))
  coll & TRUE
  return(1)
}
check_error(f, types_f_point_box, pattern = "Found unsupported left type")

# infer_sum is shared by sum (tested in section 8) and prod
f <- function() {
  coll |> type(collection(Point))
  z <- prod(coll)
  return(z)
}
check_error(f, types_f_point_box, pattern = "Found unallowed type")

# --- 11. common_type()'s fn_node branch (unit-level) -----------------------------
# every DSL-reachable path that could hand a bare fn_node to common_type() is
# intercepted earlier by a more specific check (reassignment has its own
# explicit "You cannot reassign a function..." stop before ever calling
# common_type(); for/while merges never carry a raw fn_node through), so this
# branch has no reachable end-to-end test -- exercise it directly instead
f_with_fn <- function() {
  b <- fn(
    args(),
    return(int),
    {
      return(1L)
    }
  )
  return(1)
}
fn_type <- get_types(f_with_fn)[["b"]]
expect_true(inherits(fn_type, "fn_node"))
scalar_type <- get_types(function() {
  q <- 3.14
  return(q)
})[["q"]]
result <- ast2ast:::common_type(scalar_type, fn_type)
expect_true(is.character(result))
expect_true(grepl("Found incompatible types involving a function", result, fixed = TRUE))

# --- 12. field-assignment: assigning a function value into a struct field -------
f <- function() {
  p |> type(Point)
  get_it <- fn(
    args(),
    return(int),
    {
      return(1L)
    }
  )
  p$x <- get_it
  return(p)
}
check_error(f, types_f_point_box, pattern = "Cannot assign incompatible type to field x")

# --- 13. get_base_type() carries the collection's element type -----------------
# passing a Collection<Circle> where a Collection<Point> is declared must be
# rejected, not silently accepted because both merely say "collection"
f <- function() {
  wrong |> type(collection(Circle))
  get_it <- fn(
    args(c |> type(collection(Point)) |> const() |> ref()),
    return(int),
    {
      return(1L)
    }
  )
  return(get_it(wrong))
}
expect_error(
  ast2ast::translate(f, types_f = types_f_point_box, getsource = TRUE),
  pattern = "has not the correct type"
)

# --- 14. check_subsetting / check_type_declaration full-pipeline fixes ----------
# declaring a collection-typed variable in a function body must not be
# rejected by run_checks()'s type-declaration validation (this used to call
# collection's get_base_type() and reject it as "invalid", independent of
# any actual misuse)
f <- function() {
  coll |> type(collection(Point))
  print(coll)
  return(1)
}
e <- try(ast2ast::translate(f, types_f = types_f_point_box, getsource = TRUE), silent = TRUE)
expect_false(inherits(e, "try-error"))

# [[ ]] / at() on a collection must work through the full pipeline, not just
# infer_types() -- check_subsetting used to reject it even though
# infer_subsetting already allowed it (the exact case that surfaced this bug)
f <- function() {
  pts <- vector(mode = "Point", 5L)
  for (i in seq_len(length(pts))) {
    pts[[i]]$x <- i
    pts[[i]]$y <- i * 2
  }
  total <- 0
  for (p in pts) {
    total <- total + p$x
  }
  return(pts)
}
e <- try(ast2ast::translate(f, types_f = types_f_point_box, getsource = TRUE), silent = TRUE)
expect_false(inherits(e, "try-error"))

# --- 15. functional (compiled + executed) pipeline tests for collections --------
# the exact real-world shape that surfaced the check_subsetting/Collection.hpp
# bugs, run end to end
f <- function() {
  pts <- vector(mode = "Point", 5L)
  for (i in seq_len(length(pts))) {
    pts[[i]]$x <- i
    pts[[i]]$y <- i * 2
  }
  return(pts)
}
fcpp <- ast2ast::translate(f, types_f = types_f_point_box)
res <- fcpp()
expect_equal(length(res), 5L)
expect_equal(sapply(res, function(p) p$x), as.double(1:5))
expect_equal(sapply(res, function(p) p$y), as.double(1:5) * 2)
expect_equal(unique(sapply(res, class)), "Point")

# length() and [[ ]] element access, reducing over the collection
f <- function() {
  pts <- vector(mode = "Point", 3L)
  pts[[1]]$x <- 10
  pts[[2]]$x <- 20
  pts[[3]]$x <- 30
  total <- 0
  n <- length(pts)
  for (i in seq_len(n)) {
    total <- total + pts[[i]]$x
  }
  return(total)
}
fcpp <- ast2ast::translate(f, types_f = types_f_point_box)
expect_equal(fcpp(), 60)

# a collection as a function argument, mutated in place and returned --
# exercises Collection's SEXP constructor and to_SEXP() round trip
f <- function(pts) {
  args(pts |> type(collection(Point)))
  pts[[1]]$x <- pts[[1]]$x + 100
  return(pts)
}
fcpp <- ast2ast::translate(
  f,
  types_f = types_f_point_box
)
p1 <- structure(list(x = 1, y = 2), class = "Point")
p2 <- structure(list(x = 3, y = 4), class = "Point")
res <- fcpp(list(p1, p2))
expect_equal(res[[1]]$x, 101)
expect_equal(res[[2]]$x, 3)
expect_equal(class(res[[1]]), "Point")

# a struct field that is itself a collection, returned to R -- exercises the
# generated Box::to_SEXP(), which used to route collection-typed fields
# through etr::Cast() (no such overload) instead of calling field.to_SEXP()
f <- function() {
  b |> type(Box)
  coll <- vector(mode = "Point", 2L)
  coll[[1]]$x <- 1.0
  coll[[2]]$x <- 2.0
  b$points <- coll
  return(b)
}
fcpp <- ast2ast::translate(f, types_f = types_f_point_box)
res <- fcpp()
expect_equal(class(res), "Box")
expect_equal(length(res$points), 2L)
expect_equal(res$points[[1]]$x, 1.0)
expect_equal(res$points[[2]]$x, 2.0)
expect_equal(sapply(res$points, class), c("Point", "Point"))

# --- 16. nested structures + collections + fn_nodes + const/ref -----------------
# Circle nests a Point (struct-in-struct); Scene holds a collection of Circles
# (collection of structs that themselves have a nested struct field) plus a
# plain scalar field
types_f_nested <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
  new_type(Circle, slots(center |> type(Point), radius |> type(double)))
  new_type(Scene, slots(circles |> type(collection(Circle)), tag |> type(int)))
}

# R -> C++ -> R round trip through a four-level LHS chain
# (s$circles[[i]]$center$x), mutating one element's nested field
f <- function(s) {
  args(s |> type(Scene))
  s$circles[[1]]$center$x <- s$circles[[1]]$center$x + 10
  return(s)
}
fcpp <- ast2ast::translate(f, types_f = types_f_nested)
c1 <- structure(list(center = structure(list(x = 1, y = 2), class = "Point"), radius = 5), class = "Circle")
c2 <- structure(list(center = structure(list(x = 3, y = 4), class = "Point"), radius = 6), class = "Circle")
scene_in <- structure(list(circles = list(c1, c2), tag = 42L), class = "Scene")
res <- fcpp(scene_in)
expect_equal(res$circles[[1]]$center$x, 11)
expect_equal(res$circles[[2]]$center$x, 3)
expect_equal(res$tag, 42L)
expect_equal(class(res), "Scene")
expect_equal(class(res$circles[[1]]), "Circle")
expect_equal(class(res$circles[[1]]$center), "Point")

# inner fn, const + ref, taking a collection and reading a nested field of
# one of its elements
f <- function() {
  circles <- vector(mode = "Circle", 2L)
  circles[[1]]$center$x <- 7.0
  circles[[1]]$center$y <- 8.0
  circles[[1]]$radius <- 2.0
  get_first_x <- fn(
    args(cs |> type(collection(Circle)) |> const() |> ref()),
    return(double),
    {
      return(cs[[1]]$center$x)
    }
  )
  return(get_first_x(circles))
}
fcpp <- ast2ast::translate(f, types_f = types_f_nested)
expect_equal(fcpp(), 7)

# inner fn, const + ref, taking a single nested struct and returning its
# nested struct field
f <- function() {
  circ |> type(Circle)
  circ$center$x <- 3.0
  circ$center$y <- 4.0
  circ$radius <- 1.0
  get_center <- fn(
    args(c |> type(Circle) |> const() |> ref()),
    return(Point),
    {
      return(c$center)
    }
  )
  return(get_center(circ))
}
fcpp <- ast2ast::translate(f, types_f = types_f_nested)
res <- fcpp()
expect_equal(res$x, 3)
expect_equal(res$y, 4)
expect_equal(class(res), "Point")

# inner fn taking a collection by ref() (mutable, no const) actually mutates
# the caller's collection in place
f <- function() {
  coll <- vector(mode = "Point", 2L)
  coll[[1]]$x <- 1.0
  coll[[2]]$x <- 2.0
  bump <- fn(
    args(c |> type(collection(Point)) |> ref()),
    return(int),
    {
      c[[1]]$x <- c[[1]]$x + 100.0
      return(1L)
    }
  )
  bump(coll)
  return(coll)
}
fcpp <- ast2ast::translate(f, types_f = types_f_nested)
res <- fcpp()
expect_equal(res[[1]]$x, 101)
expect_equal(res[[2]]$x, 2)

# const rejects element-field mutation through a collection, mirroring the
# existing const-Point-field-mutation test but for a collection argument
f <- function() {
  coll |> type(collection(Point))
  bad_mut <- fn(
    args(c |> type(collection(Point)) |> const()),
    return(int),
    {
      c[[1]]$x <- 5.0
      return(1L)
    }
  )
  return(bad_mut(coll))
}
expect_error(
  ast2ast::translate(f, types_f = types_f_nested, getsource = TRUE),
  pattern = "You cannot assign to a constant variable"
)

# const + ref on a collection argument shows up correctly in the internal
# (XPtr) signature, mirroring the existing const/ref-on-Point test
fcpp_src <- ast2ast::translate(
  f <- function(cs) {
    args(cs |> type(collection(Circle)) |> const() |> ref())
    return(length(cs))
  },
  types_f = types_f_nested,
  output = "XPtr",
  getsource = TRUE
)
expect_true(grepl("const etr::Collection<Circle>&", fcpp_src, fixed = TRUE))

# --- 17. is_data_structs() through struct-field access ("$") --------------------
# nrow/ncol/dim/chol/crossprod/tcrossprod/get_diag/t/length/subsetting all
# validate their argument via is_vec_mat_or_array()/is_mat()/is_array(), which
# used to trace back through "$" to the struct itself (find_var_lhs() was
# designed to trace back through chained *subsetting*, not field access) and
# see a struct instead of the field's actual (vector/matrix/array) type.
# These are all check_fct-level, so they need the full translate() pipeline,
# not just get_types()/check_error() (same caveat as the earlier nrow test).
types_f_shapes <- function() {
  new_type(Shapes, slots(
    m |> type(matrix(double)),
    v |> type(vector(double)),
    a |> type(array(double))
  ))
}

check_full_pipeline_no_error <- function(f) {
  body(f) <- as.call(c(as.name("{"),
    quote(args(s |> type(Shapes))),
    as.list(body(f))[-1]))
  e <- try(
    ast2ast::translate(f, types_f = types_f_shapes, getsource = TRUE),
    silent = TRUE
  )
  expect_false(inherits(e, "try-error"))
}

f <- function(s) {
  n <- nrow(s$m)
  return(n)
}
check_full_pipeline_no_error(f)

f <- function(s) {
  n <- ncol(s$m)
  return(n)
}
check_full_pipeline_no_error(f)

f <- function(s) {
  d <- dim(s$m)
  return(d)
}
check_full_pipeline_no_error(f)

f <- function(s) {
  r <- chol(s$m)
  return(r)
}
check_full_pipeline_no_error(f)

f <- function(s) {
  r <- crossprod(s$m)
  return(r)
}
check_full_pipeline_no_error(f)

f <- function(s) {
  r <- tcrossprod(s$m)
  return(r)
}
check_full_pipeline_no_error(f)

f <- function(s) {
  r <- get_diag(s$m)
  return(r)
}
check_full_pipeline_no_error(f)

f <- function(s) {
  r <- t(s$m)
  return(r)
}
check_full_pipeline_no_error(f)

f <- function(s) {
  n <- length(s$m)
  return(n)
}
check_full_pipeline_no_error(f)

f <- function(s) {
  n <- length(s$v)
  return(n)
}
check_full_pipeline_no_error(f)

# check_subsetting's binary_node branch ([[, at, [) through a field
f <- function(s) {
  x <- s$v[[1L]]
  return(x)
}
check_full_pipeline_no_error(f)

f <- function(s) {
  x <- s$v[1L]
  return(x)
}
check_full_pipeline_no_error(f)

# check_subsetting's function_node branch (multi-arg subsetting) through a field
f <- function(s) {
  x <- s$m[1L, 1L]
  return(x)
}
check_full_pipeline_no_error(f)

# the root-variable-based checks must still correctly reject a genuine
# non-array/matrix/vector field (regression guard for the "$" fix)
f <- function(s) {
  args(s |> type(Shapes))
  n <- nrow(s$tag)
  return(n)
}
types_f_shapes_with_scalar <- function() {
  new_type(Shapes, slots(
    m |> type(matrix(double)),
    v |> type(vector(double)),
    a |> type(array(double)),
    tag |> type(double)
  ))
}
expect_error(
  ast2ast::translate(f, types_f = types_f_shapes_with_scalar, getsource = TRUE),
  pattern = "You can only call nrow on variables of type array or matrix"
)

# --- printing of new_types ---------------------------------------------

types_f_point <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
}

# 1. printing a plain struct
f <- function() {
  p |> type(Point)
  print(p)
}
fcpp <- ast2ast::translate(f, types_f = types_f_point)
out <- capture.output(fcpp())
expect_true(any(grepl("Point", out)))
expect_true(any(grepl("x:", out)))
expect_true(any(grepl("y:", out)))

# 2. nested printing (a struct containing struct fields)
types_f_nested <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
  new_type(
    Bla,
    slots(
      p1 |> type(Point),
      p2 |> type(Point)
    )
  )
}
f <- function() {
  p |> type(Bla)
  print(p)
}
fcpp <- ast2ast::translate(f, types_f = types_f_nested)
out <- capture.output(fcpp())
expect_true(any(grepl("Bla", out)))
expect_true(any(grepl("p1:", out)))
expect_true(any(grepl("p2:", out)))
expect_equal(sum(grepl("Point", out)), 2L)

# 3. printing a collection of structs
f <- function() {
  c <- vector("Point", 5L)
  print(c)
}
fcpp <- ast2ast::translate(f, types_f = types_f_point)
out <- capture.output(fcpp())
expect_true(any(grepl("Collection\\[5\\]", out)))
expect_equal(sum(grepl("Point", out)), 5L)

# --- struct used as an if-condition -------------------------------------

types_f_awesome <- function() {
  new_type(
    AwesomeClass,
    slots(
      x |> type(double)
    )
  )
}
f <- function() {
  a |> type(AwesomeClass)
  if (a) print("Foo") else print("Bar")
}
expect_error(
  ast2ast::translate(f, types_f = types_f_awesome),
  pattern = "cannot be used as a condition"
)

# --- comparing struct fields via $ in a condition must not be misflagged
# as "struct used as a condition" (only the struct itself is disallowed,
# not a $-access into a scalar field)
f <- function() {
  a |> type(AwesomeClass)
  b |> type(AwesomeClass)
  a$x <- 1
  b$x <- 2
  if (a$x < b$x) print("Foo") else print("Bar")
}
fcpp <- ast2ast::translate(f, types_f = types_f_awesome)
out <- capture.output(fcpp())
expect_true(any(grepl("Foo", out)))

# --- const() is enforced through nested struct/collection/field access -----

types_f_scene <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
  new_type(Circle, slots(center |> type(Point), radius |> type(double)))
  new_type(Scene, slots(circles |> type(collection(Circle)), tag |> type(int)))
}
f <- function(s) {
  args(s |> type(Scene) |> const())
  s$circles[[1L]]$center$x <- s$circles[[1L]]$center$x + 10
  return(s)
}
expect_error(
  ast2ast::translate(f, types_f = types_f_scene, getsource = TRUE),
  pattern = "You cannot assign to a constant variable"
)

# --- ref() lets an inner fn() mutate a struct argument in place ------------

types_f_point <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
}
f <- function(p) {
  args(p |> type(Point))
  mutate_it <- fn(
    args(a |> type(Point) |> ref()),
    return(double),
    {
      a$x <- a$x + 1
      return(a$x)
    }
  )
  r <- mutate_it(p)
  return(p)
}
fcpp <- ast2ast::translate(f, types_f = types_f_point)
p_in <- structure(list(x = 1, y = 2), class = "Point")
expect_equal(fcpp(p_in)$x, 2)

# --- new_type declaration edge cases ----------------------------------------

# 1. duplicated slot names
types_f_dup_slot <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      x |> type(int)
    )
  )
}
f <- function() {
  p |> type(Point)
  return(p$x)
}
expect_error(
  ast2ast::translate(f, types_f = types_f_dup_slot),
  pattern = "Duplicated slot name: x"
)

# 2. empty slots
types_f_empty <- function() {
  new_type(Empty, slots())
}
f <- function() {
  e |> type(Empty)
  return(1)
}
expect_error(
  ast2ast::translate(f, types_f = types_f_empty),
  pattern = "A custom type must have at least one slot"
)

# 3. self-referencing type through collection()
types_f_self_ref <- function() {
  new_type(Node, slots(val |> type(double), children |> type(collection(Node))))
}
f <- function() {
  n |> type(Node)
  return(n$val)
}
expect_error(
  ast2ast::translate(f, types_f = types_f_self_ref, getsource = TRUE),
  pattern = "Self-referencing types are not supported"
)

# 4. new_type redeclaration -- the second declaration must not silently win
types_f_redecl <- function() {
  new_type(Point, slots(x |> type(double)))
  new_type(Point, slots(y |> type(int)))
}
f <- function() {
  p |> type(Point)
  return(p$y) # only the second declaration's slot (y) actually exists
}
expect_error(
  ast2ast::translate(f, types_f = types_f_redecl, getsource = TRUE),
  pattern = "Duplicated type name: Point"
)

# 5. slot name equal to its own type name
types_f_weird <- function() {
  new_type(Weird, slots(Weird |> type(double)))
}
f <- function() {
  w |> type(Weird)
  return(w$Weird)
}
expect_error(
  ast2ast::translate(f, types_f = types_f_weird),
  pattern = "cannot be the same as its own type name"
)

# --- output = "XPtr" with a mutated struct field ----------------------------

types_f_ptr <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
}
f <- function(p) { args(p |> type(Point)); p$x <- p$x + 1; return(p) }
ptr <- ast2ast::translate(
  f, types_f = types_f_ptr,
  output = "XPtr"
)
expect_true(inherits(ptr, "XPtr"))

# --- struct fields are matched by name, not by list position ---------------

types_f_pair <- function() {
  new_type(Pair, slots(y |> type(vec(double)), x |> type(vec(double))))
}
f <- function(p) {
  args(p |> type(Pair))
  return(p)
}
fcpp <- ast2ast::translate(f, types_f = types_f_pair)
# x listed before y here, but slots() declares y before x -- if the
# constructor matched by position instead of name, x and y would be swapped.
p_in <- structure(list(x = c(1, 2, 3), y = c(4, 5, 6)), class = "Pair")
p_out <- fcpp(p_in)
expect_equal(p_out$x |> c(), c(1, 2, 3))
expect_equal(p_out$y |> c(), c(4, 5, 6))

# --- collection used as an if-condition -------------------------------------

f <- function() {
  cs |> type(collection(AwesomeClass))
  if (cs) print("Foo") else print("Bar")
}
expect_error(
  ast2ast::translate(f, types_f = types_f_awesome),
  pattern = "is a collection and cannot be used as a condition"
)

# --- struct used as a while-condition ---------------------------------------

f <- function() {
  a |> type(AwesomeClass)
  while (a) {
    print("Foo")
  }
}
expect_error(
  ast2ast::translate(f, types_f = types_f_awesome),
  pattern = "cannot be used as a condition"
)

# --- passing an uninitialized variable to a fn() argument -------------------

f <- function() {
  square <- fn(
    args(x |> type(double) |> const()),
    return(double),
    { return(x * x) }
  )
  return(square(undeclared_var))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "Found uninitialized variable: undeclared_var"
)

# --- a local variable cannot shadow a custom type's own name ----------------

types_f_shadow <- function() {
  new_type(Point, slots(x |> type(double)))
}
f <- function() {
  p |> type(Point)
  Point <- 5.0
  return(p$x + Point)
}
expect_error(
  ast2ast::translate(f, types_f = types_f_shadow),
  pattern = "is already used as a custom type name"
)

# a field named the same as another declared type is fine (not a shadowing
# variable, just a field access -- exempt via node$field_name). check_no_error
# uses get_types(), which never calls run_checks() -- this check specifically
# lives in run_checks(), so the full translate() pipeline is required here.
types_f_shadow_field <- function() {
  new_type(Circle, slots(x |> type(double)))
  new_type(Shape, slots(Circle |> type(double)))
}
f <- function() {
  s |> type(Shape)
  return(s$Circle)
}
e <- try(ast2ast::translate(f, types_f = types_f_shadow_field), silent = TRUE)
expect_false(inherits(e, "try-error"))

# a struct's own slot name is fine reused as a local variable name -- p$x and
# a bare x are resolved through entirely separate paths (field_name-cached
# type vs. vars_list lookup), so there's no real collision.
types_f_slot_shadow <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
}
f <- function() {
  p |> type(Point)
  p$x <- 1
  p$y <- 2
  x <- 5
  return(p$x + x)
}
fcpp <- ast2ast::translate(f, types_f = types_f_slot_shadow)
expect_equal(fcpp(), 6)
