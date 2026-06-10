files <- list.files("./R", full.names = TRUE)
trash <- lapply(files, source)
f <- function() {
  bar2 <- fn(
    f_args = function(a) {
      a |> type(double)
    },
    return_value = type(vec(double)),
    block = function(a) {
      return(c(1, 2, 3))
    }
  )
  foo <- fn(
    f_args = function(x) {
      x |> type(int)
    },
    return_value = type(int),
    block = function(x) {
      bar <- fn(
        f_args = function(a) {
          a |> type(double)
        },
        return_value = type(vec(double)),
        block = function(a) {
          return(c(1, 2, 3))
        }
      )
      h <- 3.14
      j <- 1L
      asdf <- bar2(h)
      return(3L)
    }
  )
  x <- 4L
  bla <- foo(x)
  a <- 3.14
  bla <- bar2(a)
  print(bla)
}
fcpp <- translate(f, verbose = TRUE)
fcpp()


x <- 3.14
trunc(x)
floor(x)
