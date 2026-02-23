trash <- list.files("./R", full.names = TRUE) |> lapply(source)
f <- function(a) {
  # ---------- Function bla -------------------------
  bla <- fn(
    args_f = function(a) {
      a |> type(vec(double))
    },
    return_value = type(double),
    block = function(a) {
      return(3.14)
    }
  )
  # ---------- Function bar -------------------------
  bar <- fn(
    args_f = function(obj) {
      obj |> type(double)
    },
    return_value = type(double),
    block = function(obj) {
      return(8)
    }
  )

  # ---------- Function foo -------------------------
  foo <- fn(
    args_f = function(a) a |> type(double),
    return_value = type(double),
    block = function(a) {

  # ---------- inner Function bar --------------------
      bar <- fn(
        args_f = function(obj) {
          obj |> type(double)
        },
        return_value = type(double),
        block = function(obj) {

          inner_inner <- fn(
            args_f = function(obj) {
              obj |> type(logical)
            },
            return_value = type(vec(logical)),
            block = function(obj) {
              print(bla(c(1.1)))
              return(c(TRUE, FALSE))
            }
          )

          inner_inner(TRUE)
          return(9)
        }
      )
      print(bla(c(1.1)))
      return(bar(1))

    }
  )

  return(foo(a[[1L]]))
}
fcpp <- translate(f, getsource = TRUE)
if (is.character(fcpp)) cat(fcpp, "\n")
fcpp <- translate(f)
fcpp(c(1))
# traceback()
