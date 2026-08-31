\name{translate}
\alias{translate}
\title{Translate an R function into C++}
\usage{
translate(
  f,
  types_f = NULL,
  output = "R",
  derivative = NULL,
  verbose = FALSE,
  getsource = FALSE,
  debug = TRUE
)
}
\arguments{
  \item{f}{An R function to be translated into C++. Argument types are declared
    with an \code{args(...)} block as the first statement of its body; see the
    Details section.}
  \item{types_f}{A helper function that declares custom struct types via
    \code{new_type()}, for use as argument types, variable types, or slot
    types inside \code{f}. Optional; defaults to \code{NULL}. See the
    Details section.}
  \item{output}{Controls what is returned:
    \itemize{
      \item \code{"R"} (default): an R function wrapping the compiled C++.
      \item \code{"XPtr"}: an external pointer to the compiled C++ function.
    }}
  \item{derivative}{When derivatives are required one can set here the mode of the automatic differentiation:
    \itemize{
      \item \code{NULL} (default): You don't require any derivatives.
      \item \code{"forward"} Determine the derivatives via forward automatic differentiation.
      \item \code{"reverse"} Determine the derivatives via reverse automatic differentiation.
    }}
  \item{verbose}{Logical. If \code{TRUE}, prints output from the compilation process.}
  \item{getsource}{Logical. If \code{TRUE}, returns the generated C++ source code as a string instead of compiling it.}
  \item{debug}{Logical. Defaults to \code{TRUE}. When \code{TRUE}, a runtime
    error is reported as \code{"In '<statement>': <message>"}, naming the
    specific line of translated code that failed. Set to \code{FALSE} to get
    the plain, unattributed message instead.}
}
\value{
  Depending on \code{output}:
  \itemize{
    \item \emph{R}: an R function that directly calls the compiled C++ code.
    \item \emph{XPtr}: a function that returns an external pointer to the compiled C++ code.
  }
}
\description{
  \code{translate()} compiles an R function into C++ code using the
  \emph{ast2ast} expression template library. The result can be called
  directly from R (default) or returned as an external pointer. The C++
  source code can also be retrieved without compilation.
}
\details{
  \subsection{Supported functions}{
    The following R constructs are currently supported:
    \enumerate{
      \item Assignment: \code{=}, \code{<-}, and \code{$} (struct field access/assignment)
      \item Allocation: \code{vector}, \code{matrix}, \code{array}, \code{rep}, \code{logical},
        \code{integer}, \code{numeric}
      \item Object info: \code{length}, \code{dim}, \code{nrow}, \code{ncol}
      \item Arithmetic: \code{+}, \code{-}, \code{*}, \code{/}, \code{\%\%}, \code{\%/\%}, \code{\%*\%}, \code{^}
      \item Indexing: \code{[]} and \code{[[]]}
      \item Math: \code{sin}, \code{asin}, \code{sinh}, \code{cos}, \code{acos}, \code{cosh},
        \code{tan}, \code{atan}, \code{tanh}, \code{sqrt}, \code{abs}, \code{log}, \code{exp},
        \code{floor}, \code{ceiling}, \code{trunc}
      \item Reductions: \code{sum}, \code{prod}, \code{max}, \code{min}, \code{which.max},
        \code{which.min}, \code{all}, \code{any}, \code{rev}
      \item Linear algebra: \code{t}, \code{chol}, \code{crossprod}, \code{tcrossprod}, \code{diag},
        \code{get_diag}, \code{solve}, \code{backsolve}, \code{forwardsolve}, \code{rbind}, \code{cbind}
      \item Root finding / least squares: \code{uniroot}, \code{nnls}
      \item Casts: \code{as.numeric}, \code{as.integer}, \code{as.logical}
      \item Sequences: \code{seq_len}, \code{seq_along}
      \item Concatenation: \code{c}
      \item Control flow: \code{for}, \code{while}, \code{repeat}, \code{next}, \code{break},
        \code{if}, \code{else if}, \code{else}
      \item Comparison: \code{==}, \code{!=}, \code{>}, \code{<}, \code{>=}, \code{<=}
      \item Logical ops: \code{&&}, \code{||}, \code{&}, \code{|}, \code{!}
      \item Printing: \code{print}
      \item Errors: \code{stop}
      \item Return: \code{return}
      \item Catmull–Rom spline: \code{cmr}
      \item Sequence operator: \code{:}
      \item Helpers: \code{is.na}, \code{is.nan}, \code{is.finite}, \code{is.infinite}
      \item Explicit typing: \code{type()}
      \item Derivative functions in forward mode: \code{seed}, \code{unseed}, and \code{get_dot}
      \item Derivative functions in reverse mode: \code{deriv}
      \item Inner functions: \code{fn()}, see below
      \item Custom types: \code{new_type()}, see below
    }
    Types are static in C++ and cannot be changed within the function. Each type consists of a base data type and a data structure:
    \itemize{
      \item Base types: \code{logical}, \code{integer} (or \code{int}), \code{double}
      \item Structures: \code{scalar}, \code{vector} (or \code{vec}), \code{matrix} (or \code{mat})
    }

    Types are usually inferred automatically. Users may annotate explicitly, for example:
    \preformatted{
      a |> type(logical)      # scalar logical
      b |> type(vec(int))     # integer vector
      c |> type(mat(double))  # double matrix
    }
    Scalars in ast2ast differ from R: in R, scalars are length-1 vectors, but in C++ they are true scalars and cannot be subset.
  }
  \subsection{Argument types}{
    Arguments default to \code{matrix(double)} if \code{f}'s body has no
    \code{args(...)} block. To override, make \code{args(...)} the first
    statement of the body, one \code{type()} annotation per argument:
    \preformatted{
      f <- function(a, b, c) {
        args(
          a |> type(borrow_vec(double)) |> ref(),
          b |> type(borrow_mat(double)) |> ref() |> const(),
          c |> type(double) |> ref()
        )
        # ... body ...
      }
    }
    Supported extensions for arguments:
    \itemize{
      \item \code{borrow_vec}, \code{borrow_mat}: pass inputs by reference, modifying them in place.
      \item \code{const()}: disallow modification of inputs.
      \item \code{ref()}: pass by reference (only valid when \code{output = "XPtr"}).
    }
  }
  \subsection{Inner functions}{
    Functions can be defined inside \code{f} with \code{fn()}, which takes three
    positional parts: \code{args(...)} (argument types, same form as the outer
    \code{args(...)} block), \code{return(...)} (the return type), and a
    \code{\{ \}} block (the function body; the braces are optional for a single
    statement). Inner functions may call each other, including recursively, and
    can be passed as values to functions expecting one, such as
    \code{uniroot()}. A non-\code{const()} parameter only binds to a bare
    variable at the call site, not to an arbitrary expression (e.g. \code{x + 1});
    declare the parameter \code{const()} to accept expressions.
    \preformatted{
      factorial <- fn(
        args(a |> type(int) |> const()),
        return(int),
        {
          if (a == 1L) return(a) else return(a * factorial(a - 1L))
        }
      )
    }
    \code{uniroot(f, interval, tol, maxiter)} takes a function of a single
    \code{double} returning a \code{double} (or two arguments if a fifth,
    extra-data argument is supplied to \code{uniroot}), and returns a struct
    with fields \code{root}, \code{f_root}, \code{iter}, and \code{estim_prec}.
    \code{nnls(A, b)} solves the non-negative least squares problem and
    returns the solution vector.
  }
  \subsection{Custom types}{
    User-defined struct types are declared in a \code{types_f} helper function
    (passed to \code{translate()} via \code{types_f =}) and created with
    \code{new_type()} and \code{slots()}:
    \preformatted{
      types_f <- function() {
        new_type(Point, slots(x |> type(double), y |> type(double)))
      }
    }
    On the R side, a value of a custom type is a named list with a matching
    \code{class} attribute, e.g. \code{structure(list(x = 1, y = 2), class = "Point")}.
    Fields are read and written with \code{$}, including chained access such
    as \code{s$circles[[1L]]$center$x}. A slot may itself be a custom type
    (nested structs) or \code{collection(TypeName)}, a vector of a custom
    type; allocate one locally with \code{vector(mode = "TypeName", n)}.
  }
  \subsection{Derivatives}{
    \code{ast2ast} supports automatic differentiation (AD) in two modes: forward
    and reverse. The mode is selected via the \code{derivative} argument of
    \code{translate()}.
    \itemize{
      \item \code{derivative = "forward"} enables forward-mode AD.
      \item \code{derivative = "reverse"} enables reverse-mode AD.
    }
    The AD system is intentionally low-level and explicit. Rather than providing
    high-level wrappers (such as a built-in \code{jacobian()}), users are expected
    to assemble derivative computations themselves using a small set of primitive
    building blocks. This keeps the interface flexible, transparent, and close
    to the generated C++ code.
    \subsection{Forward mode}{
    In forward mode, derivatives are propagated alongside values. The following
    functions are available:
    \itemize{
      \item \code{seed(x, i)}: marks the \code{i}-th component of \code{x} as the
        active direction (sets its derivative to 1).
      \item \code{unseed(x, i)}: resets the derivative state of the \code{i}-th
        component.
      \item \code{get_dot(y)}: extracts the derivative (dot) values of \code{y}.
    }
    A typical pattern is to loop over input dimensions, seed one component at a
    time, evaluate the function, extract derivatives using \code{get_dot()}, and
    assemble the Jacobian manually.
    }
    \subsection{Reverse mode}{
    In reverse mode, derivatives are accumulated by backpropagation from outputs
    to inputs. The function
    \itemize{
      \item \code{deriv(y, x)}
    }
    computes the Jacobian of \code{y} with respect to \code{x}. This call must
    appear explicitly in the translated function. Reverse mode is particularly
    efficient when the number of outputs is small relative to the number of inputs.
    }
    \subsection{Design philosophy}{
    Derivative computation in \code{ast2ast} is explicit by design. The full control
    flow (including loops, seeding, unseeding, and accumulation) is visible in the
    user code and translated directly into C++. This makes the generated code easy
    to inspect, reason about, and modify, and avoids hidden performance costs.
    \strong{Important:} The \code{derivative} argument only enables the AD
    infrastructure. It does not automatically differentiate your function. You
    must explicitly call \code{seed()}, \code{unseed()}, \code{get_dot()}, or
    \code{deriv()} inside your function, depending on the chosen mode.
    }
  }
  \strong{Note:} The generated C++ mimics R semantics closely but not exactly.
  Always validate compiled functions against the original R implementation before
  using in production. See the vignette \emph{Detailed Documentation} for a
  full comparison, and \emph{InformationForPackageAuthors} for internals.
}
\examples{
\dontrun{
# Hello World
# ----------------------------------------------------------------
f <- function() {
  print("Hello World!")
}
f_cpp <- ast2ast::translate(f)
f_cpp()

# Derivatives
# ----------------------------------------------------------------
f <- function(y, x) {
  y[[1L]] <- x[[1L]] * x[[2L]]
  y[[2L]] <- x[[1L]] + x[[2L]]*x[[2L]]
  jac <- deriv(y, x)
  return(jac)
}
fcpp_reverse <- ast2ast::translate(f, derivative = "reverse")
y <- c(0, 0)
x <- c(2, 3)
fcpp_reverse(y, x)

f <- function(y, x) {
  jac <- matrix(0.0, length(y), length(x))
  for (i in 1L:length(x)) {
    seed(x, i)
    y[[1L]] <- x[[1L]] * x[[2L]]
    y[[2L]] <- x[[1L]] + x[[2L]]*x[[2L]]
    d <- get_dot(y)
    jac[TRUE, i] <- d
    unseed(x, i)
  }
  return(jac)
}
fcpp_forward <- ast2ast::translate(f, derivative = "forward")
fcpp_forward(y, x)

# Bubble sort (using [[ for scalars)
# ----------------------------------------------------------------
bubble <- function(a) {
  size <- length(a)
  for (i in 1:size) {
    for (j in 1:(size - 1)) {
      if (a[[j]] > a[[j + 1]]) {
        temp <- a[[j]]
        a[[j]] <- a[[j + 1]]
        a[[j + 1]] <- temp
      }
    }
  }
  return(a)
}
bubble_cpp <- ast2ast::translate(bubble)
bubble_cpp(runif(10))

# Fibonacci sequence
# ----------------------------------------------------------------
fib <- function(n = 10) {
  f <- integer(n)
  f[[1L]] <- 1L
  f[[2L]] <- 1L
  for (i in 3L:n) {
    f[i] <- f[i-1L] + f[i-2L]
  }
  return(f)
}
fib_cpp <- ast2ast::translate(fib)
fib_cpp(10)

# Custom types
# ----------------------------------------------------------------
types_f <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
}
f <- function(p) {
  args(p |> type(Point))
  p$x <- p$x + 1
  return(p)
}
fcpp <- ast2ast::translate(f, types_f = types_f)
p <- structure(list(x = 1, y = 2), class = "Point")
fcpp(p)

# Inner functions and uniroot
# ----------------------------------------------------------------
f <- function(interval) {
  args(interval |> type(vec(double)))
  g <- fn(
    args(x |> type(double)),
    return(double),
    {
      return(x^2 - 4)
    }
  )
  res <- uniroot(g, interval, 1e-10, 1000)
  return(res$root)
}
fcpp <- ast2ast::translate(f)
fcpp(c(0, 10))

# External pointer interface
# ----------------------------------------------------------------
f <- function() {
  print("Hello World from C++")
}
ptr <- ast2ast::translate(f, output = "XPtr")

# Call from C++ side
Rcpp::sourceCpp(code = "
  #include <Rcpp.h>
  typedef void (*fp)();
  // [[Rcpp::export]]
  void call_fct(Rcpp::XPtr<fp> inp) {
    fp f = *inp;
    f();
  }")
call_fct(ptr)

}}
