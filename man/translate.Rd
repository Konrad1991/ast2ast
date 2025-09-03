\name{translate}
\alias{translate}
\title{Translate an R function into C++}
\usage{
translate(
  f,
  args_f = NULL,
  output = "R",
  verbose = FALSE,
  getsource = FALSE
)
}
\arguments{
  \item{f}{An R function to be translated into C++.}
  \item{args_f}{A helper function that defines the types of the arguments of \code{f}.
    Optional; defaults to \code{NULL}. See the Details section.}
  \item{output}{Controls what is returned:
    \itemize{
      \item \code{"R"} (default): an R function wrapping the compiled C++.
      \item \code{"XPtr"}: an external pointer to the compiled C++ function.
    }}
  \item{verbose}{Logical. If \code{TRUE}, prints output from the compilation process.}
  \item{getsource}{Logical. If \code{TRUE}, returns the generated C++ source code as a string instead of compiling it.}
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
      \item Assignment: \code{=} and \code{<-}
      \item Allocation: \code{vector}, \code{matrix}, \code{rep}, \code{logical}, \code{integer}, \code{numeric}
      \item Object info: \code{length}, \code{dim}
      \item Arithmetic: \code{+}, \code{-}, \code{*}, \code{/}
      \item Indexing: \code{[]}, \code{[[]]}, and \code{at}
      \item Math: \code{sin}, \code{asin}, \code{sinh}, \code{cos}, \code{acos}, \code{cosh},
        \code{tan}, \code{atan}, \code{tanh}, \code{sqrt}, \code{log}, \code{^}, \code{exp}
      \item Concatenation: \code{c}
      \item Control flow: \code{for}, \code{while}, \code{repeat}, \code{next}, \code{break},
        \code{if}, \code{else if}, \code{else}
      \item Comparison: \code{==}, \code{!=}, \code{>}, \code{<}, \code{>=}, \code{<=}
      \item Logical ops: \code{&&}, \code{||}, \code{&}, \code{|}
      \item Printing: \code{print}
      \item Return: \code{return}
      \item Catmull–Rom spline: \code{cmr}
      \item Sequence operator: \code{:}
      \item Helpers: \code{is.na}, \code{is.finite}, \code{is.infinite}
      \item Explicit typing: \code{type()}
    }
    Types are fixed at compile time:
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
    Arguments default to \code{vec(double)} if \code{args_f} is \code{NULL}.
    To override, supply a function that annotates each argument:
    \preformatted{
      f_args <- function(a, b, c) {
        a |> type(borrow_vec(double)) |> ref()
        b |> type(borrow_mat(double)) |> ref() |> const()
        c |> type(double) |> ref()
      }
    }
    Supported extensions for arguments:
    \itemize{
      \item \code{borrow_vec}, \code{borrow_mat}: pass inputs by reference, modifying them in place.
      \item \code{const()}: disallow modification of inputs.
      \item \code{ref()}: pass by reference (only valid when \code{output = "XPtr"}).
    }
  }

  \strong{Note:} The generated C++ mimics R semantics closely but not exactly.
  Always validate compiled functions against the original R implementation before
  using in production. See the vignette \emph{Detailed Documentation} for a
  full comparison, and \emph{InformationForPackageAuthors} for internals.
}
\examples{
\dontrun{
# 1. Hello World
f <- function() {
  print("Hello World!")
}
f_cpp <- ast2ast::translate(f)
f_cpp()

# 2. External pointer interface
ptr <- ast2ast::translate(f, output = "XPtr", verbose = TRUE)

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

# 3. Run sum (rolling window sum)
run_sum <- function(x, n) {
  sz <- length(x)
  out <- numeric(sz)
  out[n] <- sum(x[1:n])
  for (i in (n+1):sz) {
    out[i] <- out[i-1] + x[i] - x[i-n]
  }
  out[1:(n-1)] <- NA
  out
}
run_sum_cpp <- ast2ast::translate(run_sum)
set.seed(42)
x <- rnorm(10000); n <- 500
all.equal(run_sum(x,n), run_sum_cpp(x,n))

# 4. Bubble sort (using [[ for scalars)
bubble <- function(a) {
  size <- length(a)
  for (i in 1:size) {
    for (j in 1:(size-1)) {
      if (a[j] > a[j+1]) {
        tmp <- a[[j]]
        a[[j]] <- a[[j+1]]
        a[[j+1]] <- tmp
      }
    }
  }
  a
}
bubble_cpp <- ast2ast::translate(bubble)
x <- runif(10)
identical(sort(x), bubble_cpp(x))

# 5. Fibonacci sequence
fib <- function(n = 10) {
  f <- numeric(n)
  f[1] <- 1; f[2] <- 1
  for (i in 3:n) f[i] <- f[i-1] + f[i-2]
  f
}
fib_cpp <- ast2ast::translate(fib)
fib_cpp(10)
}}
