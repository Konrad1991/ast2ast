\name{J}
\alias{J}
\title{Calculates the jacobian function and translatesthe resulting function into a C++ function.}
\usage{
  J(
    f,
    y,
    x,
    output = "R",
    types_of_args = "SEXP",
    return_type = "SEXP",
    reference = FALSE,
    verbose = FALSE,
    getsource = FALSE
  )
}
\arguments{
  \item{f}{The function which should be translated from R to C++.}
  \item{y}{The variables to compute the derivatives of (the dependent variable). For example: df/dx}
  \item{x}{The variables to which respect the variables are calcualted (the independent variable). For example: df/dx}
  \item{output}{If set to "R"" an R function wrapping the C++ code is returned. \cr
    If output is set to "XPtr"" an external pointer object pointing to the C++ code is returned. \cr
    The default value is "R".}
  \item{types_of_args}{define the types of the arguments passed to the function as an character vector. 
    This is an optional input if using "XPtr" as output. \cr
    The default value is "SEXP" as this is the only possibility for output "R". \cr
    In case one want to use an external pointer the easiest way is to pass "sexp" for types_of_args. \cr
    Beyond that it is possible to pass "double", "ptr_vec" and "ptr_mat".
    For more information see below for details and check the vignette \emph{InformationForPackageAuthors}.
    \strong{Beyond that, be aware that the passed \emph{SEXP} objects are only copied if the object size increases.
      Thus, R objects can be modified within the function! See in section details for an example} \cr}
  \item{return_type}{is a character defining the type which the function returns. The default value is "SEXP"" as this is the only possibility for output "R". \cr
    Additionally, the possibilities "sexp" and "void" exist for the external pointer interface.}
  \item{reference}{If set to TRUE the arguments are passed by reference (not possible if output is "R").}
  \item{verbose}{If set to TRUE the output of the compilation process is printed.}
  \item{getsource}{If set to TRUE the function is not compiled and instead the C++ source code itself is returned.}
}
\value{
  If output is set to \emph{R} an R function is returned. Thus, the C++ code can directly be called within R. \cr
  In contrast a function which returns an external pointer is generated if the output is set to \emph{XPtr}. \cr
}
\description{
  An R function is translated to C++ source code and afterwards the code is compiled. \cr
  The result can be an external pointer (\emph{XPtr}) or an \emph{R} function. \cr
  The default value is an R function. \cr
  Further information can be found in the vignette: \emph{Detailed Documentation}. \cr
}
\details{
  \strong{The types \emph{numeric vector} and \emph{numeric matrix} are supported.
    Notably, it is possible that the variables change the type within the function. }\cr
  \strong{Beyond that, be aware that the passed \emph{SEXP} objects are only copied if the size increases. Thus, R objects can be modified within the function! \cr
    For example in the following code the variable \emph{a} contains \emph{1, 2, and 3} before the function call and afterwards \emph{1, 1 and 1}.
    In contrast for variable \emph{b} the size changes and thus the object within \emph{R} is not modified. Furthermore, the variable \emph{c} is not increased and only the first element is         changed.} \cr
  \verb{
    f <- function(a, b, c) { 
      a[c(1, 2, 3)] <- 1 
      b <- vector(10) 
      c <- vector(1)
    } 
    fcpp <- ast2ast::translate(f) 
    a <- c(1, 2, 3) 
    b <- c(1, 2, 3) 
    c <- c(1, 2, 3)
    fcpp(a, b,c) 
    print(a) 
    print(b) 
    print(c)
  }
  \strong{It is possible to declare a variable of a scalar numeric data type.
    This is done by adding \emph{_db} to the end of the variable. Each time \emph{_db} is found
    the variable is declared as a scalar numeric data type. In this case the
    object cannot change its type! In the example below the variable \emph{a_db} is of type double whereas \emph{b} is of type "sexp".} \cr
  \verb{
    f <- function() {
      a_db = 3.14
      b = 3.14
    }
    fcpp <- ast2ast::translate(f, verbose = TRUE)
    fcpp()
  } \cr
  In R every object is under the hood a \emph{SEXP} object.
  In case an \emph{R} function is created as output only \emph{SEXP} elements can be passed to the function. 
  Furthermore, these functions always return a \emph{SEXP} element. Even if nothing is returned; in this case \emph{NULL} is returned!. 
  Notably, is that only numeric vectors (in R also scalar values are vectors) or numeric matrices can be passed to the function. \cr
  In contrast if an external pointer is created other types can be specified which are passed to the function or returned from it. 
  The default value is a variable of type \emph{sexp}. This is the data type which is used in the C++ code. 
  The \emph{ptr_vec} and \emph{ptr_mat} interface work in a different way. If using \emph{ptr_vec} a \emph{double*} pointer is expected as first element.
  Additionally a second argument is needed which is of type \emph{int} and which defines the size of the array. 
  This works in the same way for \emph{ptr_mat}. But instead of the size argument two integers are needed which define the number of rows and columns.
  Both arguments have to be of type \emph{int}. 
  Notably, the memory is only borrowed. Thus, the memory is not automatically deleted! See vignette \emph{InformationForPackageAuthors} for more information. \cr
  \strong{The following functions are supported:}
  \enumerate{
    \item assignment: = and <-
      \item allocation: vector and matrix
    \item information about objects: length and dim
    \item Basic operations: +, -, *, /
      \item Indices: \emph{'[]'}. \strong{The function 'at' cannot be used! Beyond that only integer values are allowed within the brackets.}
    \item mathematical functions: sin, asin, sinh, cos, acos, cosh, tan, atan, tanh, sqrt, log, ^ and exp
    \item concatenate objects: c
    \item control flow: for, if, else if, else
      \item comparison: ==, !=, >, <, >= and <=
      \item printing: print
    \item returning objects: return
    \item catmull-rome spline: cmr
    \item to get a range of numbers the ':' function can be used
    \item is.na and is.infinite can be used to test for NA and Inf.
  }
  
  For more details see: \code{\link[dfdr:jacobian]{dfdr::jacobian()}}
}
\examples{ # Further examples can be found in the vignettes. 
  \dontrun{
      # simple example
      f <- function(y) {
        ydot <- vector(length = 2)
        a <- 1.1
        b <- 0.4
        c <- 0.1
        d <- 0.4
        ydot[1] <- y[1]*a - y[1]*y[2]*b
        ydot[2] <- y[2]*y[1]*c - y[2]*d
        return(ydot)
      }
      jac <- ast2ast::J(f, ydot, y, verbose = TRUE)
      jac(c(10, 11))
  }
}