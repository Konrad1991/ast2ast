\name{translate}
\alias{translate}
\title{Translates an R function into a C++ function.}
\usage{
  translate(
    f,
    output = "R",
    types_of_args = "SEXP",
    return_type = "SEXP",
    reference = FALSE,
    verbose = FALSE
  )
}
\arguments{
  \item{f}{The function which should be translated from R to C++.}
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
      \item Indices: \emph{'[]'} and at
    \item mathematical functions: sin, asin, sinh, cos, acos, cosh, tan, atan, tanh, sqrt, log, ^ and exp
    \item concatenate objects: c
    \item control flow: for, if, else if, else
      \item comparison: ==, !=, >, <, >= and <=
      \item printing: print
    \item returning objects: return
    \item catmull-rome spline: cmr
    \item to get a range of numbers the ':' function can be used
    \item is.na and is.infinite can be used to test for NA and Inf.
    \item d-, p-, q- and r-unif, -norm, -lnorm and -gamma (for gamma argument \emph{Scale} cannot be defined and is calculated using \emph{1/rate})
  }
  \strong{Some details about the implemented functions}
  \itemize{
    \item allocation of memory works: Following forms are possible:
      \itemize{
        \item vector(size_of_elements)
        \item vector(value, size_of_elements)
        \item matrix(nrows, ncols)
        \item matrix(value, nrows, ncols)
        \item matrix(vector, nrows, ncols)
      }
    \item For indices squared brackets \emph{'[]'} can be used as common in R. Beyond that the function 'at' exists
    which accepts as first argument a variable and as the second argument you pass the desired index.
    The caveat of using 'at' is that only \strong{one} entry can be accessed. The function \emph{'[]'} can return more then one element. \cr
    \strong{The \emph{at}-function returns a reference to the vector entry.
      Therefore variable[index] can behave differently then \emph{at(variable, index)}.
      The function has to be used carefully when \emph{at} is used.
      Especially if \emph{'[]'} and \emph{at} are mixed the function behaviour is difficult to predict.
      Please test it before using it in a serious project.}  \cr
    Here is a small example presented how to use the subset functions:
      \verb{
        f <- function() {
          a <- c(1, 2, 3)
          print(at(a, 1))
          print(a[1:2])
        }
        fcpp <- ast2ast::translate(f)
        fcpp()
      }
    \item For-loops can be written as common in R
    \itemize{
      \item Nr.1 \cr
      for(index in variable)\{ \cr
        # do whatever \cr
        \} \cr
      \item Nr.2 \cr
      for(index in 1:length(variable)\{ \cr
        # do whatever \cr
        \} \cr
        }
    \item Be aware that it is possible to assign the result of a comparison to a variable. Example see below: \cr
    However, the vector will contain only \emph{0} or \emph{1} instead of \emph{FALSE} or \emph{TRUE}.
    \verb{
      a = c(1, 2, 3)
      b = c(1, 2, 1)
      c = a != b
    }
    \item The print function accepts either a scalar, vector, matrix, string, bool or nothing (empty line).
    \item In order to return an object use the \emph{return} function (\strong{The last object is not returned automatically as in R}).
    \item In order to interpolate values the \emph{cmr} function can be used. The function needs three arguments.
    \enumerate{
      \item the first argument is the point of the independent variable (\strong{x}) for which the dependent variable should be calculated (\strong{y}). This has to be a vector of length one.
      \item the second argument is a vector defining the points of the independent variable (\strong{x}). This has to be a vector of at least length four.
      \item the third argument is a vector defining the points of the dependent variable (\strong{y}). This has to be a vector of at least length four.
    }
    \strong{Be aware that the R code is translated to ETR an expression template library which tries to mimic R. \cr
      However, it does not behave exactly like R! Please check your compiled function before using it in a serious project.
      If you want to see how \emph{ast2ast} differs from R in detail check the vignette: \emph{Detailed Documentation}.}
  }
}
\examples{ # Further examples can be found in the vignettes. 
    \dontrun{
      # Hello World
      # ==========================================================================
      
      # Translating to R_fct
      # --------------------------------------------------------------------------
      f <- function() { print("Hello World!")}
      ast2ast::translate(f)
      f()
      
      # Translating to external pointer
      # --------------------------------------------------------------------------
      f <- function() { print("Hello World!")}
      pointer_to_f_cpp <- ast2ast::translate(f, 
                                             output = "XPtr", return_type = "void")
      Rcpp::sourceCpp(code = '
      #include <Rcpp.h>
      typedef void (*fp)();

      // [[Rcpp::export]]
      void call_fct(Rcpp::XPtr<fp> inp) {
        fp f = *inp;
        f(); } ')
        
      call_fct(pointer_to_f_cpp)
      
      # Run sum example:
      # ==========================================================================
      
      # R version of run sum
      # --------------------------------------------------------------------------
      run_sum <- function(x, n) {
        sz <- length(x)
        
        ov <- vector(mode = "numeric", length = sz)
        
        ov[n] <- sum(x[1:n])
        for(i in (n+1):sz) {
          
          ov[i] <- ov[i-1] + x[i] - x[i-n]
        }
        
        ov[1:(n-1)] <- NA
        
        return(ov)
      }
      
      # translated Version of R function
      # --------------------------------------------------------------------------
      run_sum_fast <- function(x, n) {
        sz <- length(x)
        ov <- vector(sz)
        
        sum_db = 0
        for(i in 1:n) {
          sum_db <- sum_db + at(x, i)
        }
        ov[n] <- sum_db
        
        for(i in (n + 1):sz) {
          ov[i] <- at(ov, i - 1) + at(x, i) - at(x, i - at(n, 1))
        }
        
        ov[1:(n - 1)] <- NA
        
        return(ov)
      }
      run_sum_cpp <- ast2ast::translate(run_sum_fast, verbose = FALSE)
      set.seed(42)
      x <- rnorm(10000)
      n <- 500
      one <- run_sum(x, n)
      two <- run_sum_cpp(x, n)
    }
}