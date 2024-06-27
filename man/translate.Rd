\name{translate}
\alias{translate}
\title{Translates an R function into a C++ function.}
\usage{
  translate(
    f,
    output = "R",
    types_of_args = "double",
    data_structures = "vector",
    handle_inputs = "copy",
    references = FALSE,
    independent_variable = NULL,
    verbose = FALSE,
    getsource = FALSE
  )
}
\arguments{
  \item{f}{The function which should be translated from R to C++.}
  \item{output}{If set to 'R' an R function wrapping the C++ code is returned. \cr
    If output is set to  'XPtr' an external pointer object
    pointing to the C++ code is returned. \cr
    The default value is 'R'.}
  \item{types_of_args}{define the types of the arguments passed
    to the function as an character vector. \cr 
    The character vector should be either of length 1
    or has the same length as the number of arguments to the function.\cr
    In case the output is set to 'R' 'logical', 'int' or 'double' are available. \cr
    If the 'XPtr' interface is used additionally 'const logical',
    'const int' and 'const double' can be chosen. \cr
    For more information see below for details and
    check the vignette \emph{InformationForPackageAuthors}.\cr}
  \item{data_structures}{defines the data structures of the
    arguments passed to the function (as an character vector). \cr
    The character vector should be either of length 1
    or has the same length as the number of arguments to the function.\cr
    In case the output is set to 'R' one can chose between 
    'scalar' and 'vector'. \cr If the output is set to 'XPtr'
    one can set a data structure to 'scalar', 'vector' or 'borrow.\cr
  }
  \item{handle_inputs}{defines how the arguments to the function 
    should be handled as character vector. \cr
    The character vector should be either of length 1
    or has the same length as the number of arguments to the function.\cr
    In case the output is an R function
    the arguments can be either copied ('copy') or borrowed ('borrow'). \cr
   \strong{If you chose borrow R objects which are passed to the function 
   are modified. \cr This is in contrast to the usual behaviour of R.} \cr 
   If the output is an XPtr the arguments can be only borrowed ('borrow'). \cr
   In case only part of the arguments should be borrowed than an empty string
   ("") can be used to indicate this.\cr
  }
  \item{references}{defines whether the arguments are passed by reference
    or whether they are copied. This is indicated by a logical vector. \cr
    The logical vector should be either of length 1
    or has the same length as the number of arguments to the function.\cr
    If set to TRUE the arguments are passed by reference otherwise not.
    This option can be only used when the output is set to 'XPtr'
  }
  \item{independent_variable}{defines a variable for that all variables within the function \cr 
    should be differentiated. The default value is "". \cr
    In case no derivatives are required keep "".
  }
  \item{verbose}{If set to TRUE the output of the compilation process is printed.}
  \item{getsource}{If set to TRUE the function is not compiled and instead the C++ source code itself is returned.}
}
\value{
  If output is set to \emph{R} an R function is returned.
  Thus, the C++ code can directly be called within R. \cr
  In contrast a function which returns an external pointer
  is generated if the output is set to \emph{XPtr}. \cr
}
\description{
  An R function is translated to C++ source
  code and afterwards the code is compiled. \cr
  The result can be an external pointer (\emph{XPtr}) or an \emph{R} function. \cr
  The default value is an R function. \cr
  Further information can be found in the vignette: \emph{Detailed Documentation}. \cr
}
\details{
  \subsection{Type system}{
    Each variable has a fixed type in a C++ program. \cr
    In \emph{ast2ast} the default type for each variable is a data structure called 'vector'. \cr
    Each object in 'vector' is as default of type 'double'. 
    Notably, it is defined at runtime \cr
    whether a variable is a 'vector' in the sense of on R vector or it is a matrix.\cr
  }
  \subsection{Types of arguments to function}{
    The types of the arguments to the function are set together of:
    \enumerate{
      \item types_of_args; c("int", "int")
      \item data_structures; c("vector", "scalar")
      \item handle_inputs; c("borrow", "")
      \item references; c(TRUE, TRUE)
    }
    In this example this results in: 
    \verb{
      f(etr::Vec<int>& argumentNr1Input, int& argumentNr2) {
        etr::Vec<int, etr::Borrow<int>> argumentNr1(argumentNr1Input.d.p,
                                                    argumentNr1Input.size());
        ... rest of function code
      }
    }
  }
  \subsection{Types within the function}{
    As mentioned above the default type is a 'vector' containing 'doubles' \cr
    Additionally, it is possible to set specific types for a variable. \cr
    However, the type cannot be changed if once defined.
    It is possible to define the following types:
   \enumerate{
     \item logical
     \item int
     \item double
     \item logical_vector
     \item int_vector
     \item double_vector
   } 
   The first three mentioned types are scalar types. \cr
   These types cannot be resized. 
   Meaning that the behave like a vector of length 1, \cr
   which cannot be extended to have more elements.
   Notably, the scalar values cannot be subsetted. \cr
   The advantage is that scalar values need less memory.
   \subsection{declare variable with type}{
     The variables are declared with the type by using the \cr
     '::' operator. Here are some examples:
     \verb{
       f <- function() {
         d::double <- 3.14
         l::logical <- TRUE
         dv::int_vector <- vector(mode = "integer", length = 2)
       }
     }
   }
   \subsection{Borrowing}{
     As mentioned above it is possible to borrow arguments to a function. \cr
     Thus, R objects can be modified within the function. \cr
     Please be aware that it is not possible to resize the borrowed variable, \cr
     Therefore, the code below throws an error.
     Here an example:
     \verb{
       f <- function(a, b, c) { 
         a[c(1, 2, 3)] <- 1 
         b <- vector(length = 10) 
         c <- vector(length = 1)
       } 
       fcpp <- ast2ast::translate(f, handle_inputs = "borrow") 
       a <- b <- c <- c(1, 2, 3) 
       fcpp(a, b,c) 
     }
  }
  \subsection{Derivatives}{
    In case the 'independent_variable' is set. The derivatives are calculated for all variables \cr
    with respect to the defined variable. The derivatives can be extracted by using the function 'get_deriv'. \cr
    \verb{
      y = x*x
      dydx = get_deriv(y)
    }
  }
  \subsection{The following functions are supported:}{
  \enumerate{
    \item assignment: = and <-
      \item allocation: vector, matrix and rep
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
  }}
  \subsection{Some details about the implemented functions}{
  \itemize{
    \item For indices squared brackets \emph{'[]'} can be used as common in R. Beyond that the function 'at' exists
    which accepts as first argument a variable and as the second argument you pass the desired index.
    The caveat of using 'at' is that only \strong{one} entry can be accessed. The function \emph{'[]'} can return more then one element. \cr
    \strong{The \emph{at}-function returns a reference to the vector entry.
      Therefore variable[index] can behave differently then \emph{at(variable, index)}.
      If only integers are found within \emph{'[]'} the function \emph{at} is used at the right side of an assignment operator (\emph{=}).
      The \emph{at}-function can also be used on the left side of an assignment operator. 
      However, in this case only \emph{at} should be used at the right side. Otherwise the results are wrong.
      }  \cr
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
    \item For- and while-loops can be written as common in R
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
    \item The print function accepts either a scalar, vector, matrix, string, bool or nothing (empty line).
    \item In order to return an object use the \emph{return} function (\strong{The last object is not returned automatically as in R}).
    \item In order to interpolate values the \emph{cmr} function can be used. The function needs three arguments.
    \enumerate{
      \item the first argument is the point of the independent variable (\strong{x}) for which the dependent variable should be calculated (\strong{y}). This has to be a vector of length one.
      \item the second argument is a vector defining the points of the independent variable (\strong{x}). This has to be a vector of at least length four.
      \item the third argument is a vector defining the points of the dependent variable (\strong{y}). This has to be a vector of at least length four.
    }
    \strong{Be aware that the R code is translated to ETR an expression template library which tries to mimic R. \cr
      However, it does not behave exactly like R! Please check your compiled function before using it in a serious project. \cr
      If you want to see how \emph{ast2ast} differs from R in detail check the vignette: \emph{Detailed Documentation}. \cr
      In case you want to know how \emph{ast2ast} works in detail check the vignette:   \emph{InformationForPackageAuthors}. \cr
    }
  }
  }
}
}
\examples{ # Further examples can be found in the vignettes. 
    \dontrun{
      f <- function() {
        print("Hello World!")
      }
      fcpp <- ast2ast::translate(f)
      fcpp()

      # Translating to external pointer
      # --------------------------------------------------------------------------
      f <- function() {
        print("Hello World!")
      }
      pointer_to_f_cpp <- ast2ast::translate(f,
                                             output = "XPtr", verbose = TRUE
      )

      Rcpp::sourceCpp(code = "
                      #include <Rcpp.h>
                      typedef void (*fp)();

                      // [[Rcpp::export]]
                      void call_fct(Rcpp::XPtr<fp> inp) {
                        fp f = *inp;
                        f(); } ")

      call_fct(pointer_to_f_cpp)

      # Run sum example:
      # ==========================================================================

      # R version of run sum
      # --------------------------------------------------------------------------
      run_sum <- function(x, n) {
        sz <- length(x)

        ov <- vector(mode = "numeric", length = sz)

        ov[n] <- sum(x[1:n])
        for (i in (n + 1):sz) {
          ov[i] <- ov[i - 1] + x[i] - x[i - n]
        }

        ov[1:(n - 1)] <- NA

        return(ov)
      }

      # translated Version of R function
      # --------------------------------------------------------------------------
      run_sum_fast <- function(x, n) {
        sz <- length(x)
        ov <- vector(mode = "numeric", length = sz)

        sum_db <- 0
        for (i in 1:n) {
          sum_db <- sum_db + at(x, i)
        }
        ov[n] <- sum_db

        for (i in (n + 1):sz) {
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
