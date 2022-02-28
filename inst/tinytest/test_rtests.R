
library(ast2ast)

# subsetting
# ==============================================================================
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  a[4, c(1, 4)] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1:3, 20, 5:15, 20) ) #70.9



#f <- function(a) {
#  a <- matrix(1:16, 4, 4)
#  a[4, ] <- 20
#  return(a)
#}
#fetr <- translate(f)
#ret <- test(fetr)
#expect_equal(ret, c(1:3, 20, 5:7, 20, 9:11, 20, 13:15, 20) ) #70.8




f <- function() {
  a_db <- 1
  a_db[1] = 2
}
ret <- translate(f)
expect_equal(ret, NULL) #0

f <- 1
expect_error(translate(f)) #1

Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')

# access outside boundaries
f <- function(a) {
  a[11] <- 20
}
#fetr <- translate(f)
#expect_error(test(fetr) ) #2


f <- function(a) {
  a[0] <- 20
}
#fetr <- translate(f)
#expect_error(test(fetr)) #3

# plus
# ==============================================================================
# vectors
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector plus(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')

f <- function(a) {
  a <- a + 3
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, c(4, 5, 6, 7)) #4


f <- function(a) {
  a <- a + vector(3.14, 4)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, c(1,2,3,4) + 3.14) #5


f <- function(a) {
  a <- a + matrix(3, 2, 2)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, c(1,2,3,4) + 3)#6


f <- function(a) {
  a <- 1
  a <- a + 3.14
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, 4.14)#7


f <- function(a) {
  a <- 1
  a <- a + vector(3, 4)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, rep(4, 4))#8


f <- function(a) {
  b <- vector(2)
  b[1] <- 100
  b[2] <- 200
  a <- a + b
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, c(101, 202, 103, 204))#8.1



f <- function(a) {
  b <- vector(9)
  b[1] <- 100
  b[2] <- 200
  b[3] <- 300
  b[4] <- 400
  b[5] <- 500
  b[6] <- 600
  b[7] <- 700
  b[8] <- 800
  b[9] <- 900
  a <- a + b
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, c(101, 202, 303, 404, 501, 602, 703, 804, 901))#8.2


f <- function(a) {
  a <- 1
  a <- vector(3, 4) + a
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, rep(4, 4))#9



f <- function(a) {
  a <- 1
  a <- 1+ 4
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, 5)#10



# matrices
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix plus(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = matrix(3, 2, 2);
  sexp b = Fct(a);

  NumericMatrix ret = b;
  return ret;
}

')

f <- function(a) {
  a <- a + a
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(6, 2, 2))#11


f <- function(a) {
  a <- a + 1
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(4, 2, 2))#12


f <- function(a) {
  a <- 4 + a
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(7, 2, 2))#13


f <- function(a) {
  b <- vector(3, 2)
  a <- a + b
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(6, 2, 2))#14
# ==============================================================================



















# subtract
# ==============================================================================
# vectors
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector plus(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')

f <- function(a) {
  a <- a - 3
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, c(-2, -1, 0, 1))#15


f <- function(a) {
  a <- a - vector(3.14, 4)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, c(1,2,3,4) - 3.14)#16


f <- function(a) {
  a <- a - matrix(3, 2, 2)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, c(1,2,3,4) - 3)#17


f <- function(a) {
  a <- 1
  a <- a - 3.14
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, 1 - 3.14)#18


f <- function(a) {
  a <- 1
  a <- a - vector(3, 4)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, rep(-2, 4) )#19


f <- function(a) {
  a <- 1
  a <- vector(3, 4) - a
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, rep(2, 4))#20



f <- function(a) {
  a <- 1
  a <- 1 - 4
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, -3)#21



# matrices
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix plus(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = matrix(3, 2, 2);
  sexp b = Fct(a);

  NumericMatrix ret = b;
  return ret;
}

')

f <- function(a) {
  a <- a - a*2
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(-3, 2, 2))#22


f <- function(a) {
  a <- a - 1
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(2, 2, 2))#23


f <- function(a) {
  a <- 4 - a
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(1, 2, 2))#24


f <- function(a) {
  b <- vector(3, 2)
  a <- a - b
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(0, 2, 2))#25
# ==============================================================================













# divide
# ==============================================================================
# vectors
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector plus(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')

f <- function(a) {
  a <- a / 2.
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, c(0.5, 1, 1.5, 2) )#26


f <- function(a) {
  a <- a / c(1, 2, 3, 4)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, rep(1, 4))#27


f <- function(a) {
  a <- a / matrix(3, 2, 2)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, c(1,2,3,4) / 3)#28


f <- function(a) {
  a <- 1
  a <- a / 3.14
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, 1 / 3.14)#29


f <- function(a) {
  a <- 1
  a <- a / vector(3, 4)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, 1 / rep(3, 4) )#30


f <- function(a) {
  a <- 2
  a <- vector(3, 4) / a
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, rep(3, 4) / 2)#31



f <- function(a) {
  a <- 4
  a <- 1 / 2
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)

expect_equal(ret, 1/2)#32



# matrices
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix plus(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = matrix(3, 2, 2);
  sexp b = Fct(a);

  NumericMatrix ret = b;
  return ret;
}

')

f <- function(a) {
  a <- a / (a*2)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(1/2, 2, 2))#33


f <- function(a) {
  a <- a / 2
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(1.5, 2, 2))#34


f <- function(a) {
  a <- 4 / a
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(4/3, 2, 2))#35


f <- function(a) {
  b <- vector(3, 2)
  a <- a / b
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(1, 2, 2))#36
# ==============================================================================











# times
# ==============================================================================
# vectors
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector plus(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')

f <- function(a) {
  a <- a * 2
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, c(1,2,3,4)*2 )#37


f <- function(a) {
  a <- a * c(1, 2, 3, 4)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, c(1,2,3,4)*c(1,2,3,4) ) # 38


f <- function(a) {
  a <- a * matrix(3, 2, 2)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, c(1,2,3,4)*3 ) #39


f <- function(a) {
  a <- 1
  a <- a * 3.14
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, 3.14)#40


f <- function(a) {
  a <- 2
  a <- a * vector(3, 4)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, 2*rep(3, 4) )#41


f <- function(a) {
  a <- 2
  a <- vector(3, 4) * a
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, rep(3, 4) * 2)#42



f <- function(a) {
  a <- 4
  a <- 2 * 3
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, 6)#43



# matrices
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix plus(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = matrix(3, 2, 2);
  sexp b = Fct(a);

  NumericMatrix ret = b;
  return ret;
}

')

f <- function(a) {
  a <- a * (a*2)
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, (matrix(9, 2, 2)*2) )#44


f <- function(a) {
  a <- a * 2
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(6, 2, 2))#45


f <- function(a) {
  a <- 4 * a
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(4*3, 2, 2))#46


f <- function(a) {
  b <- vector(3, 2)
  a <- a * b
  return(a)
}
fetr <- translate(f)
ret <- plus(fetr)
expect_equal(ret, matrix(9, 2, 2))#47
# ==============================================================================











# colon, dim and length
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')

f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)
  c <- c(-100, a, b, 100)
  d <- vector(1, 2)
  e <- matrix(-1, 2, 2)
  f <- c(c, d, e, 2)
  return(f)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(-100, 1, 2, 3, 5, 6, 7, 100, 1, 1, rep(-1, 4), 2) )#48


f <- function(a) {
  v <- c(1, 2, 3, 4)
  m <- matrix(2, 2, 3)
  b <- c(length(a), dim(m) )

  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4, 2, 3) )#49












# ==
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
bool test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  bool ret = b;
  return ret;
}

')

f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)

  return(a == b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, FALSE) #50


f <- function(a) {
  a <- c(1, 2, 3)
  return(a == a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #51



f <- function(a) {
  a <- c(2, 2, 2)
  return(a == 2)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #52








# >=
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
bool test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  bool ret = b;
  return ret;
}

')

f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)

  return(a >= b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, FALSE) #53


f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(0, 1, 2)
  c <- a - b
  return(a >= c)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #54



f <- function(a) {
  a <- c(2, 2, 2)
  return(a >= 1)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #55















# <=
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
bool test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  bool ret = b;
  return ret;
}

')

f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)

  return(b <= a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, FALSE) #56


f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(0, 1, 2)
  c <- a - b
  return(c <= a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #57



f <- function(a) {
  a <- c(2, 2, 2)
  return(a <= 2)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #58





# >
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
bool test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  bool ret = b;
  return ret;
}

')

f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)

  return(a > b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, FALSE) #59


f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(1, 1, 2)
  c <- a - b
  return(a > c)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #60



f <- function(a) {
  a <- c(2, 2, 2)
  return(a > 1)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #61


# <
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
bool test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  bool ret = b;
  return ret;
}

')

f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)

  return(b < a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, FALSE) #62


f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(1, 1, 2)
  c <- a + b
  return(a < c)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #63



f <- function(a) {
  a <- c(2, 2, 2)
  return(a < 5)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #64





# !=
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
bool test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  bool ret = b;
  return ret;
}

')

f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3)

  return(a != b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, FALSE) #65


f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(1, 1, 2)
  c <- a - b
  return(a != c)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #66



f <- function(a) {
  a <- c(2, 2, 2)
  return(a != 1)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #67






# additional comparison tests for rvalues and results of calculations
# ==
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
bool test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  bool ret = b;
  return ret;
}

')

f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)

  return(a + b == b - a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, FALSE) #50.1

f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)

  return(a + b == c(100, 100, 200))
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, FALSE) #50.2


f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)

  return(a + b != b - a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #50.3

f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)

  return(a + b != c(100, 100, 200))
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #50.4



f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)

  return(a + b > b - a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #50.5

f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)

  return(a + b < c(100, 100, 200))
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #50.6


f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)

  return(a + b >= b - a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #50.7

f <- function(a) {
  a <- c(1, 2, 3)
  b <- c(5, 6, 7)

  return(a + b <= c(100, 100, 200))
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, TRUE) #50.8







# exponent & log
# ==============================================================================
# vectors
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')

f <- function(a) {
  a <- a^2 + log(1)
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 4, 9, 16) ) #68


# matrices
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = matrix(3, 2, 2);
  sexp b = Fct(a);

  NumericMatrix ret = b;
  return ret;
}

')

f <- function(a) {
  a <- a^3
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, matrix(27, 2, 2))#69



f <- function(a) {
  a <- exp(a, 3) + 1
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, matrix(28, 2, 2))#69.1


# subsetting
# ==============================================================================
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')

f <- function(a) {
  for(i in 1:length(a)) {
    a[i] = 100
  }
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, rep(100, 4) ) #70

f <- function(a) {
  a[] <- 0
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, rep(0, 4) ) #70.0

f <- function(a) {
  a[1] <- 0
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(0, 2, 3, 4) ) #70.1

f <- function(a) {
  a[2.3] <- 0
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 0, 3, 4) ) #70.2

f <- function(a) {
  a[TRUE] <- 0
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, rep(0, 4) ) #70.3

f <- function(a) {
  b <- c(1, 2, 3, 4, 1)
  a[b] <- 1:5
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(5, 2, 3, 4) ) #70.4

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  a[4, 4] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1:15, 20) ) #70.5

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  a[4, 4.5] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1:15, 20) ) #70.6

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  a[4, TRUE] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1:3, 20, 5:7, 20, 9:11, 20, 13:15, 20) ) #70.7

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  a[4, ] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1:3, 20, 5:7, 20, 9:11, 20, 13:15, 20) ) #70.8

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  a[4, c(1, 4)] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1:3, 20, 5:15, 20) ) #70.9

f <- function(a) {
  a <- matrix(1:4, 2, 2)
  a[1.3, 1] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 2, 3, 4) ) #70.10


f <- function(a) {
  a <- matrix(1:4, 2, 2)
  a[T, 1] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 20, 3, 4) ) #70.11

f <- function(a) {
  a <- matrix(1:4, 2, 2)
  a[, 1] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 20, 3, 4) ) #70.12

f <- function(a) {
  a <- matrix(1:6, 3, 2)
  b <- c(1, 3)
  a[b, 1] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 2, 20, 4, 5, 6) ) #70.13

f <- function(a) {
  a <- matrix(1:6, 3, 2)
  a[1.5, 1.1] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 2, 3, 4, 5, 6) ) #70.14

f <- function(a) {
  a <- matrix(1:6, 3, 2)
  a[1.5, T] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 2, 3, 20, 5, 6) ) #70.14

f <- function(a) {
  a <- matrix(1:6, 3, 2)
  a[1.5, ] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 2, 3, 20, 5, 6) ) #70.15

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 3)
  a[1.5, b] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 2, 3, 4, 5, 6, 20, 8, 9) ) #70.16

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  a[T, 1.5] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 20, 20, 4:9) ) #70.17

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  a[, 1.5] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 20, 20, 4:9) ) #70.18

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 2)
  a[b , 1.5] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 20, 3:9) ) #70.19


f <- function(a) {
  a <- matrix(1:9, 3, 3)
  a[TRUE , T] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, rep(20, 9) ) #70.20

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  a[TRUE , ] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, rep(20, 9) ) #70.21

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  b <- c(2, 3)
  a[TRUE , b] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2, 3,20, 20, 20, 20, 20, 20) ) #70.22

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  a[ , TRUE] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, rep(20, 9) ) #70.23

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 2)
  a[b , TRUE] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 20, 3, 20, 20, 6, 20, 20, 9) ) #70.24

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  a[ , ] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, rep(20, 9) ) #70.25

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 2)
  a[ , b] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 20, 20, 20, 20, 20, 7, 8, 9) ) #70.26

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 3)
  a[b , ] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 2, 20, 20, 5, 20, 20, 8, 20) ) #70.27

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 3)
  c <- c(1, 2)
  a[b , c] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 2, 20, 20, 5, 20, 7, 8, 9) ) #70.28

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  c <- c(1, 2)
  a[c + c] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 20, 3, 20, 5:16) ) #70.29

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  a[a < 5] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 20, 20, 20, 5:16) ) #70.30

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  c <- c(1, 2)
  a[1, c+ c] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2, 3, 4,  20, 6, 7, 8, 9, 10, 11, 12, 20, 14, 15, 16) ) #70.31

f <- function(a) { ########################
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2, 3, 4)
  a[1, b >= 3] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1:8, 20, 10:12, 20, 14:16) ) #70.32

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2)
  a[b+ b, 1] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 20, 3, 20, 5:16)  ) #70.33

f <- function(a) { ########################
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2, 3, 4)
  a[b > 3, 4] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:15, 20) ) #70.34


f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2)
  a[3.5, b+ b] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1, 2, 3, 4, 5, 6, 20, 8, 9, 10, 11, 12, 13, 14, 20, 16 ) ) #70.35

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2, 3, 4)
  a[3.5, b <= 1] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1, 2, 20,4:16) ) #70.36

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2)
  a[b+ b, 1.5] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1, 20, 3, 20, 5:16 ) )#70.37

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2, 3, 4)
  a[b >= 1, 1.5] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(20, 20, 20, 20, 5:16) )#70.38

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2)
  a[TRUE, b + b] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:4, rep(20, 4), 9:12, rep(20, 4)) )#70.39

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2, 3, 4)
  a[TRUE, b >= 3] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:8, rep(20, 8)) )#70.40

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2)
  a[b + b, TRUE] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1, 20, 3, 20, 5, 20, 7, 20, 9, 20, 11, 20, 13, 20, 15, 20) )#70.41

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2, 3, 4)
  a[b == 1, TRUE] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(20, 2:4, 20, 6:8, 20, 10:12, 20, 14:16) )#70.42

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2)
  a[, b + b] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:4, rep(20, 4), 9:12, rep(20, 4)) )#70.43

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2, 3, 4)
  a[, b >= 3] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:8, rep(20, 8)) )#70.44

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2)
  a[b + b, ] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1, 20, 3, 20, 5, 20, 7, 20, 9, 20, 11, 20, 13, 20, 15, 20) )#70.45

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2, 3, 4)
  a[b == 1, ] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(20, 2:4, 20, 6:8, 20, 10:12, 20, 14:16) )#70.46

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2)
  a[b + b, b + b] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:5, 20, 7, 20, 9:13, 20, 15, 20) )#70.47

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2)
  c <- c(2, 4)
  a[b + b, c] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:5, 20, 7, 20, 9:13, 20, 15, 20) )#70.48

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2)
  c <- c(2, 4)
  a[c, b + b] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:5, 20, 7, 20, 9:13, 20, 15, 20) )#70.49

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1, 2)
  c <- c(1, 2, 3, 2)
  a[b + b, c == 2] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:5, 20, 7, 20, 9:13, 20, 15, 20) )#70.50

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(2,4)
  c <- c(1, 2, 3, 2)
  a[b, c == 2] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:5, 20, 7, 20, 9:13, 20, 15, 20) )#70.51

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(1,2)
  c <- c(1, 2, 3, 2)
  a[c == 2, b+b] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:5, 20, 7, 20, 9:13, 20, 15, 20) )#70.52

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(2,4)
  c <- c(1, 2, 3, 2)
  a[c == 2, b] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:5, 20, 7, 20, 9:13, 20, 15, 20) )#70.53

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  c <- c(1, 2, 3, 2)
  a[c == 2, c == 2] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:5, 20, 7, 20, 9:13, 20, 15, 20) )#70.54

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(2, 4)
  c <- c(1, 2, 3, 2)
  a[b, c == 2] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:5, 20, 7, 20, 9:13, 20, 15, 20) )#70.55

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  b <- c(2, 4)
  c <- c(1, 2, 3, 2)
  a[c==2, b] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret,  c(1:5, 20, 7, 20, 9:13, 20, 15, 20) )#70.56







Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')

f <- function(a) {
  a <- 1:6
  b <- a[2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 2 ) #70.57

f <- function(a) {
  a <- 1:6
  b <- a[2.5]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 2 ) #70.58

f <- function(a) {
  a <- 1:6
  b <- a[TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6 ) #70.59

f <- function(a) {
  a <- 1:6
  b <- a[]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6 ) #70.60

f <- function(a) {
  a <- 1:6
  b <- c(1, 5)
  b <- a[b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 5) ) #70.61


Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) ();

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp b = Fct();

  NumericVector ret = b;
  return ret;
}

')


f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[1, 2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4) #70.62

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[1, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 4, 7)) #70.63

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[1, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 4, 7)) #70.64

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 3)
  b <- a[1, b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 7)) #70.65

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[1.1, 2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4) #70.66

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[TRUE, 2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,5,6)) #70.67

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[, 2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,5,6)) #70.68

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 3)
  b <- a[b, 2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,6)) #70.69

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[1.1, 2.2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4) #70.70

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[1.1, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 4, 7)) #70.71

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[1.1, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,4,7)) #70.72

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 3)
  b <- a[1.1, b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 7)) #70.73

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[TRUE, 2.1]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,5,6)) #70.74

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[, 2.1]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,5,6)) #70.75

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 3)
  b <- a[b, 2.5]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,6)) #70.76

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[TRUE, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:9) #70.77

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[TRUE, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:9) #70.78

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 2)
  b <- a[TRUE, b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6) #70.79

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:9) #70.80

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 2)
  b <- a[b, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2, 4, 5, 7, 8)) #70.81

f <- function() {
  a <- matrix(1:9, 3, 3)

  b <- a[, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:9) #70.82

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 2)
  b <- a[, b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6) #70.83

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 2)
  b <- a[b, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2, 4, 5, 7, 8)) #70.84

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 2)
  b <- a[b, b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2, 4, 5)) #70.85


Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')


f <- function(a) {
  a <- 1:6
  b <- c(1, 2)
  b <- a[b + b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(2, 4) ) #70.86

f <- function(a) {
  a <- 1:6
  b <- c(1, 2, 3, 4)
  c <- c(1, 5, 3, 6.7)
  b <- a[b == c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 3) ) #70.87



Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) ();

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp b = Fct();

  NumericVector ret = b;
  return ret;
}

')

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[1, b + c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 4)) #70.88

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[1, b != c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1) #70.89


f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b + c, 1]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2)) #70.90

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b != c, 2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4) #70.91

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[1.5, b + c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 4)) #70.92

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[1.4, b != c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1) #70.93

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b + c, 1.5]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2)) #70.94

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b != c, 2.5]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4) #70.95

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[TRUE, b + c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6) #70.96

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1, 2, 3)
  c <- c(1, 1, 2, 5)
  b <- a[TRUE, b == c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4:9) #70.97

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b + c, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5, 7, 8)) #70.98



f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b == c, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(2, 5, 8)) #70.99

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[, b + c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6) #70.100

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1, 2, 3)
  c <- c(1, 1, 2, 5)
  b <- a[, b == c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4:9) #70.101

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b + c, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5, 7, 8)) #70.102

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b == c, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(2, 5, 8)) #70.103

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b + c, b+c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5)) #70.104

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  rs <- c(1, 2)
  b <- a[rs, b+c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5)) #70.105

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  cs <- c(1, 2)
  b <- a[b+c, cs]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5)) #70.106

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b+c, b == c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4, 5)) #70.107

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  rs <- c(1, 2)
  b <- a[rs, b == c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4, 5)) #70.108

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b == c, b+c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(2, 5)) #70.109

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  cs <- c(1, 2)
  b <- a[b == c, cs]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(2, 5)) #70.110

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b == c, b == c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 5) #70.111



# test subset with r value
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')


f <- function(a) {
  a <- 1:6
  b <- a[c(1, 5)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 5) ) #70.112


Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) ();

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp b = Fct();

  NumericVector ret = b;
  return ret;
}

')


f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 3)
  b <- a[1, c(1, 3)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 7)) #70.113


f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(1, 3)
  b <- a[c(1, 3), 2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,6)) #70.114


f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[1.1, c(1, 3)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 7)) #70.115

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[c(1, 3), 2.5]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,6)) #70.116

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[TRUE, c(1,2)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6) #70.117

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[c(1,2), TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2, 4, 5, 7, 8)) #70.118

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[, c(1,2)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6) #70.119

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[c(1,2), ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2, 4, 5, 7, 8)) #70.120

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[c(1,2), c(1,2)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2, 4, 5)) #70.121


Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')


f <- function(a) {
  a <- 1:6
  b <- a[c(2, 4)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(2, 4) ) #70.122


Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) ();

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp b = Fct();

  NumericVector ret = b;
  return ret;
}

')

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[1, c(1, 2)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 4)) #70.123


f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[c(1, 2), 1]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2)) #70.124

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[1.5, c(1, 2)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 4)) #70.125

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[c(1, 2), 1.5]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2)) #70.126

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[TRUE, c(1, 2)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6) #70.127

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[c(1, 2), TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5, 7, 8)) #70.128

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[, c(1, 2)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6) #70.129

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[c(1, 2), ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5, 7, 8)) #70.130

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- a[c(1, 2), c(1, 2)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5)) #70.131

f <- function() {
  a <- matrix(1:9, 3, 3)
  rs <- c(1, 2)
  b <- a[rs, c(1, 2)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5)) #70.132

f <- function() {
  a <- matrix(1:9, 3, 3)
  cs <- c(1, 2)
  b <- a[c(1, 2), cs]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5)) #70.133

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[c(1, 2), b == c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4, 5)) #70.134

f <- function() {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b == c, c(1, 2)]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(2, 5)) #70.135









































# test subassign with rvalues
# ==============================================================================
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')


f <- function(a) {
  a[c(1,2,3,4,1)] <- 1:5
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(5, 2, 3, 4) ) #70.136


f <- function(a) {
  a <- matrix(1:16, 4, 4)
  a[4, c(1, 4)] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1:3, 20, 5:15, 20) ) #70.137

f <- function(a) {
  a <- matrix(1:6, 3, 2)
  a[c(1,3), 1] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 2, 20, 4, 5, 6) ) #70.138

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  a[1.5, c(1,3)] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 2, 3, 4, 5, 6, 20, 8, 9) ) #70.139


f <- function(a) {
  a <- matrix(1:9, 3, 3)
  a[c(1,2), 1.5] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 20, 3:9) ) #70.140

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  a[TRUE , c(2,3)] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2, 3,20, 20, 20, 20, 20, 20) ) #70.141

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  a[c(1,2), TRUE] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 20, 3, 20, 20, 6, 20, 20, 9) ) #70.142

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  a[ , c(1,2)] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 20, 20, 20, 20, 20, 7, 8, 9) ) #70.143

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  a[c(1, 3), ] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 2, 20, 20, 5, 20, 20, 8, 20) ) #70.144

f <- function(a) {
  a <- matrix(1:9, 3, 3)
  a[c(1, 3), c(1, 2)] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(20, 2, 20, 20, 5, 20, 7, 8, 9) ) #70.145

f <- function(a) {
  a <- matrix(1:16, 4, 4)
  a[c(2, 4)] <- 20
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 20, 3, 20, 5:16) ) #70.146


# test subset result of calculation e.g. subset(a+a, 1)

Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')

f <- function(a) {
  b <- (1:6 + 0)[2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 2 ) #70.147

f <- function(a) {
  b <- (1:6 + 0)[2.5]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 2 ) #70.148

f <- function(a) {
  b <- (1:6 + 0)[TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6 ) #70.149

f <- function(a) {
  b <- (1:6 + 0)[]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6 ) #70.150

f <- function(a) {
  b <- c(1, 5)
  b <- (1:6 + 0)[b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 5) ) #70.151


Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) ();

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp b = Fct();

  NumericVector ret = b;
  return ret;
}

')


f <- function() {
  b <- (matrix(1:9, 3, 3) + 0)[1, 2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4) #70.152

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[1, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 4, 7)) #70.153

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[1, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 4, 7)) #70.154

f <- function() {
  b <- c(1, 3)
  b <- (matrix(0, 3, 3) + 1:9)[1, b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 7)) #70.155

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[1.1, 2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4) #70.156

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[TRUE, 2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,5,6)) #70.157

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[, 2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,5,6)) #70.158

f <- function() {
  b <- c(1, 3)
  b <- (matrix(0, 3, 3) + 1:9)[b, 2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,6)) #70.159

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[1.1, 2.2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4) #70.160

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[1.1, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 4, 7)) #70.161

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[1.1, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,4,7)) #70.162

f <- function() {
  b <- c(1, 3)
  b <- (matrix(0, 3, 3) + 1:9)[1.1, b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 7)) #70.163

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[TRUE, 2.1]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,5,6)) #70.164

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[, 2.1]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,5,6)) #70.165

f <- function() {
  b <- c(1, 3)
  b <- (matrix(0, 3, 3) + 1:9)[b, 2.5]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4,6)) #70.166

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[TRUE, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:9) #70.167

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[TRUE, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:9) #70.168

f <- function() {
  b <- c(1, 2)
  b <- (matrix(0, 3, 3) + 1:9)[TRUE, b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6) #70.169

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:9) #70.170

f <- function() {
  b <- c(1, 2)
  b <- (matrix(0, 3, 3) + 1:9)[b, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2, 4, 5, 7, 8)) #70.171

f <- function() {
  b <- (matrix(0, 3, 3) + 1:9)[, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:9) #70.172

f <- function() {
  b <- c(1, 2)
  b <- (matrix(0, 3, 3) + 1:9)[, b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6) #70.173

f <- function() {
  b <- c(1, 2)
  b <- (matrix(0, 3, 3) + 1:9)[b, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2, 4, 5, 7, 8)) #70.174

f <- function() {
  b <- c(1, 2)
  b <- (matrix(0, 3, 3) + 1:9)[b, b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2, 4, 5)) #70.175


Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')


f <- function(a) {
  b <- c(1, 2)
  b <- (1:6 + 0)[b + b]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(2, 4) ) #70.176

f <- function(a) {
  b <- c(1, 2, 3, 4)
  c <- c(1, 5, 3, 6.7)
  b <- (1:6 + 0)[b == c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 3) ) #70.177



Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) ();

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp b = Fct();

  NumericVector ret = b;
  return ret;
}

')

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[1, b + c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 4)) #70.178

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[1, b != c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1) #70.179


f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[b + c, 1]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2)) #70.180

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[b != c, 2]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4) #70.181

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[1.5, b + c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 4)) #70.182

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[1.4, b != c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1) #70.183

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[b + c, 1.5]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2)) #70.184

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[b != c, 2.5]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4) #70.185

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[TRUE, b + c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6) #70.186

f <- function() {
  b <- c(0, 1, 2, 3)
  c <- c(1, 1, 2, 5)
  b <- (matrix(0, 3, 3) + 1:9)[TRUE, b == c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4:9) #70.187

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[b + c, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5, 7, 8)) #70.188



f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[b == c, TRUE]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(2, 5, 8)) #70.189

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[, b + c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 1:6) #70.190

f <- function() {
  b <- c(0, 1, 2, 3)
  c <- c(1, 1, 2, 5)
  b <- (matrix(0, 3, 3) + 1:9)[, b == c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 4:9) #70.191

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[b + c, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5, 7, 8)) #70.192

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[b == c, ]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(2, 5, 8)) #70.193

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[b + c, b+c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5)) #70.194

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  rs <- c(1, 2)
  b <- (matrix(0, 3, 3) + 1:9)[rs, b+c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5)) #70.195

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  cs <- c(1, 2)
  b <- (matrix(0, 3, 3) + 1:9)[b+c, cs]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1,2, 4, 5)) #70.196

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[b+c, b == c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4, 5)) #70.197

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  rs <- c(1, 2)
  b <- (matrix(0, 3, 3) + 1:9)[rs, b == c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(4, 5)) #70.198

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[b == c, b+c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(2, 5)) #70.199

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  cs <- c(1, 2)
  b <- (matrix(0, 3, 3) + 1:9)[b == c, cs]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(2, 5)) #70.200

f <- function() {
  b <- c(0, 1)
  c <- c(1, 1)
  b <- (matrix(0, 3, 3) + 1:9)[b == c, b == c]
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, 5) #70.201



# matrices
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = matrix(3, 2, 2);
  sexp b = Fct(a);

  NumericMatrix ret = b;
  return ret;
}

')

f <- function(a) {
  for(i in 1:dim(a)[1]){
    for(j in 1:dim(a)[2]) {
      a[i, j] = i + j
    }
  }
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, matrix(c(2, 3, 3, 4) ,2, 2) )#76











# vectors
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = coca(1, 2, 3, 4);
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')

f <- function(a) {
  a <- c(0, 0.2, 0.4, 0.99)
  b <- sin(a)
  return(b)
}

fetr <- translate(f, verbose = FALSE)
ret <- test(fetr)
expect_equal(ret, sin(c(0, 0.2, 0.4, 0.99) ) ) #81


f <- function(a) {
  a <- c(0, 0.2, 0.4, 0.99)
  b <- asin(a)
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, asin(c(0, 0.2, 0.4, 0.99) ) ) #82


f <- function(a) {
  a <- c(0, 0.2, 0.4, 0.99)
  b <- sinh(a)
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, sinh(c(0, 0.2, 0.4, 0.99) ) ) #83


f <- function(a) {
  a <- c(0, 0.2, 0.4, 0.99)
  b <- cos(a)
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, cos(c(0, 0.2, 0.4, 0.99) ) ) #84


f <- function(a) {
  a <- c(0, 0.2, 0.4, 0.99)
  b <- acos(a)
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, acos(c(0, 0.2, 0.4, 0.99) ) ) #85



f <- function(a) {
  a <- c(0, 0.2, 0.4, 0.99)
  b <- cosh(a)
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, cosh(c(0, 0.2, 0.4, 0.99) ) ) #86




f <- function(a) {
  a <- c(0, 0.2, 0.4, 0.99)
  b <- tan(a)
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, tan(c(0, 0.2, 0.4, 0.99) ) ) #87


f <- function(a) {
  a <- c(0, 0.2, 0.4, 0.99)
  b <- atan(a)
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, atan(c(0, 0.2, 0.4, 0.99) ) ) #88



f <- function(a) {
  a <- c(0, 0.2, 0.4, 0.99)
  b <- tanh(a)
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, tanh(c(0, 0.2, 0.4, 0.99) ) ) #89










# test sexp to arma and Rcpp and vice versa
# ==============================================================================
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"

// [[Rcpp::plugins(cpp17)]]

// [[Rcpp::export]]
arma::vec test() {
  sexp a = coca(1, 2, 3, 4);
  arma::vec v = a;
  return v;
}

')

ret <- test()
expect_equal(ret, matrix(c(1, 2, 3, 4), 4, 1) ) #90




Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"

// [[Rcpp::plugins(cpp17)]]

// [[Rcpp::export]]
arma::mat test() {
  sexp a = matrix(colon(1, 16), 4, 4);
  arma::mat v = a;
  return v;
}

')

ret <- test()
expect_equal(ret, matrix(1:16, 4, 4) ) #91



Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::mat test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  arma::mat M(4, 4);

  sexp a(4, 4, M.memptr(), 2);
  sexp b = Fct(a);

  arma::mat ret = b;
  return ret;
}

')

f <- function(a) {
  for(i in 1:4) {
    for(j in 1:4)
    a[i, j] <- i + j
  }
  a[1, ] <- 100
  a[, 2] <- 200
  return(a)
}

fetr <- translate(f)
ret <- test(fetr)
m <- matrix(1, 4, 4)
m <- f(m)
expect_equal(ret, m) #91.1


Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"

// [[Rcpp::plugins(cpp17)]]

// [[Rcpp::export]]
Rcpp::NumericMatrix test() {
  sexp a = matrix(colon(1, 16), 4, 4);
  Rcpp::NumericMatrix v = a;
  return v;
}

')

ret <- test()
expect_equal(ret, matrix(1:16, 4, 4) ) #92


















# ==============================================================================
