
library(ast2ast)

f <- 1
expect_error(translate(f)) #1

# access outside boundaries
f <- function(a) {
  a[11] <- 20
}
fetr <- translate(f)
a <- 1:10
expect_error(fetr(a)) #2


f <- function(a) {
  a[0] <- 20
}
fetr <- translate(f)
a <- 1:10
expect_error(fetr(a)) #3



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
  a <- a / 2
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






















# subsetting
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
  for(i in 1:length(a)) {
    a[i] = 100
  }
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, rep(100, 4) ) #70



f <- function(a) {
  sub(a, 1:2) = 100
  sub(a, 3:4) = 200
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(100, 100, 200, 200) ) #71


f <- function(a) {
  sub(a, a >= 2) = -1
  sub(a, a > -2) = -2

  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(-2, -2, -2, -2) ) #72


f <- function(a) {
  b <- c(100, 200)
  sub(a, 3:4) <- b
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(1, 2, 100, 200) ) #73


f <- function(a) {
  b <- c(100, 200, 300, 400)
  sub(a, 1:2) <- sub(b, c(1, 4))
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, c(100, 400, 3, 4) ) #74


f <- function(a) {
  b <- c(100, 200, 300, 400)
  sub(a, NULL) <- 1
  return(a)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, rep(1, 4) ) #75

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

  sexp a = matrix(3, 4, 4);
  sexp b = Fct(a);

  NumericMatrix ret = b;
  return ret;
}

')

f <- function(a) {
  b <- c(6, 8)
  sub(a, c(1, 2), c(3, 4)) = b + b
  c <- sub(a, c(1, 2), c(3, 4) )
  return(c)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, matrix(c(12, 16, 12, 16) ,2, 2) )#77


f <- function(a) {
  b <- c(6, 8)
  sub(a, c(1, 2), c(3, 4)) = b + b

  sub(a, a <= 3) <- 5

  b <- sub(a, a == 5)
  return(b)
}
fetr <- translate(f)
ret <- test(fetr)
expect_equal(ret, matrix(c(2, 3, 3, 4) ,2, 2) )#78
