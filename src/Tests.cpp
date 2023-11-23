// [[Rcpp::plugins(cpp20)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include "./etr_new/UtilsTraits.hpp"
#include "./etr_new/unaryCalculations.hpp"
#include "./etr_new/binaryCalculations.hpp"
#include "./etr_new/subsetting.hpp"
#include "./etr_new/interpolation.hpp"
#include "./etr_new/helper.hpp"
#include "./etr_new/BufferVector.hpp"

typedef etr::Vec<etr::BaseType> sexp;
typedef etr::Vec<etr::BaseType, etr::BorrowSEXP<etr::BaseType, etr::BorrowSEXPTrait>> sexpSEXP;


// [[Rcpp::export]] 
SEXP bubbleSort(SEXP aSEXP) {
    sexpSEXP a(aSEXP);
    sexp size;
    sexp temp;
    size = etr::length(a);
    for (auto&i:  etr::colon(etr::i2d(1), size)) {;
       for (auto&j:  etr::colon(etr::i2d(1), (size - etr::i2d(1)))) {
           if (etr::subset(a, j) > etr::subset(a, j + etr::i2d(1))) {
               temp = etr::subset(a, j);
               etr::subset(a, j) = etr::subset(a, j + etr::i2d(1));
               etr::subset(a, j + etr::i2d(1)) = temp;
           }
       }
    }
    return(etr::cpp2R(a));
}

// [[Rcpp::export]]
SEXP fibonacci() {
    sexp ret = etr::vector(etr::i2d(20));
    etr::subset(ret, 1) = etr::i2d(1);
    etr::subset(ret, 2) = etr::i2d(1);
    for (auto&i:  etr::colon(etr::i2d(3), etr::length(ret))) {;
        etr::subset(ret, i) = etr::subset(ret, i - etr::i2d(1)) + etr::subset(ret, i - etr::i2d(2));
    };
    return(etr::cpp2R(ret));
}

// [[Rcpp::export]]
SEXP randomStuff1() {
    sexp m1 = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(12)), etr::i2d(3), etr::i2d(4));
    sexp m2 = etr::matrix(etr::i2d(50), etr::i2d(3), etr::i2d(4));
    sexp ret = m1 + m2 - etr::i2d(1);
    return(etr::cpp2R(ret));
}

// [[Rcpp::export]]
SEXP randomStuff2(SEXP aSEXP) {
    sexpSEXP a(aSEXP);
    etr::subset(a, 11) = etr::i2d(20);
    return(etr::cpp2R(a));
}

// [[Rcpp::export]]
SEXP randomStuff3(SEXP aSEXP) {
    sexpSEXP a(aSEXP);
     etr::subset(a, 0) = etr::i2d(20);
    return(etr::cpp2R(a));
}

// [[Rcpp::export]]
SEXP addTest(SEXP aSEXP, SEXP typeTest) {
    sexpSEXP type_test(typeTest);
    sexpSEXP a(aSEXP); sexp b;
    if (type_test == 1.0) {;
        a = a + etr::i2d(3);
        return(etr::cpp2R(a));
    } else if (type_test == 2.0) {;
        a = a + etr::vector(3.14, etr::i2d(4));
        return(etr::cpp2R(a));
    } else if (type_test == 3.0) {
        a = a + etr::matrix(etr::i2d(3), etr::i2d(2), etr::i2d(2));
        return(etr::cpp2R(a));
    } else if (type_test == 4.0) {;
        a = etr::i2d(1);
        a = a + 3.14;
        return(etr::cpp2R(a));
    } else if (type_test == 5.0) {;
        a = etr::i2d(1);
        a = a + etr::vector(etr::i2d(3), etr::i2d(4));
        return(etr::cpp2R(a));
    } else if (type_test == 6.0) {;
        b = etr::vector(etr::i2d(2));
        etr::subset(b, 1) = etr::i2d(100);
        etr::subset(b, 2) = etr::i2d(200);
        a = a + b;
        return(etr::cpp2R(a));
    } else if (type_test == 7.0) {;
        b = etr::vector(etr::i2d(9));
        etr::subset(b, 1) = etr::i2d(100);
        etr::subset(b, 2) = etr::i2d(200);
        etr::subset(b, 3) = etr::i2d(300);
        etr::subset(b, 4) = etr::i2d(400);
        etr::subset(b, 5) = etr::i2d(500);
        etr::subset(b, 6) = etr::i2d(600);
        etr::subset(b, 7) = etr::i2d(700);
        etr::subset(b, 8) = etr::i2d(800);
        etr::subset(b, 9) = etr::i2d(900);
        a = a + b;
        return(etr::cpp2R(a));
    } else if (type_test == 8.0) {;
        a = etr::i2d(1);
        a = etr::vector(etr::i2d(3), etr::i2d(4)) + a;
        return(etr::cpp2R(a));
    } else if (type_test == 9.0) {;
        a = etr::i2d(1);
        a = etr::i2d(1) + etr::i2d(4);
        return(etr::cpp2R(a));
    } else if (type_test == 10.0) {;
        a = a + a;
        return(etr::cpp2R(a));
    } else if (type_test == 11.0) {;
        a = a + etr::i2d(1);
        return(etr::cpp2R(a));
    } else if (type_test == 12.0) {;
        a = etr::i2d(4) + a;
        return(etr::cpp2R(a));
    } else if (type_test == 13.0) {;
        b = etr::vector(etr::i2d(3), etr::i2d(2));
        a = a + b;
        return(etr::cpp2R(a));
    };
    return etr::cpp2R();
}

// [[Rcpp::export]]
SEXP minusTest(SEXP aSEXP, SEXP typeTest) {
    sexpSEXP type_test(typeTest);
    sexpSEXP a(aSEXP); sexp b;
    if (type_test == 1.0) {;
        a = a - etr::i2d(3);
        return(etr::cpp2R(a));
    } else if (type_test == 2.0) {;
        a = a - etr::vector(3.14, etr::i2d(4));
        return(etr::cpp2R(a));
    } else if (type_test == 3.0) {;
        a = a - etr::matrix(etr::i2d(3), etr::i2d(2), etr::i2d(2));
        return(etr::cpp2R(a));
    } else if (type_test == 4.0) {;
        a = etr::i2d(1);
        a = a - 3.14;
        return(etr::cpp2R(a));
    } else if (type_test == 5.0) {;
        a = etr::i2d(1);
        a = a - etr::vector(etr::i2d(3), etr::i2d(4));
        return(etr::cpp2R(a));
    } else if (type_test == 6.0) {;
        b = etr::vector(etr::i2d(2));
        etr::subset(b, 1) = etr::i2d(100);
        etr::subset(b, 2) = etr::i2d(200);
        a = a - b;
        return(etr::cpp2R(a));
    } else if (type_test == 7.0) {;
        b = etr::vector(etr::i2d(9));
        etr::subset(b, 1) = etr::i2d(100);
        etr::subset(b, 2) = etr::i2d(200);
        etr::subset(b, 3) = etr::i2d(300);
        etr::subset(b, 4) = etr::i2d(400);
        etr::subset(b, 5) = etr::i2d(500);
        etr::subset(b, 6) = etr::i2d(600);
        etr::subset(b, 7) = etr::i2d(700);
        etr::subset(b, 8) = etr::i2d(800);
        etr::subset(b, 9) = etr::i2d(900);
        a = a - b;
        return(etr::cpp2R(a));
    } else if (type_test == 8.0) {;
        a = etr::i2d(1);
        a = etr::vector(etr::i2d(3), etr::i2d(4)) - a;
        return(etr::cpp2R(a));
    } else if (type_test == 9.0) {;
        a = etr::i2d(1);
        a = etr::i2d(1) - etr::i2d(4);
        return(etr::cpp2R(a));
    } else if (type_test == 10.0) {;
        a = a - a;
        return(etr::cpp2R(a));
    } else if (type_test == 11.0) {;
        a = a - etr::i2d(1);
        return(etr::cpp2R(a));
    } else if (type_test == 12.0) {;
        a = etr::i2d(4) - a;
        return(etr::cpp2R(a));
    } else if (type_test == 13.0) {;
        b = etr::vector(etr::i2d(3), etr::i2d(2));
        a = a - b;
        return(etr::cpp2R(a));
    } 
    return etr::cpp2R();
}

// [[Rcpp::export]]
SEXP divideTest(SEXP aSEXP, SEXP typeTest) {
    sexpSEXP type_test(typeTest);
    sexpSEXP a(aSEXP); sexp b;
    if (type_test == 1.0) {;
        a = a/etr::i2d(3);
        return(etr::cpp2R(a));
    } else if (type_test == 2.0) {;
        a = a/etr::vector(3.14, etr::i2d(4));
        return(etr::cpp2R(a));
    } else if (type_test == 3.0) {;
        a = a/etr::matrix(etr::i2d(3), etr::i2d(2), etr::i2d(2));
        return(etr::cpp2R(a));
    } else if (type_test == 4.0) {;
        a = etr::i2d(1);
        a = a/3.14;
        return(etr::cpp2R(a));
    } else if (type_test == 5.0) {;
        a = etr::i2d(1);
        a = a/etr::vector(etr::i2d(3), etr::i2d(4));
        return(etr::cpp2R(a));
    } else if (type_test == 6.0) {;
        b = etr::vector(etr::i2d(2));
        etr::subset(b, 1) = etr::i2d(100);
        etr::subset(b, 2) = etr::i2d(200);
        a = a/b;
        return(etr::cpp2R(a));
    } else if (type_test == 7.0) {;
        b = etr::vector(etr::i2d(9));
        etr::subset(b, 1) = etr::i2d(100);
        etr::subset(b, 2) = etr::i2d(200);
        etr::subset(b, 3) = etr::i2d(300);
        etr::subset(b, 4) = etr::i2d(400);
        etr::subset(b, 5) = etr::i2d(500);
        etr::subset(b, 6) = etr::i2d(600);
        etr::subset(b, 7) = etr::i2d(700);
        etr::subset(b, 8) = etr::i2d(800);
        etr::subset(b, 9) = etr::i2d(900);
        a = a/b;
        return(etr::cpp2R(a));
    } else if (type_test == 8.0) {;
        a = etr::i2d(1);
        a = etr::vector(etr::i2d(3), etr::i2d(4))/a;
        return(etr::cpp2R(a));
    } else if (type_test == 9.0) {;
        a = etr::i2d(1);
        a = etr::i2d(1)/etr::i2d(4);
        return(etr::cpp2R(a));
    } else if (type_test == 10.0) {;
        a = a/a;
        return(etr::cpp2R(a));
    } else if (type_test == 11.0) {;
        a = a/etr::i2d(1);
        return(etr::cpp2R(a));
    } else if (type_test == 12.0) {;
        a = etr::i2d(4)/a;
        return(etr::cpp2R(a));
    } else if (type_test == 13.0) {;
        b = etr::vector(etr::i2d(3), etr::i2d(2));
        a = a/b;
        return(etr::cpp2R(a));
    }
    return etr::cpp2R();
}

// [[Rcpp::export]]
SEXP timesTest(SEXP aSEXP, SEXP typeTest) {
    sexpSEXP type_test(typeTest);
    sexpSEXP a(aSEXP); sexp b;
    if (type_test == 1.0) {;
        a = a * etr::i2d(3);
        return(etr::cpp2R(a));
    } else if (type_test == 2.0) {;
        a = a * etr::vector(3.14, etr::i2d(4));
        return(etr::cpp2R(a));
    } else if (type_test == 3.0) {;
        a = a * etr::matrix(etr::i2d(3), etr::i2d(2), etr::i2d(2));
        return(etr::cpp2R(a));
    } else if (type_test == 4.0) {;
        a = etr::i2d(1);
        a = a * 3.14;
        return(etr::cpp2R(a));
    } else if (type_test == 5.0) {;
        a = etr::i2d(1);
        a = a * etr::vector(etr::i2d(3), etr::i2d(4));
        return(etr::cpp2R(a));
    } else if (type_test == 6.0) {;
        b = etr::vector(etr::i2d(2));
        etr::subset(b, 1) = etr::i2d(100);
        etr::subset(b, 2) = etr::i2d(200);
        a = a * b;
        return(etr::cpp2R(a));
    } else if (type_test == 7.0) {;
        b = etr::vector(etr::i2d(9));
        etr::subset(b, 1) = etr::i2d(100);
        etr::subset(b, 2) = etr::i2d(200);
        etr::subset(b, 3) = etr::i2d(300);
        etr::subset(b, 4) = etr::i2d(400);
        etr::subset(b, 5) = etr::i2d(500);
        etr::subset(b, 6) = etr::i2d(600);
        etr::subset(b, 7) = etr::i2d(700);
        etr::subset(b, 8) = etr::i2d(800);
        etr::subset(b, 9) = etr::i2d(900);
        a = a * b;
        return(etr::cpp2R(a));
    } else if (type_test == 8.0) {;
        a = etr::i2d(1);
        a = etr::vector(etr::i2d(3), etr::i2d(4)) * a;
        return(etr::cpp2R(a));
    } else if (type_test == 9.0) {;
        a = etr::i2d(1);
        a = etr::i2d(1) * etr::i2d(4);
        return(etr::cpp2R(a));
    } else if (type_test == 10.0) {;
        a = a * a;
        return(etr::cpp2R(a));
    } else if (type_test == 11.0) {;
        a = a * etr::i2d(1);
        return(etr::cpp2R(a));
    } else if (type_test == 12.0) {;
        a = etr::i2d(4) * a;
        return(etr::cpp2R(a));
    } else if (type_test == 13.0) {;
        b = etr::vector(etr::i2d(3), etr::i2d(2));
        a = a * b;
        return(etr::cpp2R(a));
    };
    return etr::cpp2R();
}

// [[Rcpp::export]]
SEXP mixedStuffTest(SEXP aSEXP, SEXP typeTest) {
    sexpSEXP type_test(typeTest);
    sexpSEXP a(aSEXP); sexp b;
    sexp c, d, e, f, m, v;
    if (type_test == 7.1) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(7));
        c = etr::coca(-etr::i2d(100), a, b, etr::i2d(100));
        d = etr::vector(etr::i2d(1), etr::i2d(2));
        e = etr::matrix(-etr::i2d(1), etr::i2d(2), etr::i2d(2));
        f = etr::coca(c, d, e, etr::i2d(2));
        return(etr::cpp2R(f));
    } else if (type_test == 7.2) {;
        v = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(4));
        m = etr::matrix(etr::i2d(2), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::length(v), etr::dim(m));
        return(etr::cpp2R(b));
    } else if (type_test == 8.1) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(7));
        return(etr::cpp2R(a == b));
    } else if (type_test == 8.2) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        return(etr::cpp2R(a == a));
    } else if (type_test == 8.3) {;
        a = etr::coca(etr::i2d(2), etr::i2d(2), etr::i2d(2));
        return(etr::cpp2R(a == etr::i2d(2)));
    } else if (type_test == 9.1) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(2));
        return(etr::cpp2R(a <= b));
    } else if (type_test == 9.2) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(4));
        return(etr::cpp2R(a <= a));
    } else if (type_test == 9.3) {;
        a = etr::coca(etr::i2d(2), etr::i2d(2), etr::i2d(2));
        return(etr::cpp2R(a <= etr::i2d(2)));
    } else if (type_test == 10.1) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(7));
        return(etr::cpp2R(a < b));
    } else if (type_test == 10.2) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        return(etr::cpp2R(a < a));
    } else if (type_test == 10.3) {;
        a = etr::coca(etr::i2d(2), etr::i2d(2), etr::i2d(2));
        return(etr::cpp2R(a < etr::i2d(2)));
    } else if (type_test == 11.1) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(7));
        return(etr::cpp2R(a > b));
    } else if (type_test == 11.2) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        return(etr::cpp2R(a > a));
    } else if (type_test == 11.3) {;
        a = etr::coca(etr::i2d(2), etr::i2d(2), etr::i2d(2));
        return(etr::cpp2R(a > etr::i2d(1)));
    } else if (type_test == 12.1) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(3));
        return(etr::cpp2R(a >= b));
    } else if (type_test == 12.2) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        return(etr::cpp2R(a >= a));
    } else if (type_test == 12.3) {;
        a = etr::coca(etr::i2d(2), etr::i2d(2), etr::i2d(2));
        return(etr::cpp2R(a >= etr::i2d(1)));
    } else if (type_test == 13.1) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(3));
        return(etr::cpp2R(a != b));
    } else if (type_test == 13.2) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        return(etr::cpp2R(a != a));
    } else if (type_test == 13.3) {;
        a = etr::coca(etr::i2d(2), etr::i2d(2), etr::i2d(2));
        return(etr::cpp2R(a != etr::i2d(1)));
    } else if (type_test == 14.1) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(7));
        return(etr::cpp2R(a + b == b - a));
    } else if (type_test == 14.2) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(7));
        return(etr::cpp2R(a + b == etr::coca(etr::i2d(100), etr::i2d(100), etr::i2d(200))));
    } else if (type_test == 14.3) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(7));
        return(etr::cpp2R(a + b != b - a));
    } else if (type_test == 14.4) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(7));
        return(etr::cpp2R(a + b != etr::coca(etr::i2d(100), etr::i2d(100), etr::i2d(200))));
    } else if (type_test == 14.5) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(7));
        return(etr::cpp2R(a + b > b - a));
    } else if (type_test == 14.6) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(7));
        return(etr::cpp2R(a + b < etr::coca(etr::i2d(100), etr::i2d(100), etr::i2d(200))));
    } else if (type_test == 14.7) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(7));
        return(etr::cpp2R(a + b >= b - a));
    } else if (type_test == 14.8) {;
        a = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3));
        b = etr::coca(etr::i2d(5), etr::i2d(6), etr::i2d(7));
        return(etr::cpp2R(a + b <= etr::coca(etr::i2d(100), etr::i2d(100), etr::i2d(200))));
    };
    return etr::cpp2R();
}

/*** R
# bubblesort
library(tinytest)
x <- c(1, 5, 2, 3, 8, 10, 1.5)
ret <- bubbleSort(x)
print(ret)
expect_equal(ret, c(1, 1.5, 2, 3, 5, 8, 10))  

ret <- fibonacci()
res <- c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765)
expect_equal(ret, res)  

expect_equal(randomStuff1(), matrix(50:61, 3, 4))
expect_error(randomStuff2())
expect_error(randomStuff3())

expect_equal(addTest(c(1, 2, 3, 4), 1), c(4, 5, 6, 7))
expect_equal(addTest(c(1, 2, 3, 4), 2), c(1, 2, 3, 4) + 3.14)
expect_equal(addTest(c(1, 2, 3, 4), 3), matrix(4:7, 2, 2))
expect_equal(addTest(0, 4), 4.14)
expect_equal(addTest(0, 5), rep(4, 4))
expect_equal(addTest(c(1, 2, 3, 4), 6), c(101, 202, 103, 204))  
expect_equal(addTest(c(1, 2, 3, 4), 7), c(101, 202, 303, 404, 501, 602, 703, 804, 901))  
expect_equal(addTest(c(1, 2, 3, 4), 8), rep(4, 4))
expect_equal(addTest(c(1, 2, 3, 4), 9), 5)
expect_equal(addTest(matrix(3, 2, 2), 10), matrix(6, 2, 2))
expect_equal(addTest(matrix(3, 2, 2), 11), matrix(4, 2, 2))
expect_equal(addTest(matrix(3, 2, 2), 12), matrix(7, 2, 2))
expect_equal(addTest(matrix(3, 2, 2), 13), matrix(6, 2, 2))

expect_equal(minusTest(c(1, 2, 3, 4), 1), c(1, 2, 3, 4) - 3)
expect_equal(minusTest(c(1, 2, 3, 4), 2), c(1, 2, 3, 4) - 3.14)
expect_equal(minusTest(c(1, 2, 3, 4), 3), 1:4 - matrix(3, 2, 2))
expect_equal(minusTest(0, 4), 1 - 3.14)
expect_equal(minusTest(0, 5), 1 - rep(3, 4))
expect_equal(minusTest(c(1, 2, 3, 4), 6), c(1, 2, 3, 4) - c(100, 200))  # failed
expect_equal(minusTest(c(1, 2, 3, 4), 7), c(1, 2, 3, 4) - seq(100, 900, 100))  # failed
expect_equal(minusTest(c(1, 2, 3, 4), 8), rep(3, 4) - 1)
expect_equal(minusTest(c(1, 2, 3, 4), 9), 1 - 4)
expect_equal(minusTest(matrix(3, 2, 2), 10), matrix(3, 2, 2) - matrix(3, 2, 2))
expect_equal(minusTest(matrix(3, 2, 2), 11), matrix(3, 2, 2) - 1)
expect_equal(minusTest(matrix(3, 2, 2), 12), 4 - matrix(3, 2, 2))
expect_equal(minusTest(matrix(3, 2, 2), 13), matrix(3, 2, 2) - rep(3, 2))

expect_equal(divideTest(c(1, 2, 3, 4), 1), c(1, 2, 3, 4) / 3)
expect_equal(divideTest(c(1, 2, 3, 4), 2), c(1, 2, 3, 4) / 3.14)
expect_equal(divideTest(c(1, 2, 3, 4), 3), 1:4 / matrix(3, 2, 2))
expect_equal(divideTest(0, 4), 1 / 3.14)
expect_equal(divideTest(0, 5), 1 / rep(3, 4))
expect_equal(divideTest(c(1, 2, 3, 4), 6), c(1, 2, 3, 4) / c(100, 200))  
expect_equal(divideTest(c(1, 2, 3, 4), 7), c(1, 2, 3, 4) / seq(100, 900, 100))
expect_equal(divideTest(c(1, 2, 3, 4), 8), rep(3, 4) / 1)
expect_equal(divideTest(c(1, 2, 3, 4), 9), 1 / 4)
expect_equal(divideTest(matrix(3, 2, 2), 10), matrix(3, 2, 2) / matrix(3, 2, 2))
expect_equal(divideTest(matrix(3, 2, 2), 11), matrix(3, 2, 2) / 1)
expect_equal(divideTest(matrix(3, 2, 2), 12), 4 / matrix(3, 2, 2))
expect_equal(divideTest(matrix(3, 2, 2), 13), matrix(3, 2, 2) / rep(3, 2))

expect_equal(timesTest(c(1, 2, 3, 4), 1), c(1, 2, 3, 4) * 3)
expect_equal(timesTest(c(1, 2, 3, 4), 2), c(1, 2, 3, 4) * 3.14)
expect_equal(timesTest(c(1, 2, 3, 4), 3), 1:4 * matrix(3, 2, 2))
expect_equal(timesTest(0, 4), 1 * 3.14)
expect_equal(timesTest(0, 5), 1 * rep(3, 4))
expect_equal(timesTest(c(1, 2, 3, 4), 6), c(1, 2, 3, 4) * c(100, 200))  # failed
expect_equal(timesTest(c(1, 2, 3, 4), 7), c(1, 2, 3, 4) * seq(100, 900, 100))
expect_equal(timesTest(c(1, 2, 3, 4), 8), rep(3, 4) * 1)
expect_equal(timesTest(c(1, 2, 3, 4), 9), 1 * 4)
expect_equal(timesTest(matrix(3, 2, 2), 10), matrix(3, 2, 2) * matrix(3, 2, 2))
expect_equal(timesTest(matrix(3, 2, 2), 11), matrix(3, 2, 2) * 1)
expect_equal(timesTest(matrix(3, 2, 2), 12), 4 * matrix(3, 2, 2))
expect_equal(timesTest(matrix(3, 2, 2), 13), matrix(3, 2, 2) * rep(3, 2))

expect_equal(mixedStuffTest(0, 7.1), c(-100, 1, 2, 3, 5, 6, 7, 100, 1, 1, rep(-1, 4), 2))
expect_equal(mixedStuffTest(0, 7.2), c(4, 2, 3))
expect_equal(mixedStuffTest(0, 8.1), as.numeric(c(1, 2, 3) == c(5, 6, 7)))
expect_equal(mixedStuffTest(0, 8.2), as.numeric(c(1, 2, 3) == c(1, 2, 3)))
expect_equal(mixedStuffTest(0, 8.3), as.numeric(c(2, 2, 2) == 2))
expect_equal(mixedStuffTest(0, 10.1), as.numeric(c(1, 2, 3) < c(5, 6, 7)))
expect_equal(mixedStuffTest(0, 10.2), as.numeric(c(1, 2, 3) < c(1, 2, 3)))
expect_equal(mixedStuffTest(0, 10.3), as.numeric(c(2, 2, 2) < 2))
expect_equal(mixedStuffTest(0, 11.1), as.numeric(c(1, 2, 3) > c(5, 6, 7)))
expect_equal(mixedStuffTest(0, 11.2), as.numeric(c(1, 2, 3) > c(1, 2, 3)))
expect_equal(mixedStuffTest(0, 11.3), as.numeric(c(2, 2, 2) > 1))
expect_equal(mixedStuffTest(0, 12.1), as.numeric(c(1, 2, 3) >= c(5, 6, 3)))
expect_equal(mixedStuffTest(0, 12.2), as.numeric(c(1, 2, 3) >= c(1, 2, 3)))
expect_equal(mixedStuffTest(0, 12.3), as.numeric(c(2, 2, 2) >= 1))
expect_equal(mixedStuffTest(0, 13.1), as.numeric(c(1, 2, 3) != c(5, 6, 3)))
expect_equal(mixedStuffTest(0, 13.2), as.numeric(c(1, 2, 3) != c(1, 2, 3)))
expect_equal(mixedStuffTest(0, 13.3), as.numeric(c(2, 2, 2) != 1))
expect_equal(mixedStuffTest(0, 14.1), rep(as.numeric(FALSE), 3))
expect_equal(mixedStuffTest(0, 14.2), rep(as.numeric(FALSE), 3))
expect_equal(mixedStuffTest(0, 14.3), rep(as.numeric(TRUE), 3))
expect_equal(mixedStuffTest(0, 14.4), rep(as.numeric(TRUE), 3))
expect_equal(mixedStuffTest(0, 14.5), rep(as.numeric(TRUE), 3))
expect_equal(mixedStuffTest(0, 14.6), rep(as.numeric(TRUE), 3))
expect_equal(mixedStuffTest(0, 14.7), rep(as.numeric(TRUE), 3))
expect_equal(mixedStuffTest(0, 14.8), rep(as.numeric(TRUE), 3))
*/



/*

if (type_test == 15.1) {;
    a = etr::exp(a, etr::i2d(2)) + etr::ln(etr::i2d(1));
    return(etr::cpp2R(a));
} else if (type_test == 15.2) {;
    a = etr::exp(a, etr::i2d(3));
    return(etr::cpp2R(a));
} else if (type_test == 15.3) {;
    a = etr::exp(a, etr::i2d(3)) + etr::i2d(1);
    return(etr::cpp2R(a));
} else if (type_test == 15.4) {;
    a = sqroot(a);
    return(etr::cpp2R(a));
};

if (type_test == 16.1) {;
    for (auto&i:  etr::colon(etr::i2d(1), etr::length(a))) {;
        etr::subset(a, i) = etr::i2d(100);
    };
    return(etr::cpp2R(a));
} else if (type_test == 16.2) {;
    etr::subset(a, true) = etr::i2d(0);
    return(etr::cpp2R(a));
} else if (type_test == 16.3) {;
    etr::subset(a, 1) = etr::i2d(0);
    etr::subset(a, 2.3) = etr::i2d(0);
    return(etr::cpp2R(a));
} else if (type_test == 16.4) {;
    etr::subset(a, true) = etr::i2d(0);
    return(etr::cpp2R(a));
} else if (type_test == 16.5) {;
    b = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(4), etr::i2d(1));
    etr::subset(a, b) = etr::colon(etr::i2d(1), etr::i2d(5));
    return(etr::cpp2R(a));
} else if (type_test == 16.6) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    etr::subset(a, 4, 4) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.7) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    etr::subset(a, 4, 4.5) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.8) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    etr::subset(a, 4, true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.9) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    etr::subset(a, 4, true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.11) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    etr::subset(a, 4, etr::coca(etr::i2d(1), etr::i2d(4))) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.12) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(4)), etr::i2d(2), etr::i2d(2));
    etr::subset(a, 1.3, 1) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.13) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(4)), etr::i2d(2), etr::i2d(2));
    etr::subset(a, true, 1) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.14) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(4)), etr::i2d(2), etr::i2d(2));
    etr::subset(a, true, 1) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.15) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(6)), etr::i2d(3), etr::i2d(2));
    b = etr::coca(etr::i2d(1), etr::i2d(3));
    etr::subset(a, b, 1) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.16) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(6)), etr::i2d(3), etr::i2d(2));
    etr::subset(a, 1.5, 1.1) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.17) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(6)), etr::i2d(3), etr::i2d(2));
    etr::subset(a, 1.5, true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.18) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(6)), etr::i2d(3), etr::i2d(2));
    etr::subset(a, 1.5, true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.19) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(3));
    etr::subset(a, 1.5, b) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.21) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    etr::subset(a, true, 1.5) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.22) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    etr::subset(a, true, 1.5) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.23) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, b, 1.5) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.24) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    etr::subset(a, true, true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.25) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    etr::subset(a, true, true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.26) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(2), etr::i2d(3));
    etr::subset(a, true, b) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.27) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    etr::subset(a, true, true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.28) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, b, true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.29) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    etr::subset(a, true, true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.31) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, true, b) = etr::i2d(20);
    return(etr::cpp2R(a));
};

if (type_test == 16.32) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(3));
    etr::subset(a, b, true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.33) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(3));
    c = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, b, c) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.34) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    c = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, c + c) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.35) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    etr::subset(a, a < etr::i2d(5)) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.36) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    c = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, 1, c + c) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.37) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(4));
    etr::subset(a, 1, b >= etr::i2d(3)) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.38) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, b + b, 1) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.39) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(4));
    etr::subset(a, b > etr::i2d(3), 4) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.41) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, 3.5, b + b) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.42) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(4));
    etr::subset(a, 3.5, b <= etr::i2d(1)) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.43) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, b + b, 1.5) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.44) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(4));
    etr::subset(a, b >= etr::i2d(1), 1.5) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.45) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, true, b + b) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.46) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(4));
    etr::subset(a, true, b >= etr::i2d(3)) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.47) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, b + b, true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.48) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(4));
    etr::subset(a, b == etr::i2d(1), true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.49) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, true, b + b) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.51) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(4));
    etr::subset(a, true, b >= etr::i2d(3)) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.52) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, b + b, true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.53) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(4));
    etr::subset(a, b == etr::i2d(1), true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.54) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    etr::subset(a, b + b, b + b) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.55) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    c = etr::coca(etr::i2d(2), etr::i2d(4));
    etr::subset(a, b + b, c) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.56) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    c = etr::coca(etr::i2d(2), etr::i2d(4));
    etr::subset(a, c, b + b) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.57) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    c = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(2));
    etr::subset(a, b + b, c == etr::i2d(2)) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.58) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(2), etr::i2d(4));
    c = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(2));
    etr::subset(a, b, c == etr::i2d(2)) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.59) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    c = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(2));
    etr::subset(a, c == etr::i2d(2), b + b) = etr::i2d(20);
    return(etr::cpp2R(a));
};

if (type_test == 16.61) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(2), etr::i2d(4));
    c = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(2));
    etr::subset(a, c == etr::i2d(2), b) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.62) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    c = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(2));
    etr::subset(a, c == etr::i2d(2), c == etr::i2d(2)) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.63) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(2), etr::i2d(4));
    c = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(2));
    etr::subset(a, b, c == etr::i2d(2)) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.64) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    b = etr::coca(etr::i2d(2), etr::i2d(4));
    c = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(2));
    etr::subset(a, c == etr::i2d(2), b) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 16.65) {;
    a = etr::colon(etr::i2d(1), etr::i2d(6));
    b = etr::coca(etr::i2d(4), etr::i2d(4));
    etr::subset(b, 1) = etr::at(a, 2);
    etr::subset(b, 2) = etr::at(a, 2);
    return(etr::cpp2R(b));
} else if (type_test == 16.66) {;
    a = etr::colon(etr::i2d(1), etr::i2d(6));
    b = etr::subset(a, true);
    c = etr::subset(b, true);
    return(etr::cpp2R(c));
} else if (type_test == 16.67) {;
    a = etr::colon(etr::i2d(1), etr::i2d(6));
    b = etr::coca(etr::i2d(1), etr::i2d(5));
    b = etr::subset(a, b);
    return(etr::cpp2R(b));
} else if (type_test == 16.68) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, 1, true);
    c = etr::subset(a, 1, true);
    return(etr::cpp2R(b - c));
} else if (type_test == 16.69) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(3));
    b = etr::subset(a, 1, b);
    return(etr::cpp2R(b));
} else if (type_test == 16.71) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, 1.1, 2);
    return(etr::cpp2R(b));
} else if (type_test == 16.72) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, true, 2);
    return(etr::cpp2R(b));
} else if (type_test == 16.73) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, true, 2);
    return(etr::cpp2R(b));
} else if (type_test == 16.74) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(3));
    b = etr::subset(a, b, 2);
    return(etr::cpp2R(b));
} else if (type_test == 16.75) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, 1.1, 2.2);
    return(etr::cpp2R(b));
} else if (type_test == 16.76) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, 1.1, true);
    return(etr::cpp2R(b));
} else if (type_test == 16.77) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, 1.1, true);
    return(etr::cpp2R(b));
} else if (type_test == 16.78) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(3));
    b = etr::subset(a, 1.1, b);
    return(etr::cpp2R(b));
} else if (type_test == 16.79) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, true, 2.1);
    return(etr::cpp2R(b));
} else if (type_test == 16.81) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, true, 2.1);
    return(etr::cpp2R(b));
} else if (type_test == 16.82) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(3));
    b = etr::subset(a, b, 2.5);
    return(etr::cpp2R(b));
} else if (type_test == 16.83) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, true, true);
    return(etr::cpp2R(b));
} else if (type_test == 16.84) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, true, true);
    return(etr::cpp2R(b));
} else if (type_test == 16.85) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset(a, true, b);
    return(etr::cpp2R(b));
} else if (type_test == 16.86) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, true, true);
    return(etr::cpp2R(b));
} else if (type_test == 16.87) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset(a, b, true);
    return(etr::cpp2R(b));
} else if (type_test == 16.88) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, true, true);
    return(etr::cpp2R(b));
} else if (type_test == 16.89) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset(a, true, b);
    return(etr::cpp2R(b));
} else if (type_test == 16.91) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset(a, b, true);
    return(etr::cpp2R(b));
} else if (type_test == 16.92) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset(a, b, b);
    return(etr::cpp2R(b));
} else if (type_test == 16.93) {;
    a = etr::colon(etr::i2d(1), etr::i2d(6));
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset(a, b + b);
    return(etr::cpp2R(b));
} else if (type_test == 16.94) {;
    a = etr::colon(etr::i2d(1), etr::i2d(6));
    b = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(4));
    c = etr::coca(etr::i2d(1), etr::i2d(5), etr::i2d(3), 6.7);
    b = etr::subset(a, b == c);
    return(etr::cpp2R(b));
} else if (type_test == 16.95) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, 1, b + c);
    return(etr::cpp2R(b));
} else if (type_test == 16.96) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, 1, b != c);
    return(etr::cpp2R(b));
} else if (type_test == 16.97) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, b + c, 1);
    return(etr::cpp2R(b));
} else if (type_test == 16.98) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, b != c, 2);
    return(etr::cpp2R(b));
} else if (type_test == 16.99) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, 1.5, b + c);
    return(etr::cpp2R(b));
};

if (type_test == 17.1) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, 1.4, b != c);
    return(etr::cpp2R(b));
} else if (type_test == 17.2) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, b + c, 1.5);
    return(etr::cpp2R(b));
} else if (type_test == 17.3) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, b != c, 2.5);
    return(etr::cpp2R(b));
} else if (type_test == 17.4) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, true, b + c);
    return(etr::cpp2R(b));
} else if (type_test == 17.5) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1), etr::i2d(2), etr::i2d(3));
    c = etr::coca(etr::i2d(1), etr::i2d(1), etr::i2d(2), etr::i2d(5));
    b = etr::subset(a, true, b == c);
    return(etr::cpp2R(b));
} else if (type_test == 17.6) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, b + c, true);
    return(etr::cpp2R(b));
} else if (type_test == 17.7) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, b == c, true);
    return(etr::cpp2R(b));
} else if (type_test == 17.8) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, true, b + c);
    return(etr::cpp2R(b));
} else if (type_test == 17.9) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1), etr::i2d(2), etr::i2d(3));
    c = etr::coca(etr::i2d(1), etr::i2d(1), etr::i2d(2), etr::i2d(5));
    b = etr::subset(a, true, b == c);
    return(etr::cpp2R(b));
} else if (type_test == 17.11) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, b + c, true);
    return(etr::cpp2R(b));
} else if (type_test == 17.12) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, b == c, true);
    return(etr::cpp2R(b));
} else if (type_test == 17.13) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, b + c, b + c);
    return(etr::cpp2R(b));
} else if (type_test == 17.14) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    rs = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset(a, rs, b + c);
    return(etr::cpp2R(b));
} else if (type_test == 17.15) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    cs = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset(a, b + c, cs);
    return(etr::cpp2R(b));
} else if (type_test == 17.16) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, b + c, b == c);
    return(etr::cpp2R(b));
} else if (type_test == 17.17) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    rs = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset(a, rs, b == c);
    return(etr::cpp2R(b));
} else if (type_test == 17.18) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, b == c, b + c);
    return(etr::cpp2R(b));
} else if (type_test == 17.19) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    cs = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset(a, b == c, cs);
    return(etr::cpp2R(b));
} else if (type_test == 17.21) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, b == c, b == c);
    return(etr::cpp2R(b));
} else if (type_test == 17.22) {;
    a = etr::colon(etr::i2d(1), etr::i2d(6));
    b = etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(5)));
    return(etr::cpp2R(b));
} else if (type_test == 17.23) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, 1, etr::coca(etr::i2d(1), etr::i2d(3)));
    return(etr::cpp2R(b));
} else if (type_test == 17.24) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(3)), 2);
    return(etr::cpp2R(b));
} else if (type_test == 17.25) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, 1.1, etr::coca(etr::i2d(1), etr::i2d(3)));
    return(etr::cpp2R(b));
} else if (type_test == 17.26) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(3)), 2.5);
    return(etr::cpp2R(b));
} else if (type_test == 17.27) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, true, etr::coca(etr::i2d(1), etr::i2d(2)));
    return(etr::cpp2R(b));
} else if (type_test == 17.28) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(2)), true);
    return(etr::cpp2R(b));
} else if (type_test == 17.29) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, true, etr::coca(etr::i2d(1), etr::i2d(2)));
    return(etr::cpp2R(b));
};

if (type_test == 17.31) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(2)), true);
    return(etr::cpp2R(b));
} else if (type_test == 17.32) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(2)), etr::coca(etr::i2d(1), etr::i2d(2)));
    return(etr::cpp2R(b));
} else if (type_test == 17.33) {;
    a = etr::colon(etr::i2d(1), etr::i2d(6));
    b = etr::subset(a, etr::coca(etr::i2d(2), etr::i2d(4)));
    return(etr::cpp2R(b));
} else if (type_test == 17.34) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, 1, etr::coca(etr::i2d(1), etr::i2d(2)));
    return(etr::cpp2R(b));
} else if (type_test == 17.35) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(2)), 1);
    return(etr::cpp2R(b));
} else if (type_test == 17.36) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, 1.5, etr::coca(etr::i2d(1), etr::i2d(2)));
    return(etr::cpp2R(b));
} else if (type_test == 17.37) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(2)), 1.5);
    return(etr::cpp2R(b));
} else if (type_test == 17.38) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, true, etr::coca(etr::i2d(1), etr::i2d(2)));
    return(etr::cpp2R(b));
} else if (type_test == 17.39) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(2)), true);
    return(etr::cpp2R(b));
} else if (type_test == 17.41) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, true, etr::coca(etr::i2d(1), etr::i2d(2)));
    return(etr::cpp2R(b));
} else if (type_test == 17.42) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(2)), true);
    return(etr::cpp2R(b));
} else if (type_test == 17.43) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(2)), etr::coca(etr::i2d(1), etr::i2d(2)));
    return(etr::cpp2R(b));
} else if (type_test == 17.44) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    rs = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset(a, rs, etr::coca(etr::i2d(1), etr::i2d(2)));
    return(etr::cpp2R(b));
} else if (type_test == 17.45) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    cs = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(2)), cs);
    return(etr::cpp2R(b));
} else if (type_test == 17.46) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(2)), b == c);
    return(etr::cpp2R(b));
} else if (type_test == 17.47) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3));
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset(a, b == c, etr::coca(etr::i2d(1), etr::i2d(2)));
    return(etr::cpp2R(b));
};

if (type_test == 18.1) {;
    etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(4), etr::i2d(1))) = etr::colon(etr::i2d(1), etr::i2d(5));
    return(etr::cpp2R(a));
} else if (type_test == 18.2) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    etr::subset(a, 4, etr::coca(etr::i2d(1), etr::i2d(4))) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 18.3) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(6)), etr::i2d(3), etr::i2d(2));
    etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(3)), 1) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 18.4) {;
    etr::subset(a, 1.5, etr::coca(etr::i2d(1), etr::i2d(3))) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 18.5) {;
    etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(2)), 1.5) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 18.6) {;
    etr::subset(a, true, etr::coca(etr::i2d(2), etr::i2d(3))) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 18.7) {;
    etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(2)), true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 18.8) {;
    etr::subset(a, true, etr::coca(etr::i2d(1), etr::i2d(2))) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 18.9) {;
    etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(3)), true) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 18.91) {;
    etr::subset(a, etr::coca(etr::i2d(1), etr::i2d(3)), etr::coca(etr::i2d(1), etr::i2d(2))) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 18.92) {;
    a = etr::matrix(etr::colon(etr::i2d(1), etr::i2d(16)), etr::i2d(4), etr::i2d(4));
    etr::subset(a, etr::coca(etr::i2d(2), etr::i2d(4))) = etr::i2d(20);
    return(etr::cpp2R(a));
} else if (type_test == 19.1) {;
    b = etr::at((etr::colon(etr::i2d(1), etr::i2d(6)) + etr::i2d(0)), 2);
    return(etr::cpp2R(b));
} else if (type_test == 19.11) {;
    b = etr::subset((etr::colon(etr::i2d(1), etr::i2d(6)) + etr::i2d(0)), 3.5);
    return(etr::cpp2R(b));
} else if (type_test == 19.12) {;
    b = etr::subset((etr::colon(etr::i2d(1), etr::i2d(6)) + etr::i2d(0)), true);
    return(etr::cpp2R(b));
} else if (type_test == 19.13) {;
    b = etr::subset((etr::colon(etr::i2d(1), etr::i2d(6)) + etr::i2d(0)), true);
    return(etr::cpp2R(b));
} else if (type_test == 19.14) {;
    b = etr::coca(etr::i2d(1), etr::i2d(5));
    b = etr::subset((etr::colon(etr::i2d(1), etr::i2d(6)) + etr::i2d(0)), b);
    return(etr::cpp2R(b));
} else if (type_test == 19.15) {;
    b = etr::at((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), 1, 2);
    return(etr::cpp2R(b));
} else if (type_test == 19.16) {;
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), 1, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.17) {;
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), 1, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.18) {;
    b = etr::coca(etr::i2d(1), etr::i2d(3));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), 1, b);
    return(etr::cpp2R(b));
} else if (type_test == 19.19) {;
    b = etr::at((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), 1, 2);
    return(etr::cpp2R(b));
} else if (type_test == 19.21) {;
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), true, 2);
    return(etr::cpp2R(b));
} else if (type_test == 19.22) {;
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), true, 2);
    return(etr::cpp2R(b));
} else if (type_test == 19.23) {;
    b = etr::coca(etr::i2d(1), etr::i2d(3));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b, 2);
    return(etr::cpp2R(b));
} else if (type_test == 19.24) {;
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), 1.1, 2.3);
    return(etr::cpp2R(b));
} else if (type_test == 19.25) {;
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), 1.1, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.26) {;
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), 1.1, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.27) {;
    b = etr::coca(etr::i2d(1), etr::i2d(3));
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), 1.1, b);
    return(etr::cpp2R(b));
} else if (type_test == 19.28) {;
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), true, 2.1);
    return(etr::cpp2R(b));
} else if (type_test == 19.29) {;
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), true, 2.1);
    return(etr::cpp2R(b));
} else if (type_test == 19.31) {;
    b = etr::coca(etr::i2d(1), etr::i2d(3));
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), b, 2.5);
    return(etr::cpp2R(b));
};

if (type_test == 19.32) {;
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), true, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.33) {;
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), true, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.34) {;
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), true, b);
    return(etr::cpp2R(b));
} else if (type_test == 19.35) {;
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), true, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.36) {;
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), b, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.37) {;
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), true, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.38) {;
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), true, b);
    return(etr::cpp2R(b));
} else if (type_test == 19.39) {;
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), b, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.41) {;
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset((etr::matrix(etr::colon(etr::i2d(1), etr::i2d(9)), etr::i2d(3), etr::i2d(3)) + etr::i2d(0)), b, b);
    return(etr::cpp2R(b));
} else if (type_test == 19.42) {;
    b = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset((etr::colon(etr::i2d(1), etr::i2d(6)) + etr::i2d(1)), b + b);
    return(etr::cpp2R(b));
} else if (type_test == 19.43) {;
    b = etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3), etr::i2d(4));
    c = etr::coca(etr::i2d(1), etr::i2d(5), etr::i2d(3), 6.7);
    b = etr::subset((etr::colon(etr::i2d(1), etr::i2d(6)) + etr::i2d(0)), b == c);
    return(etr::cpp2R(b));
} else if (type_test == 19.44) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), 1, b + c);
    return(etr::cpp2R(b));
} else if (type_test == 19.45) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), 1, b != c);
    return(etr::cpp2R(b));
} else if (type_test == 19.46) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b + c, 1);
    return(etr::cpp2R(b));
} else if (type_test == 19.47) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b != c, 2);
    return(etr::cpp2R(b));
} else if (type_test == 19.48) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), 1.5, b + c);
    return(etr::cpp2R(b));
} else if (type_test == 19.49) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), 1.4, b != c);
    return(etr::cpp2R(b));
} else if (type_test == 19.51) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b + c, 1.5);
    return(etr::cpp2R(b));
} else if (type_test == 19.52) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b != c, 2.5);
    return(etr::cpp2R(b));
} else if (type_test == 19.53) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), true, b + c);
    return(etr::cpp2R(b));
} else if (type_test == 19.54) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1), etr::i2d(2), etr::i2d(3));
    c = etr::coca(etr::i2d(1), etr::i2d(1), etr::i2d(2), etr::i2d(5));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), true, b == c);
    return(etr::cpp2R(b));
} else if (type_test == 19.55) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b + c, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.56) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b == c, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.57) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), true, b + c);
    return(etr::cpp2R(b));
} else if (type_test == 19.58) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1), etr::i2d(2), etr::i2d(3));
    c = etr::coca(etr::i2d(1), etr::i2d(1), etr::i2d(2), etr::i2d(5));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), true, b == c);
    return(etr::cpp2R(b));
} else if (type_test == 19.59) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b + c, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.61) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b == c, true);
    return(etr::cpp2R(b));
} else if (type_test == 19.62) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b + c, b + c);
    return(etr::cpp2R(b));
} else if (type_test == 19.63) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    rs = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), rs, b + c);
    return(etr::cpp2R(b));
} else if (type_test == 19.64) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    cs = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b + c, cs);
    return(etr::cpp2R(b));
} else if (type_test == 19.65) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b + c, b == c);
    return(etr::cpp2R(b));
} else if (type_test == 19.66) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    rs = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), rs, b == c);
    return(etr::cpp2R(b));
} else if (type_test == 19.67) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b == c, b + c);
    return(etr::cpp2R(b));
} else if (type_test == 19.68) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    cs = etr::coca(etr::i2d(1), etr::i2d(2));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b == c, cs);
    return(etr::cpp2R(b));
} else if (type_test == 19.69) {;
    b = etr::coca(etr::i2d(0), etr::i2d(1));
    c = etr::coca(etr::i2d(1), etr::i2d(1));
    b = etr::subset((etr::matrix(etr::i2d(0), etr::i2d(3), etr::i2d(3)) + etr::colon(etr::i2d(1), etr::i2d(9))), b == c, b == c);
    return(etr::cpp2R(b));
} else if (type_test == 20.1) {;
    for (auto&i:  etr::colon(etr::i2d(1), etr::at(etr::dim(a), 1))) {;
        for (auto&j:  etr::colon(etr::i2d(1), etr::at(etr::dim(a), 2))) {;
            etr::subset(a, i, j) = i + j;
        };
    };
    return(etr::cpp2R(a));
};

if (type_test == 21.1) {;
    a = etr::coca(etr::i2d(0), 0.2, 0.4, 0.99);
    b = etr::sinus(a) + etr::asinus(a) + etr::sinush(a) + etr::cosinus(a) + etr::acosinus(a) + etr::cosinush(a) + etr::tangens(a) + etr::atangens(a) + etr::tangensh(a);
    return(etr::cpp2R(b));
} else if (type_test == 22.1) {;
    a = etr::colon(etr::i2d(1), etr::i2d(3));
    etr::subset(a, 1) = NA_REAL;
    etr::subset(a, 2) = R_PosInf;
    etr::subset(a, 3) = -R_PosInf;
    return(etr::cpp2R(a));
} else if (type_test == 22.2) {;
    a = etr::colon(etr::i2d(1), etr::i2d(3));
    etr::subset(a, 1) = NA_REAL;
    etr::subset(a, 2) = R_PosInf;
    etr::subset(a, 3) = -R_PosInf;
    b = etr::is_na(a);
    return(etr::cpp2R(b));
} else if (type_test == 22.3) {;
    a = etr::colon(etr::i2d(1), etr::i2d(3));
    etr::subset(a, 1) = NA_REAL;
    etr::subset(a, 2) = R_PosInf;
    etr::subset(a, 3) = -R_PosInf;
    b = etr::is_infinite(a);
    return(etr::cpp2R(b));
} else if (type_test == 22.4) {;
    a = etr::colon(etr::i2d(1), etr::i2d(3));
    b = etr::is_infinite(a + etr::i2d(1));
    c = etr::is_na(a + etr::i2d(2));
    d = b + c;
    return(etr::cpp2R(d));
} else if (type_test == 23.1) {;
    x = etr::runif_etr(etr::i2d(10), etr::i2d(0), etr::i2d(1));
    return(etr::cpp2R(x));
} else if (type_test == 23.2) {;
    return(etr::cpp2R(etr::dunif_etr(etr::i2d(10), etr::i2d(0), etr::i2d(1), FALSE)));
} else if (type_test == 23.3) {;
    return(etr::cpp2R(etr::punif_etr(etr::i2d(10), etr::i2d(0), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 23.4) {;
    return(etr::cpp2R(etr::qunif_etr(etr::i2d(10), etr::i2d(0), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 23.5) {;
    return(etr::cpp2R(etr::runif_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(0), etr::i2d(1))));
} else if (type_test == 23.6) {;
    return(etr::cpp2R(etr::dunif_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(0), etr::i2d(1), FALSE)));
} else if (type_test == 23.7) {;
    return(etr::cpp2R(etr::punif_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(0), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 23.8) {;
    return(etr::cpp2R(etr::qunif_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(0), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 23.9) {;
    return(etr::cpp2R(etr::runif_etr(etr::i2d(10), -etr::i2d(1), etr::i2d(2))));
} else if (type_test == 24.1) {;
    x = etr::rnorm_etr(etr::i2d(10), etr::i2d(0), etr::i2d(1));
    return(etr::cpp2R(x));
} else if (type_test == 24.2) {;
    return(etr::cpp2R(etr::dnorm_etr(etr::i2d(10), etr::i2d(0), etr::i2d(1), FALSE)));
} else if (type_test == 24.3) {;
    return(etr::cpp2R(etr::pnorm_etr(etr::i2d(10), etr::i2d(0), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 24.4) {;
    return(etr::cpp2R(etr::qnorm_etr(etr::i2d(10), etr::i2d(0), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 24.5) {;
    return(etr::cpp2R(etr::rnorm_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(0), etr::i2d(1))));
} else if (type_test == 24.6) {;
    return(etr::cpp2R(etr::dnorm_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(0), etr::i2d(1), FALSE)));
} else if (type_test == 24.7) {;
    return(etr::cpp2R(etr::pnorm_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(0), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 24.8) {;
    return(etr::cpp2R(etr::qnorm_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(0), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 24.9) {;
    return(etr::cpp2R(etr::rnorm_etr(etr::i2d(10), -etr::i2d(1), etr::i2d(2))));
} else if (type_test == 25.1) {;
    x = etr::rlnorm_etr(etr::i2d(10), etr::i2d(0), etr::i2d(1));
    return(etr::cpp2R(x));
} else if (type_test == 25.2) {;
    return(etr::cpp2R(etr::dlnorm_etr(etr::i2d(10), etr::i2d(0), etr::i2d(1), FALSE)));
} else if (type_test == 25.3) {;
    return(etr::cpp2R(etr::plnorm_etr(etr::i2d(10), etr::i2d(0), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 25.4) {;
    return(etr::cpp2R(etr::qlnorm_etr(etr::i2d(10), etr::i2d(0), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 25.5) {;
    return(etr::cpp2R(etr::rlnorm_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(0), etr::i2d(1))));
} else if (type_test == 25.6) {;
    return(etr::cpp2R(etr::dlnorm_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(0), etr::i2d(1), FALSE)));
} else if (type_test == 25.7) {;
    return(etr::cpp2R(etr::plnorm_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(0), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 25.8) {;
    return(etr::cpp2R(etr::qlnorm_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(0), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 25.9) {;
    return(etr::cpp2R(etr::rlnorm_etr(etr::i2d(10), -etr::i2d(1), etr::i2d(2))));
} else if (type_test == 26.1) {;
    x = etr::rgamma_etr(etr::i2d(10), etr::i2d(1), etr::i2d(1));
    return(etr::cpp2R(x));
} else if (type_test == 26.2) {;
    return(etr::cpp2R(etr::dgamma_etr(etr::i2d(10), etr::i2d(5), etr::i2d(1), FALSE)));
} else if (type_test == 26.3) {;
    return(etr::cpp2R(etr::pgamma_etr(etr::i2d(10), etr::i2d(10), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 26.4) {;
    return(etr::cpp2R(etr::qgamma_etr(etr::i2d(10), etr::i2d(10), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 26.5) {;
    return(etr::cpp2R(etr::rgamma_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(1), etr::i2d(1))));
} else if (type_test == 26.6) {;
    return(etr::cpp2R(etr::dgamma_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(1), etr::i2d(1), FALSE)));
} else if (type_test == 26.7) {;
    return(etr::cpp2R(etr::pgamma_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(7), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 26.8) {;
    return(etr::cpp2R(etr::qgamma_etr(etr::coca(etr::i2d(1), etr::i2d(2), etr::i2d(3)), etr::i2d(5), etr::i2d(1), TRUE, FALSE)));
} else if (type_test == 26.9) {;
    return(etr::cpp2R(etr::rgamma_etr(etr::i2d(10), etr::i2d(1), -etr::i2d(1))));
};

ret = etr::i2d(1);

return(etr::cpp2R(ret));expect_error(randomStuff2())
}
*/

