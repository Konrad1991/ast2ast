/*
R package ast2ast
Copyright (C) 2021 Konrad Krämer

This file is part of R package ast2ast


ast2ast is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with pso
If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4
*/

#include <testthat.h>


#include "all.hpp"


vec add_vec_sca() {
  vec a(range(1., 5.));
  a = a + 10.;
  return a;
}


context("Vector Scalar basic calculations") {
  vec result(range(11., 15.));

  test_that("Vector + scalar") {
    for(int i = 0; i < result.size(); i++) {
        expect_true(add_vec_sca()[i] == result[i]);
    }
  }
}


vec add_vec_vec() {
  vec a(range(1., 5.));
  a = a + a;
  return a;
}
context("Vector Vector basic calculations") {
  std::vector<double> res{2., 4., 6., 8., 10.};
  vec result(res);

  test_that("Vector + vector") {
    for(int i = 0; i < result.size(); i++) {
        expect_true(add_vec_vec()[i] == result[i]);
    }
  }
}


context("larger vector + shorter vector basic calculations") {
  vec larger(range(1., 6.));
  vec shorter(range(1., 3.));
  vec result(5);
  result = larger + shorter;
  std::vector<double> correct_result{2, 4, 6, 5, 7, 9};

  test_that("Vector + vector") {
    for(int i = 0; i < result.size(); i++) {
        expect_true(result[i] == correct_result[i]);
    }
  }
}




context("shorter vector + larger vector basic calculations") {
  vec larger(range(1., 6.));
  vec shorter(range(1., 3.));
  vec result(6);
  result = shorter + larger;
  std::vector<double> correct_result{2, 4, 6, 5, 7, 9};

  test_that("Vector + vector") {
    for(int i = 0; i < result.size(); i++) {
        expect_true(result[i] == correct_result[i]);
    }
  }
}


context("incompatible vectors") {
  vec a(range(1., 6.));
  vec b(range(1., 5.));
  test_that("incompatible vectors") {
        expect_error(a + b);
        expect_error(a -b);
        expect_error(a/b);
        expect_error(a*b);
  }
}



context("vector - vector") {
  vec a(range(1., 6.));
  vec b(range(0., 5.));
  vec result(6);
  result = a - b;
  vec correct(6, 1.);

  test_that("Vector - vector") {
    for(int i = 0; i < result.size(); i++) {
        expect_true(result[i] == correct[i]);
    }
  }
}



context("vector subset") {
  vec a(range(1., 6.));
  vec b(range(0., 5.));
  vec result(8, 0.);
  result.subset(0, 2) = subset(a, 0, 2) + subset(b, 2, 4);
  std::vector<double> v{3, 5, 7};
  vec correct(v);

  test_that("Vector subset") {
    for(int i = 0; i < correct.size(); i++) {
        expect_true(result[i] == correct[i]);
    }
  }
}



context("subset as R User index start = 1") {
  vec a(range(1., 6.));
  vec b(range(0., 5.));
  vec result(8, 0.);
  result.ui_subset(1, 3) = ui_subset(a, 1, 3) + ui_subset(b, 3, 5);
  std::vector<double> v{3, 5, 7};
  vec correct(v);

  test_that("Vector subset user interface") {
    for(int i = 0; i < correct.size(); i++) {
        expect_true(result[i] == correct[i]);
    }
  }
}



context("access element as R User index start = 1") {
  vec a(range(1, 10));
  vec b(range(6, 15));
  std::vector<double> correct{7,  9, 11, 13, 15, 17, 19, 21, 23, 25};

  test_that("access element R user interface") {
    for(int i = 1; i <= a.size(); i++) {
        expect_true(a.ui_g(i) + b.ui_g(i) == correct[i -1] );
    }
  }
}
