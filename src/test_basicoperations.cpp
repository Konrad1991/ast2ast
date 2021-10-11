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


context("Basic operations plus") {
  vec a = range(1, 3);
  vec b = range(1, 3);
  double num = 1.;

  vec c = range(1, 3);
  c = a + num;
  std::vector<double> result{2., 3., 4.};

  test_that("Vector + scalar") {
    for(int i = 0; i < result.size(); i++) {
        expect_true(result[i] == c[i]);
    }
  }

  vec d = range(1, 3);
  vec e(3, 0.);
  e = num + d;

  test_that("Scalar + vector") {
    for(int i = 0; i < result.size(); i++) {
        expect_true(result[i] == e[i]);
    }
  }

  vec f = range(1, 3);
  vec g = range(1, 3);
  g = f + g;
  std::vector<double> result2{2., 4., 6.};

  test_that("Vector + vector") {
    for(int i = 0; i < result2.size(); i++) {
        expect_true(result2[i] == g[i]);
    }
  }


  vec h = range(1, 4);
  mat j(2, 2, 1.);
  h = h + j;

  std::vector<double> result3{2., 3., 4., 5.};

  test_that("Vector + matrix") {
    for(int i = 0; i < result3.size(); i++) {
        expect_true(result3[i] == h[i]);
    }
  }



  vec k = range(1, 4);
  mat m(2, 2, 1.);
  m = m + k;

  std::vector<double> result4{2., 3., 4., 5.};

  test_that("Matrix + vector") {
    for(int i = 0; i < result4.size(); i++) {
        expect_true(result4[i] == m[i]);
    }
  }


  vec n = range(1, 4);
  vec o = range(1, 8);
  n = subset(o, 1, 4) + num;
  std::vector<double> result5{2., 3., 4., 5.};

  test_that("Subset + scalar") {
    for(int i = 0; i < result5.size(); i++) {
        expect_true(result5[i] == n[i]);
    }
  }

  n = num + subset(o, 1, 4);
  test_that("Scalar + subset") {
    for(int i = 0; i < result5.size(); i++) {
        expect_true(result5[i] == n[i]);
    }
  }


  n = subset(o, 1, 4) + n;
  std::vector<double> result6{3., 5., 7., 9.};

  test_that("Subset + vector") {
    for(int i = 0; i < result6.size(); i++) {
        expect_true(result6[i] == n[i]);
    }
  }


}
