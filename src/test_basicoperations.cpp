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

  n = range(1, 4);
  n = n + subset(o, 1, 4);
  std::vector<double> result6_1{2., 4., 6., 8.};
  test_that("Vector + subset") {
    for(int i = 0; i < result6_1.size(); i++) {
        expect_true(result6_1[i] == n[i]);
    }
  }



  mat m1(2, 2, 3.);
  mat m2(2, 2, 4.);

  mat m3;
  m3 = m1 + m2;
  std::vector<double> result7{7., 7., 7., 7.};

  test_that("Matrix + matrix") {
    for(int i = 0; i < result7.size(); i++) {
        expect_true(result7[i] == m3[i]);
    }
  }




}
















context("Concatenation") {

  vec v1 = range(1, 3);
  vec v2 = range(4, 6);
  vec v3 = concatenate(v1, v2);
  std::vector<double> result1{1.,2.,3., 4., 5., 6.};

  test_that("vec vec") {
    for(int i = 0; i < result1.size(); i++) {
        expect_true(result1[i] == v3[i]);
    }
  }

  vec v4 = concatenate(v1, 4.0);
  std::vector<double> result2{1.,2.,3., 4.};

  test_that("vec double") {
    for(int i = 0; i < result2.size(); i++) {
        expect_true(result2[i] == v4[i]);
    }
  }

  vec v5 = concatenate(4.0, v1);
  std::vector<double> result3{4.0 ,1.,2.,3.};

  test_that("double vec") {
    for(int i = 0; i < result3.size(); i++) {
        expect_true(result3[i] == v5[i]);
    }
  }

  mat m1(2, 2, 500.);
  vec v6 = concatenate(v1, m1);
  std::vector<double> result4{1.,2.,3., 500., 500., 500., 500.};

  test_that("vec mat") {
    for(int i = 0; i < result4.size(); i++) {
        expect_true(result4[i] == v6[i]);
    }
  }

  mat m2(2, 2, 500.);
  vec v7 = concatenate(m2, v1);
  std::vector<double> result5{500., 500., 500., 500.,1.,2.,3.};

  test_that("mat vec") {
    for(int i = 0; i < result5.size(); i++) {
        expect_true(result5[i] == v7[i]);
    }
  }



  vec v8 = range(1, 10);
  vec v9 = concatenate(v1, subset(v8, 1, 2));
  std::vector<double> result6{1.,2.,3., 1., 2.};

  test_that("vec sub") {
    for(int i = 0; i < result6.size(); i++) {
        expect_true(result6[i] == v9[i]);
    }
  }


  v9 = concatenate(subset(v8, 1, 2), v1);
  std::vector<double> result7{1., 2., 1.,2.,3.};

  test_that("vec sub") {
    for(int i = 0; i < result7.size(); i++) {
        expect_true(result7[i] == v9[i]);
    }
  }

  vec v10 = concatenate(m1, m2);
  std::vector<double> result8{500., 500., 500., 500., 500., 500., 500., 500.};

  test_that("mat mat") {
    for(int i = 0; i < result8.size(); i++) {
        expect_true(result8[i] == v10[i]);
    }
  }


  vec v11 = concatenate(m1, 200.);
  std::vector<double> result9{500., 500., 500., 500.,200.};

  test_that("mat double") {
    for(int i = 0; i < result9.size(); i++) {
        expect_true(result9[i] == v11[i]);
    }
  }


  vec v12 = concatenate(200., m1);
  std::vector<double> result10{200., 500., 500., 500., 500.};

  test_that("double mat") {
    for(int i = 0; i < result10.size(); i++) {
        expect_true(result10[i] == v12[i]);
    }
  }



  vec v13 = concatenate(m1, subset(v8, 1, 2));
  std::vector<double> result11{ 500., 500., 500., 500., 1., 2.};

  test_that("mat subset") {
    for(int i = 0; i < result11.size(); i++) {
        expect_true(result11[i] == v13[i]);
    }
  }


  vec v14 = concatenate(subset(v8, 1, 2), m1);
  std::vector<double> result12{1., 2., 500., 500., 500., 500.};

  test_that("subset mat") {
    for(int i = 0; i < result12.size(); i++) {
        expect_true(result12[i] == v14[i]);
    }
  }



  vec v15 = concatenate(subset(v8, 1, 2), subset(v1, 1, 2));
  std::vector<double> result13{1., 2., 1., 2.};

  test_that("subset subset") {
    for(int i = 0; i < result13.size(); i++) {
        expect_true(result13[i] == v15[i]);
    }
  }



  vec v16 = concatenate(100., subset(v8, 1, 2));
  std::vector<double> result14{100., 1., 2.};

  test_that("double subset") {
    for(int i = 0; i < result14.size(); i++) {
        expect_true(result14[i] == v16[i]);
    }
  }


  vec v17 = concatenate(subset(v8, 1, 2), 100.);
  std::vector<double> result15{1., 2., 100.};

  test_that("subset double") {
    for(int i = 0; i < result15.size(); i++) {
        expect_true(result15[i] == v17[i]);
    }
  }



  vec v18 = concatenate(101., 100.);
  std::vector<double> result16{101., 100.};

  test_that("subset double") {
    for(int i = 0; i < result16.size(); i++) {
        expect_true(result16[i] == v18[i]);
    }
  }

}




context("Subsetting") {

  vec v1 = concatenate(1., 2., 3., 4., 5., 6.);
  vec v2 = range(1, 3);
  vec v3 = subset(v1, v2);
  std::vector<double> result1{1., 2., 3.};

  test_that("vec with vec") {
    for(int i = 0; i < result1.size(); i++) {
        expect_true(result1[i] == v1[i]);
    }
  }

  v3 = subset(v1, subset(v2, 1));
  std::vector<double> result2{1.};
  test_that("vec with subset & double") {
        expect_true(result2[0] == v3[1]);
        expect_true(result2[0] == subset(v2, 1));
  }


}
