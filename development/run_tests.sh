#!/bin/bash

# Rscript -e "Rcpp::compileAttributes('..')"
# find ../src/ -name "*.o" -delete

Rscript -e '
arg <- c(7, 8, 9)
ast2ast:::test_arithmetic()
ast2ast:::test_arithmetic_unary()
ast2ast:::test_at_subsetting()
ast2ast:::test_basestore()
ast2ast:::test_bool()
ast2ast:::test_borrow()
ast2ast:::test_borrow_r(arg)
ast2ast:::test_c()
ast2ast:::test_cmr()
ast2ast:::test_colon()
ast2ast:::test_comparison()
ast2ast:::test_concepts()
ast2ast:::test_dim()
ast2ast:::test_isFinite()
ast2ast:::test_int()
ast2ast:::test_isInfinite()
ast2ast:::test_isNA()
ast2ast:::test_length()
ast2ast:::test_matrices()
ast2ast:::test_matrix_subsetting()
ast2ast:::test_numeric()
ast2ast:::test_rep()
ast2ast:::test_subset_with_scalar()
ast2ast:::test_vector()
ast2ast:::test_vector_subsetting()
'
