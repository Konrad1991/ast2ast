#!/bin/bash

R CMD build ~/Documents/ast2ast
mv ./ast2ast_*.tar.gz ~/Documents/ast2ast/development/
R CMD check ./ast2ast_*.tar.gz --as-cran
