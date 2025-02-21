#!/bin/bash

R CMD build /home/konrad/Documents/GitHub/RProjects/ast2ast_supplement/ast2ast
mv ./ast2ast_*.tar.gz /home/konrad/Documents/GitHub/RProjects/ast2ast_supplement/ast2ast/.development
R CMD check ./ast2ast_*.tar.gz --as-cran
