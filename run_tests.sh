#!/bin/bash

# Run the R code and capture output
output=$(Rscript -e "tinytest::test_package('ast2ast')" 2>&1)

# Check for the keyword indicating test failures
if echo "$output" | grep -q "FAIL"; then
  echo "Test Summary: Some tests failed. See details below:"
  echo "$output"
else
  echo "Test Summary: All tests passed successfully!"
fi
