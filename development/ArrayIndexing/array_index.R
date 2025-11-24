subset_own <- function(arr, ...) {
  args <- list(...)
  n_args <- length(args)
  if (length(dim(arr)) != n_args) stop("non conformable subsetting")
  if (n_args == 0) stop("array was subsetted with nothing.")

  lengths <- vapply(args, function(arg) {
    l <- length(arg)
    if (l == 0) stop("Each argument used for subsetting has to contain at least one element")
    l
  }, numeric(1))

  # The stride is a helper which stores the information of how many entries are stored in the respective dimension
  # In the first dimension the stride is 1L. As now dimension is below the first one an increase in the respective
  # dimension only increases later the offset by 1.
  # Afterwards, the strides are the product of the previous dimensions.
  # Thus, the stride of the dimension i defines how many elements are contained by
  # one entry in the respective dimension i.
  strides <- integer(n_args)
  strides[1L] <- 1L
  for (i in 2:n_args) {
    strides[i] <- strides[i - 1L] * dim(arr)[i - 1L]
  }

  size <- 1L
  for (i in seq_len(n_args)) {
    size <- size*lengths[i]
  }
  out <- integer(size)

  # Loop over each dimensions:
  # rows, cols, ....
  # offset + (row[i] - 1)*stride[i] = offset + (row[i] - 1)*1 = offset
  # offset + (col[i] - 1)*stride[i] = offset + (col[i] - 1) * (number_of_rows*1) = offset
  # offset + (args[[i]] - 1)*stride[i] = offset + (args[[i]] - 1) * (number_of_cols*number_of_rows*1)= offset
  # ....
  calc_offset <- function(args, pos, strides, n_args) {
    offset <- 1L
    for (i in seq_len(n_args)) {
      offset <- offset + (args[[i]][pos[i] + 1L] - 1L) * strides[i]
    }
    offset
  }

  posis <- lapply(lengths, function(l) {
    0:(l - 1L)
  }) |> expand.grid()

  counter <- 1L
  pos <- rep(0L, n_args)
  repeat {
    print(pos)
    offset <- calc_offset(args, pos, strides, n_args)
    out[counter] <- arr[[offset]]
    counter <- counter + 1L

    # Update pos --> does the same as the posis calculation above.
    i <- 1L
    repeat {
      pos[i] <- pos[i] + 1L
      # Walk through one dimension until pos[i] == length
      # pos[i] == length number of entries desired in the respective dimensions are done
      if (pos[i] < lengths[i]) break
      # the respective dimension is done. But Afterwards more dimension can follow.
      # Therefore, the dimension[i] has to restart again
      pos[i] <- 0L;
      # Go to next dimension
      i <- i + 1L
      if (i > n_args) {
        return(array(arr[out], lengths))
      }
    }
  }
}

m <- matrix(1:12, 2, 6)
subset_own(m, c(1, 2), c(1, 2, 3, 4))

a <- array(1:60, c(3, 4, 4))
res <- subset_own(a, c(1, 2), c(1, 2, 3), c(3))
res
a[c(1, 2), 1:3, 3]
