SubsetView <- R6::R6Class(
  "SubsetView",
  public = list(
    arr = NULL,
    indices = NULL,
    dim_values = NULL,
    initialize = function(arr, indices, dim) {
      self$arr <- arr
      self$indices <- indices
      self$dim_values <- dim
    },
    print = function() {
      print(array(self$arr[self$indices], self$dim_values))
    },
    dim = function() {
      return(self$dim_values)
    },
    get = function(n, offset = 1L) {
      self$arr[self$indices[offset:(offset + n)]]
    }
  )
)

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
    offset <- calc_offset(args, pos, strides, n_args)
    out[counter] <- offset
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
        return(SubsetView$new(arr, out, lengths))
      }
    }
  }
}

a <- array(1:144, c(3, 4, 4, 3))
res <- subset_own(a, c(1, 2), c(1, 2, 3), c(3, 2), c(1, 2))
res
a[c(1, 2), 1:3, c(3, 2), c(1, 2)]

print_own_matrix <- function(pos, temp, rs, cs) {
  cat(
    paste0(", , ",
      paste0(pos + 1L, collapse = ", ")
    ), "\n\n")
  print(matrix(temp, rs, cs))
  cat("\n")
}

print_own <- function(obj) {
  n_args <- length(obj$dim())
  strides <- integer(n_args)
  strides[1L] <- 1L
  for (i in 2:n_args) {
    strides[i] <- strides[i - 1L] * obj$dim()[i - 1L]
  }
  strides <- strides[-1L:-2L]
  rs <- obj$dim()[[1L]]
  cs <- obj$dim()[[2L]]
  n_rows_cols <- rs * cs

  calc_offset <- function(pos, strides, n_args) {
    offset <- 1L
    for (i in seq_len(n_args - 2L)) {
      offset <- offset + (pos[i]) * strides[i]
    }
    as.numeric(offset)
  }

  pos <- rep(0L, n_args - 2L)
  lengths <- obj$dim()[-1L:-2L]
  repeat {
    offset <- calc_offset(pos, strides, n_args)
    temp <- obj$get(n_rows_cols - 1L, offset)
    print_own_matrix(pos, temp, rs, cs)
    i <- 1L
    repeat {
      pos[i] <- pos[i] + 1L
      if (pos[i] < lengths[i]) break
      pos[i] <- 0L;
      i <- i + 1L
      if (i > (n_args - 2L)) return(invisible(NULL))
    }
  }
}
print_own(res)
a[c(1, 2), 1:3, c(3, 2), c(1, 2)]

print_own_array <- function(arr) {
  n_args <- length(dim(arr))
  strides <- integer(n_args)
  strides[1L] <- 1L
  for (i in 2:n_args) {
    strides[i] <- strides[i - 1L] * dim(arr)[i - 1L]
  }
  strides <- strides[-1L:-2L]
  rs <- dim(arr)[[1L]]
  cs <- dim(arr)[[2L]]
  n_rows_cols <- rs * cs

  calc_offset <- function(rel_dims, pos, strides, n_args) {
    offset <- 1L
    for (i in seq_len(n_args - 2L)) {
      offset <- offset + (pos[i]) * strides[i]
    }
    as.numeric(offset)
  }

  pos <- rep(0L, n_args - 2L)
  lengths <- dim(arr)[-1L:-2L]
  repeat {
    offset <- calc_offset(rel_dims, pos, strides, n_args)
    temp <- arr[offset:(offset + n_rows_cols - 1L)]
    print_own_matrix(pos, temp, rs, cs)
    i <- 1L
    repeat {
      pos[i] <- pos[i] + 1L
      if (pos[i] < lengths[i]) break
      pos[i] <- 0L;
      i <- i + 1L
      if (i > (n_args - 2L)) return(invisible(NULL))
    }
  }
}
b <- array(1:24, c(2, 3, 4))
print_own_array(b)
b
