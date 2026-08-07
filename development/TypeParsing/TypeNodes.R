# =====================================================================
# Type nodes
# =====================================================================
pre_type_node <- R6::R6Class(
  "pre_type_node",
  public = list(
    name = NULL,
    data_struct = NULL,
    const_or_mut = "mutable",
    copy_or_ref = "copy",
    r_fct = NULL,
    real_type = NULL,
    iterator = NULL,
    type_decl = NULL,
    fct_input = NULL,
    error = NULL,
    initialize = function(iterator, type_decl, fct_input, error) {
      self$iterator <- iterator
      self$type_decl <- type_decl
      self$fct_input <- fct_input
      self$error <- error
    },
    print = function() {
      cat(paste0(self$name, ": "))
      print(self$data_struct)
    },
    stringify = function(indent = "") {
      paste0(indent, self$data_struct$stringify(indent), " ", self$name, ";")
    },
    get_base_type = function() {
      self$data_struct$get_base_type()
    },
    get_data_struct = function() {
      self$data_struct$get_data_struct()
    },
    get_data_struct_verbose = function() {
      self$data_struct$get_data_struct_verbose()
    },
    get_const_or_mut = function() {
      self$const_or_mut
    },
    get_copy_or_ref = function() {
      self$copy_or_ref
    },
    get_iterator = function() {
      self$iterator
    },
    get_type_decl = function() {
      self$type_decl
    },
    get_fct_input = function() {
      self$fct_input
    },
    get_error = function() {
      self$error
    },
    declare = function(indent = "") {
      res <- paste0(self$data_struct$stringify(indent), " ", self$name)
      cast_fct <- self$data_struct$cast_fct()
      if (is.null(cast_fct)) {
        paste0(res, " = ", self$name, "SEXP;")
      } else {
        paste0(res, " = ", cast_fct, "(", self$name, "SEXP);")
      }
    },
    signature = function(indent = "") {
      if (self$r_fct) {
        return(sprintf("SEXP %sSEXP", self$name))
      }
      cm <- "const "
      if (self$const_or_mut != "const") cm <- ""
      cr <- "&"
      if (self$copy_or_ref == "copy") cr <- ""
      paste0(cm, self$data_struct$stringify(indent), cr, " ", self$name)
    }
  )
)

scalar <- R6::R6Class(
  "scalar",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("scalar value of: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      basic <- is_base_type(self$get_base_type())
      if (basic) {
        res <- convert_base_type(self$base_type, self$real_type)
      } else {
        res <- self$base_type
      }
      res
    },
    cast_fct = function() {
      "SEXP2Scalar"
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "scalar"
    },
    get_data_struct_verbose = function() {
      "scalar"
    }
  )
)
vec <- R6::R6Class(
  "vec",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("vector containing: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      bt <- convert_base_type(self$base_type, self$real_type)
      sprintf("etr::Array<%s, etr::Buffer<%s>>", bt, bt)
    },
    cast_fct = function() {
      NULL
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "vector"
    },
    get_data_struct_verbose = function() {
      "vector"
    }
  )
)

mat <- R6::R6Class(
  "mat",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("matrix containing: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      bt <- convert_base_type(self$base_type, self$real_type)
      sprintf("etr::Array<%s, etr::Buffer<%s>>", bt, bt)
    },
    cast_fct = function() {
      NULL
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "matrix"
    },
    get_data_struct_verbose = function() {
      "matrix"
    }
  )
)
arr <- R6::R6Class(
  "arr",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("array containing: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      bt <- convert_base_type(self$base_type, self$real_type)
      sprintf("etr::Array<%s, etr::Buffer<%s>>", bt, bt)
    },
    cast_fct = function() {
      NULL
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "array"
    },
    get_data_struct_verbose = function() {
      "array"
    }
  )
)
borrow_vec <- R6::R6Class(
  "borrow_vec",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("vector (borrowed) containing: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      bt <- convert_base_type(self$base_type, self$real_type)
      sprintf("etr::Array<%s, etr::Borrow<%s>>", bt, bt)
    },
    cast_fct = function() {
      NULL
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "vector"
    },
    get_data_struct_verbose = function() {
      "borrow_vector"
    }
  )
)

borrow_mat <- R6::R6Class(
  "borrow_mat",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("matrix (borrowed) containing: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      bt <- convert_base_type(self$base_type, self$real_type)
      sprintf("etr::Array<%s, etr::Borrow<%s>>", bt, bt)
    },
    cast_fct = function() {
      NULL
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "matrix"
    },
    get_data_struct_verbose = function() {
      "borrow_matrix"
    }
  )
)
borrow_arr <- R6::R6Class(
  "borrow_arr",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("array (borrowed) containing: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      bt <- convert_base_type(self$base_type, self$real_type)
      sprintf("etr::Array<%s, etr::Borrow<%s>>", bt, bt)
    },
    cast_fct = function() {
      NULL
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "array"
    },
    get_data_struct_verbose = function() {
      "borrow_array"
    }
  )
)

new_type_node <- R6::R6Class(
  "new_type_node",
  public = list(
    name = NULL,
    slots = NULL,
    r_fct = NULL,
    real_type = NULL,
    initialize = function(name) {
      self$name <- name
    },
    print = function() {
      cat(self$name, "\n")
      cat("=============================\n")
      for (i in self$slots) {
        print(i)
        cat("------------------------------\n")
      }
    },

    create_constructor_SEXP = function() {
      input <- vapply(seq_len(length(self$slots)), function(i) {
        s <- self$slots[[i]]
        basic <- is_base_type(s$get_base_type())
        if (inherits(s, "pre_type_node")) {
          res <- sprintf("VECTOR_ELT(arg, %s)", i - 1)
          if (s$get_data_struct() == "scalar" && basic) {
            res <- sprintf("SEXP2Scalar(%s)", res)
          }
          res
        } else if (inherits(s, "new_type_node")) {
        } else {
          # Handle error
        }
      }, character(1L))
      names <- vapply(self$slots, function(s) s$name, character(1L))
      input <- paste0(names, "(", input, ")", collapse = ", ")
      sprintf("\texplicit %s(SEXP arg) :\n\t %s {}\n", self$name, input)
    },
    create_special_member_functions = function() {
      n <- self$name
      paste0(
        "\t", n, "(const ", n, "&) = default;\n",
        "\t", n, "(", n, "&&) noexcept = default;\n",
        "\t", n, "& operator=(const ", n, "&) = default;\n",
        "\t", n, "& operator=(", n, "&&) noexcept = default;\n"
      )
    },
    define_type = function() {
      ctr <- self$create_constructor_SEXP()
      special <- self$create_special_member_functions()
      ts <- lapply(self$slots, function(s) {
        s$stringify("\t")
      })
      paste0(
        "struct ", self$name, "{\n",
        paste0(ts, collapse = "\n"),
        "\n",
        ctr,
        "\n",
        special,
        "\n",
        "};"
      )
    },
    get_base_type = function() {
      sprintf("Class %s does not possess a base type", self$name)
    },
    get_data_struct = function() {
      self$name
    },
    get_data_struct_verbose = function() {
      self$name
    },
    get_const_or_mut = function() {
      "mutable"
    },
    get_copy_or_ref = function() {
      "copy" # Cannot be passed by reference
    }
  )
)

collection <- R6::R6Class(
  "collection",
  public = list(
    type = NULL,
    print = function() {
      cat(sprintf("collection containing: %s", self$type), "\n")
    },
    stringify = function(indent = "") {
      basic <- is_base_type(self$type)
      if (basic) {
        stop("collections cannot contain basic types")
      }
      sprintf("etr::Collection<%s>", self$type)
    },
    cast_fct = function() {
      NULL
    },
    get_base_type = function() {
      "Class collection does not possess a base type"
    },
    get_data_struct = function() {
      "collection"
    },
    get_data_struct_verbose = function() {
      "collection"
    }
  )
)
