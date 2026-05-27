format_type <- function(node) {
  if (!inherits(node, "type_node")) return("")
  how <- if (node$copy_or_ref == "copy") "" else " by reference"
  c <- if (node$const_or_mut == "const") "const " else ""
  paste0(c, node$data_struct, " of ", node$base_type, how)
}
increment <- function(indent) {
  paste0(indent, "  ")
}

print_ast_internally <- function(node, indent, variable_type_list) {
  if (inherits(node, "variable_node")) {
    cat(indent, node$name, ": ", format_type(variable_type_list[[node$name]]), "\n")
  }
  else if (inherits(node, "type_node")) {
    cat(indent, format_type(node), "\n")
  }
  else if (inherits(node, "binary_node")) {
    cat(indent, node$operator, format_type(node$internal_type), "\n")
    print_ast_internally(node$left_node, increment(indent), variable_type_list)
    print_ast_internally(node$right_node, increment(indent), variable_type_list)
  }
  else if (inherits(node, "unary_node")) {
    cat(indent, node$operator, format_type(node$internal_type), "\n")
    print_ast_internally(node$obj, increment(indent), variable_type_list)
  }
  else if (inherits(node, "if_node")) {
    cat(indent, "if ", format_type(node$condition$internal_type), "\n")
    print_ast_internally(node$condition, increment(indent), variable_type_list)
    print_ast_internally(node$true_node, increment(indent), variable_type_list)
    if(!is.null(node$else_if_nodes)) {
      lapply(node$else_if_nodes, function(arg) { print_ast_internally(arg, increment(indent), variable_type_list) })
    }
    if (!is.null(node$false_node)) {
      print_ast_internally(node$false_node, increment(indent), variable_type_list)
    }
  }
  else if (inherits(node, "block_node")) {
    lapply(node$block, function(stmt) print_ast_internally(stmt, increment(indent), variable_type_list))
  }
  else if (inherits(node, "nullary_node")) {
    cat(indent, node$operator, format_type(node$internal_type), "\n")
  }
  else if (inherits(node, "for_node")) {
    cat(indent, "for\n")
    print_ast_internally(node$i, increment(indent), variable_type_list)
    print_ast_internally(node$seq, increment(indent), variable_type_list)
    print_ast_internally(node$block, increment(indent), variable_type_list)
  }
  else if (inherits(node, "while_node")) {
    print_ast_internally(node$condition, increment(indent), variable_type_list)
    print_ast_internally(node$block, increment(indent), variable_type_list)
  }
  else if (inherits(node, "repeat_node")) {
    print_ast_internally(node$block, increment(indent), variable_type_list)
  }
  else if (inherits(node, "function_node")) {
    cat(indent, node$operator, format_type(node$internal_type), "\n")
    lapply(node$args, function(arg) {
      print_ast_internally(arg, increment(indent), variable_type_list)
    })
  }
  else if (inherits(node, "fn_node")) {
  }
  else if (inherits(node, "literal_node")) {
    cat(indent, format_type(node$internal_type), "\n")
  } else {
    stop("Unknown node type: ", class(node))
  }
}

print_ast <- function(f, args_f, function_registry, real_type, r_fct) {
  ast <- parse_body(body(f), r_fct, function_registry)
  update_function_registry(ast, function_registry)
  vars_types_list <- infer_types(ast, f, args_f, r_fct, function_registry)
  traverse_ast(ast, action_transpile_inner_functions, real_type)
  print_ast_internally(ast, "", vars_types_list)
  invisible(NULL)
}
