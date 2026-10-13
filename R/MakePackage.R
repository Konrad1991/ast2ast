make_package <- function(files, entry_point, package_dir = ".", overwrite = FALSE) {
  stopifnot("files is not of type character" = is.character(files))
  stopifnot("entry_point is not a single string" = is.character(entry_point) && length(entry_point) == 1L)
  stopifnot("package_dir is not a single string" = is.character(package_dir) && length(package_dir) == 1L)
  stopifnot("overwrite is not of type logical" = is.logical(overwrite) && length(overwrite) == 1L)

  files <- check_paths(files, package_dir)
  name <- package_name(package_dir)
  check_package_setup(package_dir, name, entry_point)
  code <- unlist(lapply(files, parse_with_srcrefs, name), recursive = FALSE)

  check_top_level_code(code)
  types_f <- pre_scan_for_types(code)
  definitions <- collect_functions(code)
  if (!entry_point %in% names(definitions)) {
    stop(sprintf("Entry point %s is not defined in the files", entry_point))
  }
  combined <- combine_functions(definitions, entry_point)
  cpp <- translate_internally(combined, types_f, NULL, entry_point, TRUE)
  # NOTE: sourceCpp-only attributes; in a package the Makevars does this
  cpp <- strsplit(cpp, "\n", fixed = TRUE)[[1]]
  cpp <- cpp[!grepl("^// \\[\\[Rcpp::(depends|plugins)", cpp)]

  cpp_file <- file.path(package_dir, "src", paste0("ast2ast_", entry_point, ".cpp"))
  write_generated(cpp_file, cpp, "//", overwrite)
  copy_headers(package_dir)
  write_makevars(package_dir, overwrite)
  Rcpp::compileAttributes(package_dir)
  invisible(name)
}

# NOTE: the srcfile is named "<pkg>: <file>" so the runtime labels read "pkg: file.R:12 (fn)"
parse_with_srcrefs <- function(file, package_name) {
  lines <- readLines(file, warn = FALSE)
  srcfile <- srcfilecopy(sprintf("%s: %s", package_name, basename(file)), lines)
  parsed <- parse(text = lines, keep.source = TRUE, srcfile = srcfile)
  Map(function(expr, ref) list(expr = expr, srcref = ref),
      as.list(parsed), attr(parsed, "srcref"))
}

check_paths <- function(files, package_dir) {
  if (!file.exists(file.path(package_dir, "DESCRIPTION"))) {
    stop(sprintf("%s is not a package: DESCRIPTION is missing", package_dir))
  }
  paths <- file.path(package_dir, files)
  missing <- paths[!file.exists(paths)]
  if (length(missing) != 0L) {
    stop(sprintf("The following files do not exist:\n%s", paste(missing, collapse = "\n")))
  }
  package_path <- normalizePath(package_dir)
  normalized <- normalizePath(paths)
  outside <- paths[!startsWith(normalized, file.path(package_path, ""))]
  if (length(outside) != 0L) {
    stop(sprintf("The following files are not inside %s:\n%s", package_dir, paste(outside, collapse = "\n")))
  }
  in_r <- paths[startsWith(normalized, file.path(package_path, "R", ""))]
  if (length(in_r) != 0L) {
    stop(sprintf(
      "Files in R/ are sourced when the package is installed; move them e.g. to inst/tools/:\n%s",
      paste(in_r, collapse = "\n")
    ))
  }
  paths
}

package_name <- function(package_dir) {
  name <- unname(read.dcf(file.path(package_dir, "DESCRIPTION"), fields = "Package")[1, 1])
  if (is.na(name)) {
    stop("DESCRIPTION has no Package field")
  }
  name
}

check_package_setup <- function(package_dir, name, entry_point) {
  description <- read.dcf(file.path(package_dir, "DESCRIPTION"))[1, ]
  field_has <- function(field, package) {
    field %in% names(description) &&
      grepl(sprintf("\\b%s\\b", package), description[[field]])
  }
  missing <- c(
    if (!field_has("LinkingTo", "Rcpp")) "DESCRIPTION: LinkingTo: Rcpp",
    if (!field_has("LinkingTo", "RcppThread")) "DESCRIPTION: LinkingTo: RcppThread",
    if (!field_has("Imports", "Rcpp")) "DESCRIPTION: Imports: Rcpp"
  )
  namespace_file <- file.path(package_dir, "NAMESPACE")
  namespace <- if (file.exists(namespace_file)) readLines(namespace_file, warn = FALSE) else character()
  if (!any(grepl(sprintf("useDynLib\\(\\s*%s\\s*,.*\\.registration\\s*=\\s*TRUE", name), namespace))) {
    missing <- c(missing, sprintf("NAMESPACE: useDynLib(%s, .registration = TRUE)", name))
  }
  if (!any(grepl("importFrom\\(\\s*Rcpp", namespace))) {
    missing <- c(missing, "NAMESPACE: importFrom(Rcpp, evalCpp)")
  }
  if (!any(grepl(sprintf("^\\s*export\\(.*\\b%s\\b", entry_point), namespace)) &&
      !any(grepl("^\\s*exportPattern\\(", namespace))) {
    missing <- c(missing, sprintf("NAMESPACE: export(%s)", entry_point))
  }
  if (length(missing) != 0L) {
    stop(sprintf("The package is not set up for make_package. Please add:\n%s",
                 paste(missing, collapse = "\n")))
  }
}

is_type_definition <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(new_type))
}

is_function_definition <- function(expr) {
  is.call(expr) &&
    (identical(expr[[1]], quote(`<-`)) || identical(expr[[1]], quote(`=`))) &&
    is.name(expr[[2]]) &&
    is.call(expr[[3]]) && identical(expr[[3]][[1]], quote(`function`))
}

is_call_to <- function(expr, name) {
  is.call(expr) && identical(expr[[1]], as.name(name))
}

check_top_level_code <- function(code) {
  unsupported <- Filter(function(x) {
    !is_type_definition(x$expr) && !is_function_definition(x$expr)
  }, code)
  if (length(unsupported) != 0L) {
    unsupported <- vapply(unsupported, function(x) {
      paste0(srcref_location(x$srcref), ": ", paste(deparse(x$expr), collapse = "\n"))
    }, character(1L))
    stop(
      sprintf(
        "Found the following not supported expressions:\n%s",
        paste(unsupported, collapse = "\n")
      )
    )
  }
}

duplicate_type_definitions <- function(types) {
  type_names <- vapply(types, function(t) {
    deparse(t$expr[[2]])
  }, character(1L))
  if (anyDuplicated(type_names) != 0) {
    duplicate <- anyDuplicated(type_names)
    stop(sprintf("%s: Type %s is redefined", srcref_location(types[[duplicate]]$srcref), type_names[duplicate]))
  }
}

pre_scan_for_types <- function(code) {
  types <- Filter(function(x) is_type_definition(x$expr), code)
  if (length(types) == 0L) {
    return(NULL)
  }
  duplicate_type_definitions(types)
  types_f <- function() {}
  body(types_f) <- as.call(c(as.name("{"), lapply(types, `[[`, "expr")))
  types_f
}

collect_functions <- function(code) {
  definitions <- Filter(function(x) is_function_definition(x$expr), code)
  function_names <- vapply(definitions, function(d) {
    deparse(d$expr[[2]])
  }, character(1L))
  if (anyDuplicated(function_names) != 0) {
    duplicate <- anyDuplicated(function_names)
    stop(sprintf("%s: Function %s is redefined", srcref_location(definitions[[duplicate]]$srcref), function_names[duplicate]))
  }
  names(definitions) <- function_names
  definitions
}

# NOTE: all other top-level fns become inner fns of the entry point,
# so translate_internally turns them into functors without new codegen
combine_functions <- function(definitions, entry_point) {
  entry <- definitions[[entry_point]]
  entry_body <- entry$expr[[3]][[3]]
  statements <- if (is_call_to(entry_body, "{")) as.list(entry_body)[-1] else list()
  if (length(statements) < 2L ||
      !is_call_to(statements[[1]], "argtypes") ||
      !is_call_to(statements[[2]], "returntype")) {
    stop(sprintf("%s: The entry point %s has to start with argtypes(...) and returntype(...)",
                 srcref_location(entry$srcref), entry_point))
  }
  helper_definitions <- definitions[names(definitions) != entry_point]
  helpers <- lapply(helper_definitions, `[[`, "expr")
  body <- as.call(c(as.name("{"), statements[1:2], helpers, statements[-(1:2)]))
  entry_refs <- attr(entry_body, "srcref")
  attr(body, "srcref") <- c(entry_refs[1:3], lapply(helper_definitions, `[[`, "srcref"), entry_refs[-(1:3)])
  eval(call("function", entry$expr[[3]][[2]], body), envir = globalenv())
}

generated_marker <- "Generated by ast2ast::make_package -- do not edit"

is_generated <- function(path) {
  first_line <- readLines(path, n = 1L, warn = FALSE)
  length(first_line) == 1L && grepl(generated_marker, first_line, fixed = TRUE)
}

check_overwrite <- function(path, overwrite) {
  if (!file.exists(path) || is_generated(path) || overwrite) {
    return()
  }
  if (!interactive()) {
    stop(sprintf("%s was not generated by make_package; use overwrite = TRUE to replace it", path))
  }
  if (!isTRUE(utils::askYesNo(sprintf("%s was not generated by make_package. Overwrite it?", path), default = FALSE))) {
    stop(sprintf("Aborted: %s was not overwritten", path))
  }
}

write_generated <- function(path, content, comment, overwrite) {
  check_overwrite(path, overwrite)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(paste(comment, generated_marker), content), path)
}

# NOTE: own subfolder so a user's inst/include is never touched
copy_headers <- function(package_dir) {
  target <- file.path(package_dir, "inst", "include", "ast2ast")
  unlink(target, recursive = TRUE)
  dir.create(target, recursive = TRUE)
  source <- system.file("include", package = "ast2ast")
  file.copy(list.files(source, full.names = TRUE), target, recursive = TRUE)
  writeLines(c(
    "The headers in this folder are copied from the R package ast2ast",
    "(https://github.com/Konrad1991/ast2ast) and are licensed under GPL (>= 2)."
  ), file.path(target, "LICENSE_NOTE"))
}

write_makevars <- function(package_dir, overwrite) {
  content <- c(
    "CXX_STD = CXX20",
    "PKG_CPPFLAGS = -I../inst/include/ast2ast",
    "PKG_LIBS = $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)"
  )
  for (file in c("Makevars", "Makevars.win")) {
    write_generated(file.path(package_dir, "src", file), content, "#", overwrite)
  }
}
