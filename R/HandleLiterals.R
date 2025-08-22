numeric_char <- function(obj) {
  !is.na(suppressWarnings(as.numeric(obj)))
}

scientific_notated <- function(obj) {
  grepl("E", obj) || grepl("e", obj)
}

integer_char <- function(obj) {
  obj <- gsub("\\L", "", obj)
  !is.na(suppressWarnings(as.integer(obj)))
}

logical_char <- function(obj) {
  !is.na(suppressWarnings(as.logical(obj)))
}

determine_literal_type <- function(obj) {
  if (numeric_char(obj)) {
    if (scientific_notated(obj)) {
      return("scientific")
    }
    return("numeric")
  } else if (integer_char(obj)) {
    return("integer")
  } else if (logical_char(obj)) {
    return("logical")
  } else {
    return("character")
  }
}

t_literal <- function(context, obj, indent) {
  dont_change_number <- c("print", ":", "[", "at")
  type <- determine_literal_type(obj)
  if (type == "logical") {
    obj <- tolower(obj)
    if (obj == "t") {
      obj <- "true"
    } else if (obj == "f") {
      obj <- "false"
    }
  }
  if (context %in% dont_change_number) {
    return(paste0(indent, obj))
  }

  if (type == "numeric") {
    if (!grepl("\\.", obj)) {
      return(paste0(obj, ".0"))
    }
    return(paste0(indent, obj))
  } else if (type == "scientific") {
    return(paste0(indent, obj))
  } else if (type == "integer") {
    return(
      paste0(
        indent, gsub("L", "", obj)
      )
    )
  } else if (type == "logical") {
    return(paste0(indent, obj))
  } else {
    return(paste0(indent, obj))
  }
}
