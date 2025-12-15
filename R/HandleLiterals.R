scientific_notated <- function(obj) {
  grepl("E", obj) || grepl("e", obj)
}

determine_literal_type <- function(obj) {
  if (is.infinite(obj)) {
    return("Inf")
  }
  if (is.na(obj) && !is.nan(obj)) {
    return("NA")
  }
  if (is.nan(obj)) {
    return("NaN")
  }
  if (is.logical(obj)) {
    return("logical")
  }
  if (is.integer(obj)) {
    return("integer")
  }
  if (is.double(obj)) {
    if (scientific_notated(deparse(obj))) {
      return("scientific")
    }
    return("numeric")
  }
  return("character")
}

t_literal <- function(context, obj, indent, type) {
  print(type)
  if (type == "NA") {
    return("etr::NA")
  } else if (type == "NaN") {
    return("etr::NaN")
  } else if (type == "Inf") {
    return("etr::Inf")
  }
  dont_change_number <- c("print", ":", "[", "[[", "at", "vector", "logical", "integer", "numeric")
  # Matrix is difficult. As only the 2. and 3. argument shouldnt be changed.
  # Not easy to do with the current design.
  if (type == "logical") {
    obj <- tolower(obj)
    if (obj == "t") {
      obj <- "true"
    } else if (obj == "f") {
      obj <- "false"
    }
  }
  if (context %within% dont_change_number) {
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
