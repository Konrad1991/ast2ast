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
  if (type == "NA") {
    return("etr::NA")
  } else if (type == "NaN") {
    return("etr::NaN")
  } else if (type == "Inf") {
    return("etr::Inf")
  }
  if (type == "logical") {
    obj <- tolower(obj)
    if (obj == "t") {
      obj <- "true"
    } else if (obj == "f") {
      obj <- "false"
    }
  }

  if (type == "numeric") {
    if (!grepl("\\.", obj)) {
      obj <- paste0(obj, ".0")
    }
    return(paste0(indent, "Double(", obj, ")"))
  } else if (type == "scientific") {
    return(paste0(indent, "Double(", obj, ")"))
  } else if (type == "integer") {
    return(paste0( indent, "Integer(", gsub("L", "", obj), ")"))
  } else if (type == "logical") {
    return(paste0(indent, "Logical(", obj, ")"))
  } else {
    return(paste0(indent, obj))
  }
}
