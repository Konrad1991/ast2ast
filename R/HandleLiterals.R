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

t_literal <- function(context, obj, indent, type, wrap, real_type) {
  if (type == "NA") {
    if (!wrap) return("NA") else return(paste0(real_type, "::NA()"))
  } else if (type == "NaN") {
    if (!wrap) return ("NaN") else return(paste0(real_type, "::NaN()"))
  } else if (type == "Inf") {
    if (!wrap) return("Inf") else return(paste0(real_type, "::Inf()"))
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
    if (!wrap) return(obj)
    return(paste0(indent, real_type, "(", obj, ")"))
  } else if (type == "scientific") {
    if (!wrap) return(obj)
    return(paste0(indent, real_type, "(", obj, ")"))
  } else if (type == "integer") {
    if (!wrap) return(obj)
    return(paste0( indent, "etr::Integer(", gsub("L", "", obj), ")"))
  } else if (type == "logical") {
    if (!wrap) return(obj)
    return(paste0(indent, "etr::Logical(", obj, ")"))
  } else {
    return(paste0(indent, obj))
  }
}
