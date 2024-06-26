setClass(
  "String",
  representation(value = "character"),
  prototype(value = character())
)

setMethod("initialize", signature(.Object = "String"), function(.Object, value) {
  .Object@value <- value
  .Object
})

cString <- function(...) {
  l <- list(...)
  delim <- tail(l, n = 1)[[1]]
  values <- sapply(l[1:(length(l) - 1)], function(x) {
    if (is.character(x)) {
      return(x)
    } else if (is(x, "String")) {
      return(x@value)
    } else {
      stop("Error can only handle class String and character")
    }
  })
  new_value <- paste0(values, collapse = delim)
  new("String", value = new_value)
}

String <- function(value) {
  new("String", value = value)
}
