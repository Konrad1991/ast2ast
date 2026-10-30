files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

types <- function() {
  new_type(
    Super,
    slots(
      next_elem |> type(Super)
    )
  )
}

f <- function() {

}

translate(f, types_f = types)
