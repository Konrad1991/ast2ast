files <- list.files("./R/", full.names = TRUE)
trash <- lapply(files, source)

testall <- function(a) {
  if (a == 0) {
    print("test1")
  } else if (a == 1) {
    print("test2")
  } else if (a == 2) {
    print("test3")
  } else {
    if (a == 3) {
      print("bla")
    }
    print("test4")
  }
}
res <- translate(testall, verbose = TRUE, getsource = TRUE)
