if (!grepl("make_package", getwd())) {
  setwd("./development/make_package")
}
files <- list.files("../../R", full.names = TRUE)
invisible(lapply(files, source))

files <- file.path("inst", "tools", c(
  "file1.R", "file2.R", "file3.R",
  "file4.R", "file5.R", "file6.R",
  "file7.R"
))
make_package(files, "f", package_dir = "testpkg")
