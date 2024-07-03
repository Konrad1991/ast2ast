tools::Rd2HTML("../man/translate.Rd",
  out = "translate.html"
)
rmarkdown::render("../vignettes/DetailedDocumentation.Rmd",
  output_format = "html_document", output_dir = "."
)
rmarkdown::render("../vignettes/InformationForPackageAuthors.Rmd",
  output_format = "html_document", output_dir = "."
)
