urls <- c(
  release = devtools::check_mac_release(pkg = ".", manual = FALSE),
  devel = devtools::check_mac_devel(pkg = ".", manual = FALSE)
)
print(urls)
