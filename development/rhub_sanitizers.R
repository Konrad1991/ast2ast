# Sanitizer checks (ASAN/UBSAN) on rhub v2 via GitHub Actions.
# Requires: .github/workflows/rhub.yaml committed + pushed, GITHUB_PAT via gitcreds.
library(rhub)

if (!file.exists(".github/workflows/rhub.yaml")) {
  rhub_setup()
  stop("rhub.yaml created: commit + push it, then rerun this script")
}

# usethis::create_github_token()
rhub_doctor()
gitcreds::gitcreds_set()

# gcc-asan = CRAN's fedora-gcc-devel ASAN + UBSAN setup
platforms <- c("gcc-asan", "clang-asan", "clang-ubsan")
available <- rhub_platforms()$name
stopifnot(all(platforms %in% available))

branch <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
rhub_check(platforms = platforms, branch = branch)
