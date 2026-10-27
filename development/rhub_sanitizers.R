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
# m1-san = CRAN's M1 mac ASAN + UBSAN (rejected 1.0.1)
platforms <- c("gcc-asan", "clang-asan", "clang-ubsan", "m1-san")
platforms <- c("clang23")
available <- rhub_platforms()$name
stopifnot(all(platforms %in% available))

# NOTE: the cache key ignores the compiler: clang-asan and clang23 share the
# Ubuntu 26.04 R library, so clang23 loads ASan-built processx.so and fails.
ids <- system2("gh", c("cache", "list", "--limit", "100", "--json", "id,key",
  "--jq", shQuote('.[] | select(.key | startswith("Ubuntu 26.04")) | .id')),
  stdout = TRUE)
for (id in ids) system2("gh", c("cache", "delete", id))

branch <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
rhub_check(platforms = platforms, branch = branch)
platforms <- c("clang23")
available <- rhub_platforms()$name
stopifnot(all(platforms %in% available))

branch <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
rhub_check(platforms = platforms, branch = branch)

