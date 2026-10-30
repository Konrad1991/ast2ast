
library(dplyr)

starwars_summary <- quote(starwars |>
  filter(species == "Human", homeworld == "Tatooine") |>
  select(name, height, mass, birth_year) |>
  mutate(bmi = mass / ((height / 100)^2)) |>
  rename(geburtsjahr = birth_year) |>
  arrange(desc(bmi))
)

get_ast <- function(code) {
  if (!is.call(code)) {
    return(code)
  }
  code <- as.list(code)
  res <- list()
  for (i in seq_along(code)) {
    res[[i]] <- get_ast(code[[i]])
  }
  res
}

ast <- get_ast(starwars_summary)
