load_usfl_teams <- function(){
  nflreadr::csv_from_url("https://github.com/ajreinhard/USFL/raw/main/data/teams.csv", header = TRUE) |>
    janitor::remove_empty("cols")
}
