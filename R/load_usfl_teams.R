#' Load USFL Teams File from Anthony Reinhard's Github
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' usfl_load_teams()
usfl_load_teams <- function(){
  teams <- nflreadr::csv_from_url("https://github.com/ajreinhard/USFL/raw/main/data/teams.csv", header = TRUE)
  janitor::remove_empty(teams, "cols")
}
