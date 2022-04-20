#' Loads and Parses USFL Scores for a Given Week
#'
#' @param week week to load
#'
#' @return A dataframe
#' @export
usfl_load_scores_in_week <- function(week){
  cli::cli_process_start("Load and Parse Week {.val {week}}")

  scores <- usfl_load_from_path("league", "scores-segment", paste0("w", week))

  out <- scores$sectionList |>
    dplyr::rename(week_day = id) |>
    tidyr::unnest(events) |>
    dplyr::mutate(url = entityLink$webUrl) |>
    dplyr::select(!tidyselect::any_of(c("favoriteEntities", "entityLink"))) |>
    dplyr::rename(away_team = upperTeam, home_team = lowerTeam) |>
    tidyr::unnest(away_team, names_sep = "_") |>
    tidyr::unnest(home_team, names_sep = "_")

  cli::cli_process_done()

  out
}

#' Load Scores of Multiple USFL Weeks
#'
#' @description This is a wrapper around [usfl_load_scores_in_week()]
#' @param weeks Weeks to load and combine
#'
#' @return A dataframe
#' @export
usfl_load_scores <- function(weeks = 1:10){
  purrr::map_dfr(weeks, usfl_load_scores_in_week)
}
