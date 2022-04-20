#' Load Raw Game Data form USFL API
#'
#' @param usfl_game_id USFL game id, e.g. "usfl2"
#' @export
#' @return list of game data
usfl_load_game_data <- function(usfl_game_id){
  cli::cli_process_start("Load and Parse Week {.val {usfl_game_id}}")

  id <- stringr::str_extract(usfl_game_id, "[:digit:]+")

  game_data <- usfl_load_from_path("event", id, "data")

  cli::cli_process_done()

  game_data
}

#' Extract and Parse PBP Date from Raw USFL Game Data
#'
#' @param raw_game_data Raw data loaded with [usfl_load_game_data()]
#'
#' @return A tibble with pbp
#' @export
usfl_parse_pbp <- function(raw_game_data){
  tidyr::unnest_longer(raw_game_data$pbp$sections, groups, values_to = "drive") |>
    dplyr::rename(qtr = title) |>
    tidyr::unnest_wider(drive, names_sep = "_") |>
    tidyr::unnest_longer(drive_plays) |>
    tidyr::unnest_longer(drive_plays) |>
    tidyr::unnest(drive_plays) |>
    dplyr::rename_with(
      .fn = function(x) paste0("play_", x),
      .cols = !tidyselect::starts_with("drive_")
    ) |>
    dplyr::mutate(game_id = raw_game_data$header$id) |>
    dplyr::select(game_id, dplyr::everything())
}

