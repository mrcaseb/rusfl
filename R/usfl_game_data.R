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

#' Extract and Parse PBP Data from Raw USFL Game Data
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

#' Extract and Parse Player Stats from Raw USFL Game Data
#'
#' @param raw_game_data Raw data loaded with [usfl_load_game_data()]
#'
#' @return A tibble with player stats
#' @export
usfl_parse_boxscores <- function(raw_game_data){
  # for testing
  # raw_game_data <- usfl_load_game_data(2)
  suppressWarnings({# numeric conversion causes irrelevant warnings
    player_stats <- seq_len(length(raw_game_data$boxscore$boxscoreSections$boxscoreItems) -1) |>
      purrr::map_dfr(function(j, raw_game_data){
        team <- raw_game_data$boxscore$boxscoreSections$boxscoreItems[[j]]
        abbr <- c(raw_game_data$header$leftTeam$name, raw_game_data$header$rightTeam$name)[[j]]
        purrr::map_dfr(seq_along(team$boxscoreTable$headers), function(i, team){
          names <- team$boxscoreTable$headers[[i]]$columns[[1]]$text |> tolower()
          names <- paste0(names[[1]], "_", names)
          names[[1]] <- "player_name"
          rows <- team$boxscoreTable$rows[[i]]$columns |>
            purrr::map_dfr(function(k, names){
              values <- k$text
              names(values) <- names
              tibble::as_tibble_row(values)
            }, names = names)
        }, team = team) |>
          dplyr::mutate(team = abbr) |>
          dplyr::select(team, dplyr::everything())
      }, raw_game_data = raw_game_data) |>
      dplyr::filter(player_name != "TOTALS") |>
      dplyr::group_by(team, player_name) |>
      dplyr::summarise(dplyr::across(.fns = function(x) dplyr::first(stats::na.omit(x)))) |>
      dplyr::ungroup() |>
      janitor::clean_names() |>
      dplyr::mutate(dplyr::across(
        .cols = !tidyselect::any_of(c("team", "player_name", "passing_com")),
        .fns = as.numeric
      ))
  })
}
