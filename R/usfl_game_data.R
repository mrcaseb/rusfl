#' Load Raw Game Data form USFL API
#'
#' @param usfl_game_id USFL game id, e.g. "usfl2"
#' @export
#' @return list of game data
usfl_load_game_data <- function(usfl_game_id){
  cli::cli_process_start("Load Week {.val {usfl_game_id}}")

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
  # for testing
  # raw_game_data <- usfl_load_game_data(2)
  out <- tidyr::unnest_longer(raw_game_data$pbp$sections, groups, values_to = "drive") |>
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
    dplyr::select(game_id, dplyr::everything()) |>
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_id"), as.integer)) |>
    dplyr::select(!tidyselect::ends_with("_imageType")) |>
    dplyr::select(!tidyselect::ends_with("_imageUrl")) |>
    dplyr::mutate(
      drive_ends_in_score = drive_leftTeamScoreChange | drive_rightTeamScoreChange,
      drive_ends_in_score = as.numeric(drive_ends_in_score),
      scoring_play = play_leftTeamScoreChange | play_rightTeamScoreChange,
      scoring_play = as.numeric(scoring_play),
      posteam = usfl_abbreviation_from_full_name(drive_entityLink$imageAltText),
      drive_imageAltText = usfl_abbreviation_from_full_name(drive_imageAltText),
      posteam_logo = drive_entityLink$imageUrl,
      away_team_scored = as.numeric(play_leftTeamScoreChange),
      home_team_scored = as.numeric(play_rightTeamScoreChange),
      qtr_seconds_remaining = as.integer(time_to_seconds(play_timeOfPlay)),
      qtr = 1L + cumsum(stringr::str_detect(play_playDescription, "End Quarter")),
      qtr = dplyr::if_else(qtr_seconds_remaining == 0 & dplyr::lag(qtr != 4L), qtr - 1L, qtr),
      yds_gained = as.integer(stringr::str_extract(play_playDescription, "(?<=for |Gain of )[:graph:]{1,4}(?= yards)")),
      down = as.integer(stringr::str_extract(play_title, "[:digit:]")),
      ydstogo = as.integer(stringr::str_extract(play_title, "(?<=AND )[:digit:]{1,2}")),
      timeout_team = stringr::str_extract(play_playDescription, "(?<=Timeout #[:digit:] by )[:upper:]{2,4}"),
      play_subtitle = if_else(play_subtitle == "50", "MID50", play_subtitle),
      side_of_field = stringr::str_extract(play_subtitle, "[:upper:]{2,4}"),
      yrdln = as.integer(stringr::str_extract(play_subtitle, "[:digit:]{1,2}")),
      season = 2021,
      roof = "open",
      yardline_100 = dplyr::if_else(side_of_field == posteam, 100L - yrdln, yrdln),
    ) |>
    dplyr::select(!c(
      drive_entityLink,
      drive_leftTeamScoreChange,
      drive_rightTeamScoreChange,
      play_leftTeamScoreChange,
      play_rightTeamScoreChange,
      play_leftTeamAbbr,
      play_rightTeamAbbr,
      play_qtr,
      play_periodOfPlay,
      drive_leftTeamScore,
      drive_rightTeamScore
      )) |>
    dplyr::rename(
      drive_result = drive_title,
      drive_posteam = drive_imageAltText,
      posteam_logo_alt = drive_alternateImageUrl,
      away_team = drive_leftTeamAbbr,
      home_team = drive_rightTeamAbbr,
      key_player_name = play_imageAltText,
      key_player_headshot = play_alternateImageUrl,
      yardline = play_subtitle,
      desc = play_playDescription,
      game_clock = play_timeOfPlay,
      away_score = play_leftTeamScore,
      home_score = play_rightTeamScore,
    ) |>
    tidyr::separate(
      drive_subtitle,
      into = c("drive_plays", "drive_yards", "drive_duration"),
      sep = " · "
    ) |>
    dplyr::mutate(dplyr::across(
      .cols = c(qtr, drive_plays, drive_yards),
      .fns = ~ as.numeric(stringr::str_extract(.x, "[:digit:]+"))
    )) |>
    dplyr::mutate(
      half_seconds_remaining = dplyr::if_else(
        .data$qtr %in% c(1, 3),
        .data$qtr_seconds_remaining + 900L,
        .data$qtr_seconds_remaining
      ),
      game_seconds_remaining = dplyr::if_else(
        .data$qtr %in% c(1, 2, 3, 4),
        .data$qtr_seconds_remaining + (900L * (4L - as.integer(.data$qtr))),
        .data$qtr_seconds_remaining
      ),
      away_score = ifelse(play_id == 1, 0L, as.integer(away_score)),
      home_score = ifelse(play_id == 1, 0L, as.integer(home_score)),
      away_timeout = dplyr::if_else(timeout_team == away_team, 1, 0, missing = 0),
      home_timeout = dplyr::if_else(timeout_team == home_team, 1, 0, missing = 0),
      defteam = ifelse(posteam == home_team, away_team, home_team),

      game_half = ifelse(qtr < 3, 'Half1', 'Half2')
    ) |>
    tidyr::fill(c(away_score, home_score)) |>
    dplyr::mutate(
      score_differential = ifelse(
        posteam == home_team,
        home_score - away_score,
        away_score - home_score
      )
    ) |>
    dplyr::group_by(game_id, game_half) |>
    dplyr::mutate(
      away_timeouts_remaining = 3 - cumsum(away_timeout),
      home_timeouts_remaining = 3 - cumsum(home_timeout),
      posteam_timeouts_remaining = ifelse(posteam == home_team, home_timeouts_remaining, away_timeouts_remaining),
      defteam_timeouts_remaining = ifelse(defteam == home_team, home_timeouts_remaining, away_timeouts_remaining)
    ) |>
    dplyr::ungroup() |>
    nflfastR::calculate_expected_points() |>
    dplyr::select(
      game_id:defteam_timeouts_remaining, ep,
      dplyr::everything()
    )

  out
}

#' Extract and Parse Player Stats from Raw USFL Game Data
#'
#' @param raw_game_data Raw data loaded with [usfl_load_game_data()]
#' @param type One of "players" or "teams". Choose type of stat output.
#'
#' @return A tibble with player/team stats
#' @export
usfl_parse_boxscores <- function(raw_game_data, type = c("players", "teams")){
  # for testing
  # raw_game_data <- usfl_load_game_data(2)

  type <- rlang::arg_match(type)

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
      ),
      game_id = raw_game_data$header$id
      ) |>
      dplyr::select(game_id, dplyr::everything())
  })

  team_stats <- raw_game_data$boxscore$boxscoreSections$boxscoreMatchup[[3]] |>
    dplyr::rename(type = title) |>
    tidyr::unnest(rows) |>
    janitor::clean_names() |>
    dplyr::mutate(
      away_team = raw_game_data$header$leftTeam$name,
      home_team = raw_game_data$header$rightTeam$name,
      game_id = raw_game_data$header$id
    ) |>
    dplyr::select(
      game_id, type, title,
      away_team, away_stat = left_stat,
      home_stat = right_stat, home_team
    )

  switch(type,
    "players" = player_stats,
    "teams" = team_stats
  )
}
