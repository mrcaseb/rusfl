#' Load and Parse USFL Roster for a Given Team
#'
#' @param team_abbr Valid USFL team abbreviation
#'
#' @return A tibble with team roster
#' @export
usfl_load_roster_team <- function(team_abbr){
  # for testing
  # team_abbr <- "HOU"
  cli::cli_process_start("Load and Parse Week {.val {team_abbr}}")

  id <- usfl_team_abbreviations(team_abbr)

  if(is.na(id)){
    cli::cli_abort("The team abbreviation {.arg {team_abbr}} is invalid!")
  }

  roster <- usfl_load_from_path("team", id, "roster")

  suppressWarnings({# integer conversion causes irrelevant warnings
    out <- purrr::map_dfr(seq_len(length(roster$groups$headers) - 1), function(i, r){
      names <- roster$groups$headers[[i]]$columns[[1]]$text |> tolower()
      names[names %in% c("offense", "defense", "special teams")] <- "player_name"
      rows <- roster$groups$rows[[i]]$columns |>
        purrr::map_dfr(function(k, names){
          values <- c(k$text, gsub("#", "", k$superscript[[1]]), k$imageUrl[[1]])
          names(values) <- c(names, "player_jersey", "headshot")
          tibble::as_tibble_row(values)
        }, names = names)
      rows
    }, r = roster) |>
      dplyr::mutate(team = team_abbr) |>
      dplyr::select(team, dplyr::everything()) |>
      dplyr::mutate(dplyr::across(c(player_jersey, age), as.integer))
  })

  cli::cli_process_done()

  out
}

#' Load Rosters of Multiple USFL Teams
#'
#' @param teams Valid USFL team abbreviations
#'
#' @return A tibble of rosters of the given teams
#' @export
usfl_load_rosters <- function(teams = NULL){
  if(is.null(teams)) teams <- usfl_team_abbreviations() |> names()
  purrr::map_dfr(teams, usfl_load_roster_team)
}
