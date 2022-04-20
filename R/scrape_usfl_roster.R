scrape_usfl_roster <- function(){
  team_slucks <- load_usfl_teams() |>
    mutate(
      sluck = str_replace_all(team_name, " ", "-"),
      sluck = paste0(tolower(sluck), "-team-roster")
    ) |>
    select(team, sluck)

  roster <- purrr::map_dfr(vctrs::vec_seq_along(team_slucks), function(i, team_slucks){
    team <- team_slucks$team[[i]]
    sluck <- team_slucks$sluck[[i]]
    cli::cli_process_start("Load {.val {team}}")
    url <- paste0("https://www.theusfl.com/", sluck)
    raw <- rvest::read_html(url)
    headshot_nodes <- rvest::html_elements(
      raw,
      xpath = "//img[contains(concat(' ',normalize-space(@class),' '),'player-headshot')]"
    )
    headshots <- xml2::xml_attrs(headshot_nodes) |>
      purrr::map_dfr(function(x){
      tibble(
        player_name = x[["alt"]],
        player_headshot = x[["src"]]
      )
    })
    tbls <- rvest::html_table(raw)

    suppressWarnings({# integer conversion causes irrelevant warnings
      out <- purrr::map_dfr(tbls, function(unit){
        unit <- janitor::clean_names(unit)
        if(!any(names(unit) %in% c("offense", "defense", "special teams"))) return(tibble())
        names(unit)[names(unit) %in% c("offense", "defense", "special teams")] <- "player_name"
        unit
      }) |>
        mutate(team = team) |>
        separate(player_name, c("player_name", "player_jersey"), sep = "#") |>
        left_join(headshots, by = "player_name") |>
        select(team, everything()) |>
        replace_na(
          replace = list(
            player_headshot = "https://b.fssta.com/uploads/application/fs-app/default-headshot.vresize.72.72.medium.0.png"
          )
        ) |>
        mutate(across(c(player_jersey, age), as.integer))
    })
    cli::cli_process_done()
    out
  }, team_slucks = team_slucks)
}

# all_roster <- scrape_usfl_roster()
