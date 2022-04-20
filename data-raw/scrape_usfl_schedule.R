#### DEPRECATED ####
scrape_usfl_schedule <- function(){
  raw <- read_html("https://www.theusfl.com/schedule")
  weeks <- xml_find_all(raw, "//section[contains(concat(' ',normalize-space(@id),' '),'w')]")
  out <-
    purrr::map_dfr(weeks, function(w){
      # for testing
      w <- weeks[[1]]
      week <- xml_find_all(w, './/*[@class="heading"]') |> xml_text(trim = TRUE)
      game_days <- xml_find_all(w, './/*[@class="title"]') |> xml_text(trim = TRUE)

      # loop over spans to be able to allocate the game days
      purrr::map(seq_along(game_days), function(s, w, week){
        game_day <- xml_find_all(w, glue::glue('.//span[{s}]//*[@class="title"]')) |> xml_text(trim = TRUE)

        games <- xml_find_all(w, glue::glue('.//span[{s}]//*[@class="score-card-elem"]'))

        game_urls <- xml_attrs(games) |>
          purrr::map_chr(function(x) x[["href"]]) |>
          tibble::as_tibble() |>
          dplyr::rename(game_url = value) |>
          dplyr::mutate(game_url = paste0("https://www.theusfl.com", game_url))

        team_info <- xml_find_all(games, './/*[@class="team-logo-image"]') |>
          xml_attrs() |>
          purrr::map_dfr(function(x){
            list(
              alt_name = x[["alt"]],
              logo_url = x[["src"]]
            )
          }) |>
          dplyr::mutate(
            alt_name = stringr::str_replace(alt_name, "Logo", "")
          )

        teams <- xml_find_all(games, './/*[@class="team-name"]') |>
          html_text2() |>
          tibble::as_tibble() |>
          dplyr::rename(team = value) |>
          dplyr::mutate(
            type = rep(c("away", "home"), dplyr::n()/2)
          )

        team_records <- xml_find_all(games, './/*[@class="team-record"]') |>
          html_text2() |>
          tibble::as_tibble() |>
          dplyr::rename(record = value)

        overall <- dplyr::bind_cols(teams, team_records, team_info) |>
          tidyr::pivot_wider(
            names_from = type,
            values_from = c(team, record, alt_name, logo_url),
            names_expand = TRUE,
            names_glue = "{type}_{.value}"
          ) |>
          tidyr::unnest_longer(everything()) |>
          dplyr::mutate(
            game_day = game_day,
            week = week
          ) |>
          dplyr::select(
            week, game_day, everything()
          ) |>
          dplyr::bind_cols(game_urls) |>
          dplyr::mutate()

        overall

      }, w = w, week = week)

    })
  out
}


