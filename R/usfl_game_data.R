#' Load Raw Game Data form USFL API
#'
#' @param usfl_game_id USFL game id, e.g. "usfl2"
#' @export
#' @return list of game data
load_usfl_game_data <- function(usfl_game_id){
  cli::cli_process_start("Load and Parse Week {.val {usfl_game_id}}")

  id <- stringr::str_extract(usfl_game_id, "[:digit:]+")

  game_data <- load_from_usfl_path("event", id, "data")

  cli::cli_process_done()

  game_data
}



