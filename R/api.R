#' Perform USFL API Request
#'
#' @inheritDotParams httr2::req_url_path_append
#'
#' @return API response
# @export
#
# @examples
# \donttest{
# game_data <- load_from_usfl_path("event", "1", "data")
# scores <- load_from_usfl_path("league", "scores-segment", "w1")
# }
load_from_usfl_path <- function(...){
  httr2::request("https://api.foxsports.com/bifrost/v1/usfl/usflcom") |>
    httr2::req_url_path_append(...) |>
    httr2::req_url_query(apikey = usfl_get_api_key()) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
}
