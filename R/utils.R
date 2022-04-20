#' Save USFL API Key in User Environment
#'
#' @param key
#'
#' @return NULL invisibly
#' @export
usfl_set_api_key <- function(key = NULL) {
  rlang::check_installed("askpass", "to savely enter the API key")
  if (is.null(key)) {
    key <- askpass::askpass("Please enter your API key")
  }
  Sys.setenv("USFL_KEY" = key)
}

#' Get USFL API Key from User Environment
#'
#' @return
# @export
usfl_get_api_key <- function() {
  key <- Sys.getenv("USFL_KEY")
  if (identical(key, "")) {
    stop("No API key found, please use `usfl_set_api_key` to set an env var")
  }
  key
}
