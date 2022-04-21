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

#' Get valid USFL Team Abbreviations and Corresponding API IDs
#'
#' @param abbr A valid abbreviation for which to output the ID and name.
#'   The default `NULL` returns all IDs and names.
#'
#' @return A named vector.
#' @export
#'
#' @examples
#' usfl_team_abbreviations()
#' usfl_team_abbreviations(c("BHAM", "HOU"))
usfl_team_abbreviations <- function(abbr = NULL){
  ids <- c("BHAM", "HOU", "MICH", "NJ", "NO", "PHI", "PIT", "TB")
  vec <- c(     2,     6,     11,   12,    7,     4,    16,   18)
  names(vec) <- ids
  if (is.null(abbr)) return(vec)
  vec[abbr]
}

# from nflfastR
# take a time string of the format "MM:SS" and convert it to seconds
time_to_seconds <- function(time){
  as.numeric(strptime(time, format = "%M:%S")) -
    as.numeric(strptime("0", format = "%S"))
}
