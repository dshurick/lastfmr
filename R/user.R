#' Get Albums with Most Plays for User
#'
#' @param api_key (string; Required) : A Last.fm API key.
#' @param user (string; Required) : The user name to fetch top albums for.
#' @param period (string; Optional) : The time period over which to retrieve
#'   top albums for. Must be one of
#'   \code{{overall, 7day, 1month, 3month, 6month, 12month}}
#' @param limit (int; Optional) : The number of results to fetch per page.
#'   Defaults to 50.
#' @param page (int; Optional) : The page number to fetch. Defaults to first
#'   page.
#'
#' @return parsed JSON
#' @export
userGetTopAlbums <- function(api_key,
                             user,
                             period = c("overall",
                                        "7day",
                                        "1month",
                                        "3month",
                                        "6month",
                                        "12month"),
                             limit = NULL,
                             page = NULL) {
  period = match.arg(period)

  coll = checkmate::makeAssertCollection()

  checkmate::assert_string(api_key, add = coll)
  checkmate::assert_string(user, add = coll)
  checkmate::assert_int(limit,
                        lower = 1,
                        null.ok = TRUE,
                        add = coll)
  checkmate::assert_int(page,
                        lower = 1,
                        null.ok = TRUE,
                        add = coll)
  checkmate::reportAssertions(coll)

  url <-   httr::modify_url(
    "http://ws.audioscrobbler.com/2.0/",
    query = list(
      method = "user.gettopalbums",
      api_key = api_key,
      user = user,
      period = period,
      limit = limit,
      page = page,
      format = "json"
    )
  )

  resp <- httr::GET(url)

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <-
    jsonlite::fromJSON(httr::content(resp, "text"))

  return(parsed)

}
