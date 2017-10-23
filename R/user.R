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

  checkmate::assert_class(api_key, classes = c("apiAccount"), add = coll)
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

  tryCatch(
    resp <- makeCall(
      query = list(
        method = "user.gettopalbums",
        api_key = api_key,
        user = user,
        period = period,
        limit = limit,
        page = page,
        format = "json"
      )
    ),
    error = function(e) {
      print("Error from within userGetTopAlbums().")
      e
    }
  )

  parsed <-
    jsonlite::fromJSON(httr::content(resp, "text"))

  return(parsed)

}

#' Perform an HTTP GET Request.
#'
#' @param query List of query parameters.
#'
#' @return An HTTP \code{\link[httr]{response}}
#' @export
makeCall <- function(query = NULL) {
  coll = checkmate::makeAssertCollection()
  checkmate::assert_list(query, null.ok = TRUE, add = coll)
  checkmate::reportAssertions(coll)
  url <- httr::modify_url("http://ws.audioscrobbler.com/2.0/",
                          query = query)
  resp <- httr::GET(url)

  if (httr::http_error(resp)) {
    message(httr::http_status(resp)$message)
    stop("Request returned error code.")
  }
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  resp
}

#' Make an \code{apiAccount} object.
#'
#' @param api_key A string representing a Last.fm API key.
#'
#' @return An \code{apiAccount}.
#' @export
apiAccount <- function(api_key) {
  coll = checkmate::makeAssertCollection()
  checkmate::assert_string(api_key, add = coll)
  checkmate::reportAssertions(coll)

  tryCatch(
    resp <- makeCall(
      query = list(
        method = "chart.gettopartists",
        api_key = api_key,
        format = "json"
      )
    ),
    error = function(e) {
      print("Unable to create apiAccount.")
      e
    }
  )

  structure(list(api_key = api_key),
            class = "apiAccount")
}

#' Make an \code{apiUser} object.
#'
#' @param user A string representing a user's account on Last.fm
#' @param apiact An object returned from \code{\link{apiAccount}}
#'
#' @return An \code{apiUser}.
#' @export
apiUser <- function(user, apiact) {
  coll = checkmate::makeAssertCollection()
  checkmate::assert_string(user, add = coll)
  checkmate::assert_class(apiact, classes = c("apiAccount"), add = coll)
  checkmate::reportAssertions(coll)

  tryCatch(
    resp <- makeCall(
      query = list(
        method = "user.getinfo",
        api_key = apiact$api_key,
        user = user,
        format = "json"
      )
    ),
    error = function(e) {
      print("Unable to create apiUser.")
      e
    }
  )

  structure(list(user = user),
            class = "apiUser")
}
