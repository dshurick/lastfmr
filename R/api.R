
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
