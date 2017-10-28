
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

  content <- httr::content(resp)

  if (!is.null(content[["error"]])) {
    message("Here are the contents of the query: ")
    utils::str(query)
    stop(content[["message"]])
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
#' @param api_key An object returned from \code{\link{apiAccount}}
#'
#' @return An \code{apiUser}.
#' @export
apiUser <- function(user, api_key) {
  coll = checkmate::makeAssertCollection()
  checkmate::assert_string(user, add = coll)
  checkmate::assert_class(api_key, classes = c("apiAccount"), add = coll)
  checkmate::reportAssertions(coll)

  resp <- makeCall(query = list(
    method = "user.getinfo",
    api_key = api_key$api_key,
    user = user,
    format = "json"
  ))

  content <- httr::content(resp)
  if (!is.null(content[["error"]]))
    stop(content[["message"]])

  structure(list(user = user),
            class = "apiUser")
}

#' Pull all Last.fm data for a user
#'
#' @param api_key (apiAccount; Required) : A Last.fm API key.
#' @param user (apiUser; Optional) : The username for the context of the
#'   request. If supplied, the user's playcount for this album is included
#'  in the response.
#'
#' @return A \code{\link[tibble]{tibble}} of results.
#' @export
pullUserData <- function(api_key, user) {
  coll = checkmate::makeAssertCollection()

  checkmate::assert_class(
    api_key,
    classes = c("apiAccount"),
    add = coll
  )

  checkmate::assert_class(
    user,
    classes = c("apiUser"),
    null.ok = TRUE,
    add = coll
  )
  checkmate::reportAssertions(coll)

  albums <- userGetTopAlbums(api_key, user, limit = 200)

  album_tracks <- albums %>%
    dplyr::slice(1:20) %>%
    dplyr::rowwise() %>%
    dplyr::do(albumGetInfo(
      api_key,
      album = .$album_name,
      artist = .$artist_name
    ))

}
