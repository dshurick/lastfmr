#' Get the top albums listened to by a user
#'
#' Get the top albums listened to by a user. You can stipulate a time period.
#' Sends the overall chart by default.
#'
#' @param api_key (apiAccount; Required) : A Last.fm API key.
#' @param user (apiUser; Required) : The user name to fetch top albums for.
#' @param period (string; Optional) : The time period over which to retrieve
#'   top albums for. Must be one of
#'   \code{{overall, 7day, 1month, 3month, 6month, 12month}}
#' @param limit (int; Optional) : The number of results to fetch per page.
#'   Defaults to 50.
#' @param page (int; Optional) : The page number to fetch. Defaults to first
#'   page.
#'
#' @return A \code{\link[tibble]{tibble}} of results
#' @references
#' See \url{https://www.last.fm/api/show/user.getTopAlbums}
#'
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
  checkmate::assert_class(user, classes = c("apiUser"), add = coll)
  checkmate::assert_int(limit,
                        lower = 1,
                        null.ok = TRUE,
                        add = coll)
  checkmate::assert_int(page,
                        lower = 1,
                        null.ok = TRUE,
                        add = coll)
  checkmate::reportAssertions(coll)
  resp <- makeCall(
    query = list(
      method = "user.gettopalbums",
      api_key = api_key$api_key,
      user = user$user,
      period = period,
      limit = limit,
      page = page,
      format = "json"
    )
  )

  parsed <-
    jsonlite::fromJSON(httr::content(resp, as = "text"), flatten = TRUE)

  totalPages <-
    as.integer(parsed$topalbums$`@attr`$totalPages)

  pages <- lapply(1:totalPages, function(pageNum) {
    resp <- try(makeCall(
      query = list(
        method = "user.gettopalbums",
        api_key = api_key$api_key,
        user = user$user,
        period = period,
        limit = limit,
        page = pageNum,
        format = "json"
      )
    ))

    parsed <-
      jsonlite::fromJSON(httr::content(resp, as = "text"), flatten = TRUE)

    album.dtf <- parsed$topalbums$album %>%
      tibble::as_tibble() %>%
      dplyr::select(name,
                    playcount,
                    mbid,
                    artist.name,
                    artist.mbid,
                    rank = `@attr.rank`)

    album.dtf
  })

  albums <- dplyr::bind_rows(pages)

  return(albums)

}


#' Get the top artists listened to by a user
#'
#' Get the top artists listened to by a user. You can stipulate a time period.
#' Sends the overall chart by default.
#'
#' @param api_key (apiAccount; Required) : A Last.fm API key.
#' @param user (apiUser; Required) : The user name to fetch top albums for.
#' @param period (string; Optional) : The time period over which to retrieve
#'   top albums for. Must be one of
#'   \code{{overall, 7day, 1month, 3month, 6month, 12month}}
#' @param limit (int; Optional) : The number of results to fetch per page.
#'   Defaults to 50.
#' @param page (int; Optional) : The page number to fetch. Defaults to first
#'   page.
#'
#' @return A \code{\link[tibble]{tibble}} of results
#'
#' @references
#' See \url{https://www.last.fm/api/show/user.getArtistTracks}
#'
#' @export
userGetTopArtists <- function(api_key,
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
  checkmate::assert_class(user, classes = c("apiUser"), add = coll)
  checkmate::assert_int(limit,
                        lower = 1,
                        null.ok = TRUE,
                        add = coll)
  checkmate::assert_int(page,
                        lower = 1,
                        null.ok = TRUE,
                        add = coll)
  checkmate::reportAssertions(coll)

  resp <- makeCall(
    query = list(
      method = "user.gettopartists",
      api_key = api_key$api_key,
      user = user$user,
      period = period,
      limit = limit,
      page = page,
      format = "json"
    )
  )


  parsed <-
    jsonlite::fromJSON(httr::content(resp, as = "text"), flatten = TRUE)

  totalPages <-
    as.integer(parsed$topartists$`@attr`$totalPages)


  pages <- lapply(1:totalPages, function(pageNum) {
    resp <- makeCall(
      query = list(
        method = "user.gettopartists",
        api_key = api_key$api_key,
        user = user$user,
        period = period,
        limit = limit,
        page = pageNum,
        format = "json"
      )
    )

    parsed <-
      jsonlite::fromJSON(httr::content(resp, as = "text"), flatten = TRUE)

    artist.dtf <- parsed$topartists$artist %>%
      tibble::as_tibble() %>%
      dplyr::select(name,
                    playcount,
                    mbid,
                    rank = `@attr.rank`)

    artist.dtf
  })

  artists <- dplyr::bind_rows(pages)

  return(artists)

}


#' Get a list of tracks scrobbled by a given last.fm user
#'
#' Get a list of tracks by a given artist scrobbled by this user, including
#' scrobble time. Can be limited to specific timeranges, defaults to all
#' time.
#'
#' @param artist (character; Required) : The artist to search for.
#' @param api_key (apiAccount; Required) : A Last.fm API key.
#' @param user (apiUser; Required) : The user name to fetch top albums for.
#' @param limit (int; Optional) : The number of results to fetch per page.
#'   Defaults to 50.
#' @param page (int; Optional) : The page number to fetch. Defaults to first
#'   page.
#'
#' @return A \code{\link[tibble]{tibble}} of results
#'
#' @references
#' See \url{https://www.last.fm/api/show/user.getArtistTracks}
#'
#' @export
userGetArtistTracks <- function(artist,
                                api_key,
                                user,
                                limit = NULL,
                                page = NULL) {
  coll = checkmate::makeAssertCollection()

  checkmate::assert_class(api_key, classes = c("apiAccount"), add = coll)
  checkmate::assert_class(user, classes = c("apiUser"), add = coll)
  checkmate::assert_string(artist, add = coll)
  checkmate::assert_int(page,
                        lower = 1,
                        null.ok = TRUE,
                        add = coll)
  checkmate::reportAssertions(coll)

  tracks <- NULL

  for (pageNum in 1:200) {
    resp <- makeCall(
      query = list(
        method = "user.getartisttracks",
        api_key = api_key$api_key,
        user = user$user,
        artist = artist,
        limit = 200,
        page = pageNum,
        format = "json"
      )
    )

    parsed <-
      jsonlite::fromJSON(httr::content(resp, as = "text"), flatten = TRUE)

    if (!length(parsed$artisttracks$track))
      break()

    tracks.dtf <- parsed$artisttracks$track %>%
      tibble::as_tibble() %>%
      dplyr::select(name,
                    mbid,
                    artist = `artist.#text`,
                    album = `album.#text`)

    tracks <- dplyr::bind_rows(tracks, tracks.dtf)
  }

  return(tracks)

}

#' Get the metadata and tracklist for an album
#'
#' Get the metadata and tracklist for an album on Last.fm using the album
#' name or a musicbrainz id.
#'
#' @param artist (character; Required, unless mbid) : The artist name.
#' @param album (character; Required, unless mbid) : The album name.
#' @param mbid (character; Optional) : The musicbrainz id for the album
#' @param autocorrect (logical; Optional) : Transform misspelled artist
#'   names into correct artist names, returning the correct version instead.
#'   The corrected artist name will be returned in the response.
#' @param user (apiUser; Optional) : The username for the context of the
#'   request. If supplied, the user's playcount for this album is included
#'  in the response.
#' @param api_key (apiAccount; Required) : A Last.fm API key.
#' @param lang (Optional) : The language to return the biography in,
#'   expressed as an ISO 639 alpha-2 code.
#'
#' @return A \code{\link[tibble]{tibble}} of results
#'
#' @references
#' See \url{https://www.last.fm/api/show/album.getInfo}
#'
#' @export
albumGetInfo <- function(api_key,
                         artist = NULL,
                         album = NULL,
                         mbid = NULL,
                         autocorrect = TRUE,
                         user = NULL,
                         lang = NULL) {
  coll = checkmate::makeAssertCollection()

  # Simple custom check function
  checkAlbum = function(artist,
                        album,
                        mbid) {
    if (is.null(mbid) && (is.null(artist) || is.null(album))) {
      "You must either provide mbid or artist & album."
    } else {
      TRUE
    }
  }

  assertAlbum = checkmate::makeAssertionFunction(checkAlbum)

  assertAlbum(artist, album, mbid, add = coll)
  checkmate::assert_class(api_key, classes = c("apiAccount"), add = coll)
  checkmate::assert_class(user, classes = c("apiUser"), add = coll)
  checkmate::assert_string(artist, null.ok = TRUE, add = coll)
  checkmate::assert_string(album, null.ok = TRUE, add = coll)
  checkmate::assert_flag(autocorrect, null.ok = TRUE, add = coll)
  checkmate::reportAssertions(coll)

  resp <- makeCall(
    query = list(
      method = "album.getinfo",
      api_key = api_key$api_key,
      user = user$user,
      artist = artist,
      album = album,
      mbid = mbid,
      autocorrect = if (autocorrect)
        1
      else
        0,
      format = "json"
    )
  )

  parsed <-
    jsonlite::fromJSON(httr::content(resp, as = "text"), flatten = TRUE)

  tracks <- parsed$album$tracks$track %>%
    dplyr::select(name,
                  duration,
                  track_num = `@attr.rank`,
                  artist_mbid = artist.mbid) %>%
    dplyr::mutate(
      duration = as.integer(duration),
      track_num = as.integer(track_num),
      artist_name = parsed$album$artist,
      album_name = parsed$album$name,
      album_mbid = parsed$album$mbid
    ) %>%
    tibble::as_tibble()

  tracks

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
