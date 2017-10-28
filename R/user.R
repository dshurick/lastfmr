#' S3 generics for user API calls
#'
#' @name user
#'
#' @param api_key (apiAccount; Required) : A Last.fm API key.
#' @param user (apiUser; Required) : The user name to fetch top albums for.
#' @param period (string; Optional) : The time period over which to retrieve
#'   top albums for. Must be one of
#'   \code{{overall, 7day, 1month, 3month, 6month, 12month}}
#' @param artist (character; Required unless mbid) : The artist name.
#' @param album (character; Required unless mbid) : The album name.
#' @param limit (int; Optional) : The number of results to fetch per page.
#'   Defaults to 50.
#' @param page (int; Optional) : The page number to fetch. Defaults to first
#'   page.
#'
#' @return A \code{\link[tibble]{tibble}} of results
#'
#' @family user
NULL

#' Get the top albums listened to by a user
#'
#' Get the top albums listened to by a user. You can stipulate a time period.
#' Sends the overall chart by default.
#'
#' @inheritParams user
#' @inherit user return
#'
#' @references
#' See \url{https://www.last.fm/api/show/user.getTopAlbums}
#'
#' @export
#' @family user
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

  pb <- progress::progress_bar$new(
    format = "Pulling data from Last.fm [:bar] :percent eta: :eta",
    total = totalPages,
    clear = FALSE,
    width = 79
  )

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
      dplyr::select(album_name = name,
                    album_playcount = playcount,
                    album_mbid = mbid,
                    artist_name = artist.name,
                    artist_mbid = artist.mbid,
                    album_rank = `@attr.rank`)

    pb$tick()

    album.dtf
  })

  albums <- dplyr::bind_rows(pages)

  return(albums)

}


#' Get the top artists listened to by a user
#'
#' Get the top artists listened to by a user. You can stipulate a time
#' period. Sends the overall chart by default.
#'
#' @inheritParams user
#' @inherit user return
#'
#' @references
#' See \url{https://www.last.fm/api/show/user.getArtistTracks}
#'
#' @export
#' @family user
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

  pb <- progress::progress_bar$new(
    format = "Pulling data from Last.fm [:bar] :percent eta: :eta",
    total = totalPages,
    clear = FALSE,
    width = 79
  )

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
      dplyr::select(artist_name = name,
                    artist_playcount = playcount,
                    artist_mbid = mbid,
                    artist_rank = `@attr.rank`)

    pb$tick()

    artist.dtf
  })

  artists <- dplyr::bind_rows(pages)

  return(artists)

}

#' Get the top tracks listened to by a user.
#'
#' Get the top tracks listened to by a user. You can stipulate a time period.
#' Sends the overall chart by default.
#'
#' @inheritParams user
#'
#' @inherit user return
#'
#' @references
#' See \url{https://www.last.fm/api/show/user.getArtistTracks}
#'
#' @export
#' @family user
userGetTopTracks <- function(api_key,
                             user,
                             period = c("overall",
                                        "7day",
                                        "1month",
                                        "3month",
                                        "6month",
                                        "12month"),
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
        method = "user.gettoptracks",
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


#' Get a list of tracks scrobbled by a given last.fm user
#'
#' Get a list of tracks by a given artist scrobbled by this user, including
#' scrobble time. Can be limited to specific timeranges, defaults to all
#' time.
#'
#' @inheritParams user
#'
#' @inherit user return
#'
#' @references
#' See \url{https://www.last.fm/api/show/user.getArtistTracks}
#'
#' @export
#' @family user
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
