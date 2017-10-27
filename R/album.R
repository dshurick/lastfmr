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

  assertAlbum(mbid = mbid, artist = artist, album = album, add = coll)
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


