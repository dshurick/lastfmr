#' artistGetTopTracks # TODO
#'
#' @param api_key FILLME # TODO
#' @param artist FILLME # TODO
#' @param mbid FILLME # TODO
#' @param autocorrect FILLME # TODO
#' @param limit FILLME # TODO
#' @param page FILLME # TODO
#'
#' @return FILLME # TODO
#' @export
artistGetTopTracks <- function(api_key,
                               artist = NULL,
                               mbid = NULL,
                               autocorrect = TRUE,
                               limit = NULL,
                               page = NULL) {


  coll = checkmate::makeAssertCollection()

  assertAlbum(mbid = mbid, artist = artist, add = coll)

  checkmate::assert_class(api_key, classes = c("apiAccount"), add = coll)
  checkmate::assert_int(limit,
                        lower = 1,
                        null.ok = TRUE,
                        add = coll)
  checkmate::assert_int(page,
                        lower = 1,
                        null.ok = TRUE,
                        add = coll)
  checkmate::reportAssertions(coll)
  # resp <- makeCall(
  #   query = list(
  #     method = "artist.gettoptracks",
  #     api_key = api_key$api_key,
  #     artist = artist,
  #     mbid = mbid,
  #     limit = limit,
  #     page = page,
  #     autocorrect = autocorrect,
  #     format = "json"
  #   )
  # )
  #
  # parsed <-
  #   jsonlite::fromJSON(httr::content(resp, as = "text"), flatten = TRUE)
  #
  # totalPages <-
  #   as.integer(parsed$topalbums$`@attr`$totalPages)
  #
  # pages <- lapply(1:totalPages, function(pageNum) {
  #   resp <- try(makeCall(
  #     query = list(
  #       method = "user.gettopalbums",
  #       api_key = api_key$api_key,
  #       user = user$user,
  #       period = period,
  #       limit = limit,
  #       page = pageNum,
  #       format = "json"
  #     )
  #   ))
  #
  #   parsed <-
  #     jsonlite::fromJSON(httr::content(resp, as = "text"), flatten = TRUE)
  #
  #   album.dtf <- parsed$topalbums$album %>%
  #     tibble::as_tibble() %>%
  #     dplyr::select(name,
  #                   playcount,
  #                   mbid,
  #                   artist.name,
  #                   artist.mbid,
  #                   rank = `@attr.rank`)
  #
  #   album.dtf
  # })
  #
  # albums <- dplyr::bind_rows(pages)

  # TODO
  return(NULL)

}
