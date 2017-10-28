
checkArtist = function(mbid, artist) {
  if (is.null(mbid) && is.null(artist)) {
    "You must either provide mbid or artist."
  } else {
    TRUE
  }
}

checkAlbum = function(mbid, artist, album) {
  if (is.null(mbid) && (is.null(artist) || is.null(album))) {
    "You must either provide mbid or artist & album."
  } else {
    TRUE
  }
}

assertAlbum <-
  function(mbid,
           artist,
           album,
           .var.name = "album",
           add = NULL) {
    res <- checkAlbum(mbid, artist, album)
    checkmate::makeAssertion(mbid, res, .var.name, add)
  }


assertArtist <-
  function(mbid,
           artist,
           .var.name = "artist",
           add = NULL) {
    res <- checkArtist(mbid, artist)
    checkmate::makeAssertion(mbid, res, .var.name, add)
  }

# assertArtist <-
#   checkmate::makeAssertionFunction(
#     check.fun = checkArtist
#   )
