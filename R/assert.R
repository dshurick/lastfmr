
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
  checkmate::makeAssertionFunction(
    check.fun = checkAlbum
  )

assertArtist <-
  checkmate::makeAssertionFunction(
    check.fun = checkArtist
  )
