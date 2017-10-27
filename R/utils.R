
# Flag inline helpers as global variables so R CMD check doesn't warn
utils::globalVariables(c(":=", ".data", ".env", "name", "mbid", "artist.#text",
                         "album.#text", "artists", "playcount", "artist.name",
                         "artist.mbid", "@attr.rank", "duration", "track_num",
                         "x"))

#' @importFrom checkmate vname makeAssertion
NULL
