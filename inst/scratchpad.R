# library(jsonlite)
# library(httr)

# api.key <- "31726802b8274ac60ae5526b9fb54703"
# shared.secret <- "193d5d983e42168ce3277db4e03f9a3d"

api_key <- apiAccount("31726802b8274ac60ae5526b9fb54703")
user <- apiUser("dshurick", api_key)

albums <- userGetTopAlbums(act, usr, limit = 200)

artists <- userGetTopArtists(act, usr, limit = 200)

tracks <- userGetArtistTracks("Radiohead", api_key = api_key, user = user)



tracks %>%
  count(artist, album, name, sort = TRUE)

tracks %>%
  count(artist, name, sort = TRUE)

tracks %>%
  count(artist, album, sort = TRUE)
