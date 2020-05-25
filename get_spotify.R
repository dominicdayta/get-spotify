library(httr)

clientID = '##############################'
secret = '################################'
response = POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)

mytoken = content(response)$access_token

HeaderValue = paste0('Bearer ', mytoken)

message("Spotify Connection Ready.")

get_artist_data<-function(artistID = '0qlWcS66ohOIi0M8JZwPft'){
  
  URI = paste0('https://api.spotify.com/v1/artists/', artistID)
  response2 = GET(url = URI, add_headers(Authorization = HeaderValue))
  Artist = content(response2)
  Artist
  
  # Get albums
  album_URI = paste0('https://api.spotify.com/v1/artists/', artistID,'/albums')
  album_response = GET(url = album_URI, add_headers(Authorization = HeaderValue))
  albums = content(album_response)
  
  nalbums = length(albums$items)
  albums_list<-data.frame(name=character(nalbums),id=character(nalbums),
                          type=character(nalbums),
                          total_tracks=character(nalbums),
                          release_date=character(nalbums),stringsAsFactors=FALSE)
  for(i in 1:nalbums){
    albums_list[i,]$id <- unlist(albums$items[[i]]$id)
    albums_list[i,]$name <- unlist(albums$items[[i]]$name)
    albums_list[i,]$type<- unlist(albums$items[[i]]$type)
    albums_list[i,]$total_tracks <- unlist(albums$items[[i]]$total_tracks)
    albums_list[i,]$release_date <- unlist(albums$items[[i]]$release_date)
  }
  
  # Get Tracks and Album Information
  tracks_list<-data.frame(name=character(0),id=character(0),
                          artist=character(0),disc_number=numeric(0),
                          track_number=numeric(0),duration_ms=numeric(0),
                          explicit=character(0),album=character(0),
                          stringsAsFactors=FALSE)
  
  for(albumID in albums_list$id){
    track_URI = paste0('https://api.spotify.com/v1/albums/', albumID,'/tracks')
    track_response = GET(url = track_URI, add_headers(Authorization = HeaderValue))
    tracks = content(track_response)
    
    ntracks = length(tracks$items)
    tracks_list1<-data.frame(name=character(ntracks),id=character(ntracks),
                             artist=character(ntracks),disc_number=numeric(ntracks),
                             track_number=numeric(ntracks),duration_ms=numeric(ntracks),
                             explicit=character(ntracks),album=character(ntracks),
                             stringsAsFactors=FALSE)
    
    for(i in 1:ntracks){
      tracks_list1[i,]$id <- unlist(tracks$items[[i]]$id)
      tracks_list1[i,]$name <- unlist(tracks$items[[i]]$name)
      tracks_list1[i,]$album <- albums_list[albums_list$id==albumID,]$name
      tracks_list1[i,]$artist <- unlist(tracks$items[[i]]$artists[[1]]$name)
      tracks_list1[i,]$disc_number <- unlist(tracks$items[[i]]$disc_number)
      tracks_list1[i,]$track_number <- unlist(tracks$items[[i]]$track_number)
      tracks_list1[i,]$duration_ms <- unlist(tracks$items[[i]]$duration_ms)
    }
    
    tracks_list<-rbind(tracks_list,tracks_list1)
  }
  
  # Get Additional Track Details
  for(i in 1:nrow(tracks_list)){
    Sys.sleep(0.10)
    track_URI2 = paste0('https://api.spotify.com/v1/audio-features/', tracks_list$id[i])
    track_response2 = GET(url = track_URI2, add_headers(Authorization = HeaderValue))
    tracks2 = content(track_response2)
    
    tracks_list$key[i] <- tracks2$key
    tracks_list$mode[i] <- tracks2$mode
    tracks_list$time_signature[i] <- tracks2$time_signature
    tracks_list$acousticness[i] <- tracks2$acousticness
    tracks_list$danceability[i] <- tracks2$danceability
    tracks_list$energy[i] <- tracks2$energy
    tracks_list$instrumentalness[i] <- tracks2$instrumentalness
    tracks_list$liveliness[i] <- tracks2$liveness
    tracks_list$loudness[i] <- tracks2$loudness
    tracks_list$speechiness[i] <- tracks2$speechiness
    tracks_list$valence[i] <- tracks2$valence
    tracks_list$tempo[i] <- tracks2$tempo
  }
  
  
  # Return Data
  return(list(
    "albums"=albums_list,
    "tracks"=tracks_list
  ))
  
}
