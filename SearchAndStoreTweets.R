## SearchAndStoreTweets.R
#' This script is intended to:
#'  (1) search Twitter for a keyword or set of keywords
#'  (2) download all matching Tweets
#'  (3) extract the location of the tweeter via Google Maps
#'  (4) save the output as a CSV file

rm(list=ls())

# path to git directory
git.dir <- "C:/Users/Sam/WorkGits/EcohydroStream/"

# load packages
require(twitteR)
require(lubridate)
require(ggmap)
require(stringr)
require(maptools)
require(DBI)
require(ROAuth)

# search string: what will you search twitter for?
search.str <- "eco-hydrology OR ecohydrology OR hydroecology OR hydro-ecology OR #hydroecology OR #ecohydrology OR (ecology and (hydrology OR hydrogeology))"

# output directory: save to Dropbox, not git repository, so it's automatically backed up
# this is also where authentication info is stored
#out.dir <- "C:/Users/Sam/Dropbox/Work/Twitter/EcohydroStream/"
out.dir <- "D:/Dropbox/Work/Twitter/EcohydroStream/"

# path to save output data
path.out <- paste0(out.dir, "TweetsOut.sqlite")

# path to save the screen output
path.sink <- paste0(out.dir, "TweetsOut_Screen_", format(Sys.time(), "%Y%m%d-%H%M"), ".txt")

## launch sink file, which will store screen output 
# this is useful when automating, so it can be double-checked later
# to make sure nothing weird happened
s <- file(path.sink, open="wt")
sink(s, type="message")

# relative path to authentication info (this is in .gitignore
# so not shared publicly). these are obtained from twitter/google
# when you create your app.
path.auth.t <- paste0(out.dir, "TwitterAuth.txt")

# read in authentication info - file has three lines of 
# comments followed by:
#   [1] consumer key (API key)
#   [2] consumer secret (API secret)
#   [3] access token
#   [4] access secret
auth.t <- read.table(path.auth.t, skip=3, stringsAsFactors=F)[,1]    # read in as vector

# set up authentication
options(httr_oauth_cache=T)   # this will store authentication as a local file
setup_twitter_oauth(auth.t[1], auth.t[2], auth.t[3], auth.t[4])

# get today/yesterday dates
date_today <- as.Date(Sys.time())
date_yesterday <- date_today-days(1)

# search twitter!
tweets <- searchTwitter(search.str, 
                        n=10000, 
                        resultType="recent",
                        since=as.character(date_yesterday),
                        until=as.character(date_today),
                        retryOnRateLimit=5000)

# get rid of retweets
tweets <- strip_retweets(tweets, strip_manual=T, strip_mt=T)

# put into data frame (only categories we care about)
df <- twListToDF(tweets)[,c("text", "created", "id", "screenName", "isRetweet", "longitude", "latitude")]

# convert text to UTF-8 to deal with weird characters
df$text <- sapply(df$text, function(row) iconv(row, to='UTF-8'))
df$text <- gsub("\n", " ", df$text)

## using Google Maps API, get estimated geographic coordinates based on user location
# limit of 2500/day! so, get clean location as much as possible first to minimize calls to API

# get user location
userInfo <- lookupUsers(df$screenName)
df.users <- twListToDF(userInfo)

# trim to only users with location info
df.users <- df.users[df.users$location != "",]

# replace % in user location with blank so geocode doesn't get messed up
df.users$location <- gsub("%", " ",df.users$location)

# trim leading/trailing white space
df.users$location <- trimws(df.users$location)

# get unique locations
locations <- unique(df.users$location)

# call geocode
geo.out <- geocode(locations, source="google", output="all")

## filter output
if (length(locations)>1){
  # status check: did geocode find a location?
  check.status <- sapply(geo.out, function(x) x["status"]=="OK" & length(x["status"])>0)
  check.status[is.na(check.status)] <- F
  geo.out <- geo.out[check.status]
  locations <- locations[check.status]
  
  # status check: is location ambiguous?
  check.ambig <- sapply(lapply(geo.out, lapply, length), function(x) x["results"]=="1")
  geo.out <- geo.out[check.ambig]
  locations <- locations[check.ambig]
  
  ## make final locations data frame
  df.locations <- data.frame(
    location = locations,
    lat.location = sapply(geo.out, function(x) x["results"]$results[[1]]$geometry$location$lat),
    lon.location = sapply(geo.out, function(x) x["results"]$results[[1]]$geometry$location$lng),
    formatted_address = sapply(geo.out, function(x) x["results"]$results[[1]]$formatted_address)
  )
  
} else {
  # status check: did geocode find a location?
  check.status <- geo.out[[2]]=="OK"
  check.status[is.na(check.status)] <- F
  if (!check.status){
    geo.out <- NULL
    locations <- NULL
  }
  
  # status check: is location ambiguous?
  check.ambig <- lapply(geo.out, lapply, length)$results>=1
  if (!check.ambig){
    geo.out <- NULL
    locations <- NULL
  }
  
  ## make final locations data frame
  df.locations <- data.frame(
    location = locations,
    lat.location = geo.out$results[[1]]$geometry$location$lat,
    lon.location = geo.out$results[[1]]$geometry$location$lng,
    formatted_address = geo.out$results[[1]]$formatted_address
  )
}

# add location info back to user data frame
df.users <- merge(df.users[c("location", "description", "screenName")], df.locations, by="location", all.x=T)

# make output data frame including tweet, user, location, etc.
df.out <- merge(df, df.users, by="screenName", all.x=T)

# get rid of commas
df.out$text <- gsub(",", " ", df.out$text)
df.out$location <- gsub(",", " ", df.out$location)
df.out$description <- gsub(",", " ", df.out$description)

# get rid of line breaks
df.out$text <- gsub("\n", " ", df.out$text)
df.out$location <- gsub("\n", " ", df.out$location)
df.out$description <- gsub("\n", " ", df.out$description)

# get rid of URLs
removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)
df.out$text <- unlist(lapply(df.out$text, removeURL))

# trim leading/trailing white space
df.out$text <- trimws(df.out$text)

# get rid of duplicate tweets
df.out <- df.out[!duplicated(df.out[c("screenName", "text")]), ]

# put in order
df.out <- df.out[order(df.out$id), ]

# convert dates to character string for database
df.out$created <- as.character(df.out$created)

## put into database
# create/connect to database
db <- dbConnect(RSQLite::SQLite(), path.out)

# add data frame to database (if it doesn't exist, it will be created)
dbWriteTable(db, "tweets", df.out, append=T)

# if you want to read in a data frame from your db to check...
#df.test <- dbReadTable(db, "tweets")

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)

# print status update
print(paste0(dim(df.out)[1], " tweets added to database"))

# close sink
close(s)
sink()
sink(type="message")
close(s)

# # make a plot
# state_map <- map_data("state")
# p.map <-
#   ggplot(data=df.out, aes(x=lon.location, y=lat.location)) +
#   geom_path(data=state_map, color="blue", aes(x=long, y=lat, group=factor(region))) +
#   geom_point(shape=21) +
#   coord_map() +
#   theme_bw() +
#   theme(panel.grid=element_blank())
