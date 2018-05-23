## LoadTweets.R
#' This script is intended to load a data frame of tweets from an
#' SQLite database generated with the script SearchAndStoreTweets.R

rm(list=ls())

# load packages
require(twitteR)
require(lubridate)
require(ggmap)
require(stringr)
require(maptools)
require(DBI)
require(ROAuth)
require(dplyr)

# output directory: this is where the SQLite database is
out.dir <- "C:/Users/Sam/Dropbox/Work/Twitter/EcohydroStream/"

# path to database
path.out <- paste0(out.dir, "rTweetsOut.sqlite")

# connect to database
db <- dbConnect(RSQLite::SQLite(), path.out)

# read in table
df <- dbReadTable(db, "tweets")

# trim to unique and rewrite
df <- unique(df)
dbWriteTable(db, "tweets", df, overwrite=T)

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)

# plot of tweets by day
df$created_at <- ymd_hms(df$created_at)
df$DOY <- yday(df$created_at)
df$Date <- as.Date(df$created_at, tz=Sys.timezone(location = TRUE))
df.d <- summarize(group_by(df, Date),
                  tweets = sum(is.finite(lat.location)))

# list of missing days
missing <- seq(df.d$Date[1], Sys.Date()-1, by="day")[!(seq(df.d$Date[1], Sys.Date()-1, by="day") %in% df.d$Date)]
print(missing)
#print(yday(missing))
#print(week(missing))

# print most recent tweet
print(paste0("Last tweet: ", df$created_at[which.max(df$status_id)]))

# bar plot: tweets by day
p.bar.tweets.DOY <-
  ggplot(df.d, aes(x=Date, y=tweets)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0, color="gray65") +
  labs(title=paste0(sum(df.d$tweets), " Ecohydro Tweets")) +
  scale_y_continuous(name="# Tweets") +
  theme_bw() +
  theme(panel.grid=element_blank())
p.bar.tweets.DOY
