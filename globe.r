if (!require("twitteR")) {
  install.packages("twitteR")
  library(twitteR)
}
if (!require("ggmap")) {
  install.packages("ggmap")
  library(ggmap)
}
if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}
if (!require("plyr")) {
  install.packages("plyr")
  library(plyr)
}

twitter_consumer_key    <- "75u9BYPYDLP7RhIH64eIHDrsG"
twitter_consumer_secret <- "ZnZ7uN9foHFgRKGp83iWjuZ9qd3mEez7tb8BV6n2XGIgJyT9Gp"
twitter_access_token    <- "2787301826-l7ByAFD7KN53kgJdYfM485p0Ek2jwIqHmJEcVIp"
twitter_access_secret   <- "ucRJrxf6Zg0gIl9C8KBmmh27NwJJX1mRZiRbFrczDjmI2"

options(httr_oauth_cache = TRUE)
setup_twitter_oauth(twitter_consumer_key, twitter_consumer_secret, twitter_access_token, twitter_access_secret)

search_hash <- "#DoctorWho"

lookup_chunkSize <- 20

tweets <- suppressWarnings(searchTwitter(search_hash, n = 200, retryOnRateLimit = 250))

tweets_userNames <- sapply(tweets, function(t) {
  t$screenName
})

tweets_userNames <- unique(tweets_userNames)
tweets_userNames_splitted <- split(tweets_userNames, ceiling(seq_along(tweets_userNames)/lookup_chunkSize))
user_locs <- sapply(unlist(lapply(tweets_userNames_splitted, function (u) lookupUsers(u))), function (u) u$location)
user_locs <- user_locs[user_locs != ""]

user_locs_geo <- lapply(user_locs, function(x) geocode(x, source = "google"))

tweets <- NULL