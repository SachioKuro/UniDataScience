#need Arg 1 = search_hash (string)

#need following structures
#user_locs_geo_list <- list()
#blacklist <- list()

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
if (!require("openssl")) {
  install.packages("openssl")
  library(openssl)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

twitter_consumer_key    <- "75u9BYPYDLP7RhIH64eIHDrsG"
twitter_consumer_secret <- "ZnZ7uN9foHFgRKGp83iWjuZ9qd3mEez7tb8BV6n2XGIgJyT9Gp"
twitter_access_token    <- "2787301826-l7ByAFD7KN53kgJdYfM485p0Ek2jwIqHmJEcVIp"
twitter_access_secret   <- "ucRJrxf6Zg0gIl9C8KBmmh27NwJJX1mRZiRbFrczDjmI2"

options(httr_oauth_cache = TRUE)
setup_twitter_oauth(twitter_consumer_key, twitter_consumer_secret, twitter_access_token, twitter_access_secret)

lookup_chunkSize <- 20

tweets <- suppressWarnings(searchTwitter(search_hash, n = 10000, retryOnRateLimit = 250))

tweets_userNames <- sapply(tweets, function(t) {
  t$screenName
})

tweets_userNames <- unique(tweets_userNames)
tweets_userNames_splitted <- split(tweets_userNames, ceiling(seq_along(tweets_userNames)/lookup_chunkSize))

user_locs <- sapply(unlist(lapply(tweets_userNames_splitted, function (u) lookupUsers(u))), function (u) u$location)
user_locs <- user_locs[user_locs != ""]
names(user_locs) <- md5(names(user_locs))

for(loc in user_locs) {
  if (!(loc %in% user_locs_geo_list$raw) && !(loc %in% blacklist)) {
    geo <- as.list(geocode(loc, output = "latlona", source = "google"))
    if (!is.na(geo$lon) && !is.na(geo$lat)) {
      user_locs_geo_list$lon <- append(user_locs_geo_list$lon, geo$lon)
      user_locs_geo_list$lat <- append(user_locs_geo_list$lat, geo$lat)
      user_locs_geo_list$formated <- append(user_locs_geo_list$formated, geo$address)
      user_locs_geo_list$raw <- append(user_locs_geo_list$raw, loc)
    } else {
      blacklist <- append(blacklist, loc)
    }
  }
}

plot <- ggplot(as.data.frame(user_locs_geo_list), aes(lon, lat))
plot <- plot + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank())
plot <- plot + geom_point(color = "red", alpha = 0.05, stroke = 0, size = 5)
plot <- plot + borders(colour = "black", size = 0.15)
cairo_pdf(paste0("PDF/", as.character(length(user_locs_geo_list[[1]])),search_hash,".pdf"), width = 20, height = 10)
plot
dev.off()

geocodeQueryCheck(userType = "free")
tweets <- NULL