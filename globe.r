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
if (!require("stringr")) {
  install.packages("stringr")
  library(stringr)
}

twitter_consumer_key    <- "75u9BYPYDLP7RhIH64eIHDrsG"
twitter_consumer_secret <- "ZnZ7uN9foHFgRKGp83iWjuZ9qd3mEez7tb8BV6n2XGIgJyT9Gp"
twitter_access_token    <- "2787301826-l7ByAFD7KN53kgJdYfM485p0Ek2jwIqHmJEcVIp"
twitter_access_secret   <- "ucRJrxf6Zg0gIl9C8KBmmh27NwJJX1mRZiRbFrczDjmI2"

options(httr_oauth_cache = TRUE)
setup_twitter_oauth(twitter_consumer_key, twitter_consumer_secret, twitter_access_token, twitter_access_secret)

lookup_chunkSize <- 20

tweets <- suppressWarnings(searchTwitter(search_hash, n = 30, retryOnRateLimit = 250))

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
    geo <- geocode(loc, output = "all", source = "google")
    while (geo$status == "OVER_QUERY_LIMIT") {
      print("OVER QUERY LIMIT") 
      print(as.character(Sys.time()))
      Sys.sleep(60*60)
      geo <- geocode(loc, output = "all", source = "google")
    }
    if (length(geo$results) == 1) {
      if (!is.na(geo$results[[1]]$geometry$location$lng) && !is.na(geo$results[[1]]$geometry$location$lat)) {
        user_locs_geo_list$lon <- append(user_locs_geo_list$lon, geo$results[[1]]$geometry$location$lng)
        user_locs_geo_list$lat <- append(user_locs_geo_list$lat, geo$results[[1]]$geometry$location$lat)
        user_locs_geo_list$formatted <- append(user_locs_geo_list$formatted, geo$results[[1]]$formatted_address)
        user_locs_geo_list$raw <- append(user_locs_geo_list$raw, loc)
      } else {
        blacklist <- append(blacklist, loc)
      }
    } else {
      blacklist <- append(blacklist, loc)
    }
  } else if(!(loc %in% blacklist)) {
    geoindex <- which(user_locs_geo_list$raw == loc)
    user_locs_geo_list[[1]] <- append(user_locs_geo_list[[1]], user_locs_geo_list[[1]][[geoindex]])
    user_locs_geo_list[[2]] <- append(user_locs_geo_list[[2]], user_locs_geo_list[[2]][[geoindex]])
    user_locs_geo_list[[3]] <- append(user_locs_geo_list[[3]], user_locs_geo_list[[3]][[geoindex]])
    user_locs_geo_list[[4]] <- append(user_locs_geo_list[[4]], user_locs_geo_list[[4]][[geoindex]])
  }
}

countriescount <- list()
for (country in user_locs_geo_list$formatted) {
  country <- str_extract(country, "[a-zA-Z][a-zA-Z ]*$")
  if (country %in% names(countriescount)) {
    countriescount[[country]] <- countriescount[[country]] + 1 
  } else {
    countriescount[[country]] <- 1
  }
}
write.table(countriescount, file = paste0("TBL/", as.character(length(user_locs_geo_list[[1]])),search_hash,"-countrycount.tbl"))

cairo_pdf(paste0("PDF2/", as.character(length(user_locs_geo_list[[1]])),search_hash,".pdf"), width = 20, height = 10)
plot <- ggplot(as.data.frame(user_locs_geo_list), aes(lon, lat))
plot <- plot + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank())
plot <- plot + geom_point(color = "red", alpha = 0.05, stroke = 0, size = 5)
plot <- plot + borders(colour = "black", size = 0.15)
dev.off()

geocodeQueryCheck(userType = "free")
tweets <- NULL