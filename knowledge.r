if (!require("twitteR")) {
  install.packages("twitteR")
  library(twitteR)
}
if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}
if (!require("plyr")) {
  install.packages("plyr")
  library(plyr)
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

buzzWords <- list(c("#Pop"), 
                  c("#Kpop"), 
                  c("#Jpop"), 
                  c("#HipHop", "#Hip Hop", "#Rap"), 
                  c("#Rock", "#Metal"),
                  c("#EDM", "#Techno", "#Hardstyle", "#Dubstep"),
                  c("#Punk"))

date <- as.Date("2016-06-16")
time <- Sys.time()

getRelatedHashs <- function(buzzWords) {
  lrelated <- list()
  lrelated <- lapply(buzzWords, function(x) {
    lrel_syn <- lapply(x[1:length(x)], function (y) {
      while (TRUE) {
        tweets <- try(suppressWarnings(searchTwitter(y, n = 10000, retryOnRateLimit = 250)))
        if (!is(tweets, 'try-error'))
          break
        else
          print("retry")
      }
      sapply(tweets, function(z) {
        str_extract_all(z$text, "(?:(#\\w+).*?)+")
        }, simplify = F)
    })
    names(lrel_syn) <- x[1:length(x)]
    lrel_syn
  })
  names(lrelated) <- lapply(buzzWords, function (z) {z[[1]][[1]]})
  lrelated
}

relatedHashs <- getRelatedHashs(buzzWords)

relatedHashsFlatten <- lapply(relatedHashs, function(x) {
  lapply(x, function(y){ 
    y <- unlist(y)
    lapply(y, function(z){
      if(length(z) > 0)
        z
    })
  })
})

countHashs <- list()
countHashs <- sapply(names(relatedHashsFlatten), function (relatedHashsFlatten_names) {
  sapply(relatedHashsFlatten[[relatedHashsFlatten_names]], function(x) {
    countHashs[[relatedHashsFlatten_names]] <- sapply(x, function(y) {
      tolower(y)
    })
  }, simplify = F)
})

countedSum <- list()

for (i in names(countHashs)) {
  countedSum[[i]] <- list()
  for (j in countHashs[[i]]) {
    tmp <- unique(j)
    for (k in tmp) {
      countedSum[[i]][[k]] <- 0
    }
  }
}

names(countedSum) <- names(countHashs)

for (i in names(countHashs)) {
  for (j in countHashs[[i]]) {
    for (k in j) {
      countedSum[[i]][[k]] <- countedSum[[i]][[k]] + 1
    }
  }
}

countedSumSorted <- lapply(countedSum, function(x) {sort(unlist(x), decreasing = T)})
countedSumSorted_df <- lapply(countedSumSorted, function(x) {data.frame(count = x)})

pdf.options(encoding = 'UTF-8')
for (i in names(countedSumSorted_df)) {
  CairoPDF(paste0("PDF/", i, format(time, "%m%d_%H%M"),"_hashtag_plots_new.pdf"), width = 20, height = 10, family = names(pdfFonts())[25])
  par(mar = c(16,4,2,4) + 0.1)
  
    plot <- barplot(
        head(countedSumSorted_df[[i]][['count']], n = 100), 
        names.arg = head(rownames(countedSumSorted_df[[i]]), n = 100), 
        las = 2, 
        space = F, 
        ylim = c(0, countedSumSorted_df[[i]][['count']][[1]] + 1000),
        main = i)
    text(x = plot, y = head(countedSumSorted_df[[i]][['count']] + 350, n = 100), 
         label = head(countedSumSorted_df[[i]][['count']], n = 100), 
         pos = NULL, cex = 0.75, col = "grey", srt = 90)
    dev.off()
}

rm(list=c("buzzWords", "countedSum", "countedSumSorted", "countHashs", "date", "i", "j", "k", "relatedHashs", "relatedHashsFlatten", "tmp"))
save.image(file = paste0("Env/", format(time, "%m%d_%H%M"), ".RData"))
