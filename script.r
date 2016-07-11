if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}
if (!require("plyr")) {
  install.packages("plyr")
  library(plyr)
}

collected <- list()
tags <- c("#Pop", "#Kpop", "#Jpop", "#HipHop", "#Rock", "#EDM", "#Punk")
i <- 0
for (data in list.files("Env")) {
  i <- i + 1
  load(paste0("Env/", data))
  print(paste("Data loaded", toString(i)))
  for (tag in tags) {
    print(tag)
    collected[[tag]] <- rbind.fill(collected[[tag]], as.data.frame(t(countedSumSorted_df[[tag]])))
  }
}

collected <- lapply(collected, t)
collected_sums <- lapply(collected, function(x)  {cbind(x, rowSums(x, na.rm = T))})
collected_sums_pop <- collected_sums$`#Pop`[,27]
collected_sums_pop <- sort(collected_sums_pop, decreasing = T)

collected_sums_jpop <- collected_sums$`#Jpop`[,27]
collected_sums_jpop <- sort(collected_sums_jpop, decreasing = T)

collected_sums_kpop <- collected_sums$`#Kpop`[,27]
collected_sums_kpop <- sort(collected_sums_kpop, decreasing = T)

collected_sums_rock <- collected_sums$`#Rock`[,27]
collected_sums_rock <- sort(collected_sums_rock, decreasing = T)

collected_sums_edm <- collected_sums$`#EDM`[,27]
collected_sums_edm <- sort(collected_sums_edm, decreasing = T)

collected_sums_hiphop <- collected_sums$`#HipHop`[,27]
collected_sums_hiphop <- sort(collected_sums_hiphop, decreasing = T)

collected_sums_punk <- collected_sums$`#Punk`[,27]
collected_sums_punk <- sort(collected_sums_punk, decreasing = T)

png("cluster_jpop.png", width = 1920, height = 1080)
par(mar = c(16,4,4,4) + 0.1)

plot <- barplot(
  head(collected_sums_kpop, n = 100), 
  names.arg = head(names(collected_sums_kpop), n = 100), 
  cex.names = 1.5,
  las = 2, 
  space = F, 
  ylim = c(0, collected_sums_kpop[[1]] + 50000))
text(x = plot, y = head(collected_sums_kpop + 10000, n = 100), 
     label = head(collected_sums_kpop, n = 100), 
     pos = NULL, cex = 1.2, col = "black", srt = 90)
dev.off()