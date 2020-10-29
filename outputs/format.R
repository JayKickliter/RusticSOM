library(rjson)
library(foreach)
library(iterators)
library(rlist)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)

result_usup  <- fromJSON(file = "output_unsupervised.json")

result_sup <- fromJSON(file = "output_supervised.json")

print(result_usup)

print(result_sup)

result_map <- data.table(map.data = result_sup$map$data)

result_wins <- data.table(map.wins = result_sup$tag_activation_map$data)

uresult_map <- data.table(map.data = result_usup$map$data)

slist <- seq(0, 297, by=3)

mapdata.rssi <- foreach(i=iter(slist)) %do% {
  x <- i+1
  output.mapdata <- result_map$map.data[x]
  output <- output.mapdata * (0 - (-134)) + (-134)
}

mapdata.snr <- foreach(i=iter(slist)) %do% {
  y <- i+2
  output.mapdata <- result_map$map.data[y]
  output <- output.mapdata * (17 - (-19)) + (-19)
}

mapdata.fspl <- foreach(i=iter(slist)) %do% {
  z <- i+3
  output.mapdata <- result_map$map.data[z]
  output <- output.mapdata * (0 - (-164)) + (-164)
}

umapdata.rssi <- foreach(i=iter(slist)) %do% {
  x <- i+1
  output.mapdata <- uresult_map$map.data[x]
  output <- output.mapdata * (0 - (-134)) + (-134)
}

umapdata.snr <- foreach(i=iter(slist)) %do% {
  y <- i+2
  output.mapdata <- uresult_map$map.data[y]
  output <- output.mapdata * (17 - (-19)) + (-19)
}

umapdata.fspl <- foreach(i=iter(slist)) %do% {
  z <- i+3
  output.mapdata <- uresult_map$map.data[z]
  output <- output.mapdata * (0 - (-164)) + (-164)
}

tlist <- seq(0, 198, by=2)

tagwins.real  <- foreach(i=iter(tlist)) %do% {
  x <- i+1
  output.tagwins.real <- result_wins$map.wins[x]

}

tagwins.fake  <- foreach(i=iter(tlist)) %do% {
  y <- i+2
  output.tagwins.fake <- result_wins$map.wins[y]
}

clmn <- c(rep(1, 10), rep(2, 10), rep(3, 10),
                      rep(4, 10), rep(5, 10), rep(6, 10),
                      rep(7, 10), rep(8, 10), rep(9, 10), rep(10, 10))
r <- rep(seq(1, 10, by = 1), 10)

per <- foreach(i=iter(seq(1, 100, by=1))) %do% {
  tr <- tagwins.real[[i]]
  res <- tr/result_sup$tag_activation_map_intermed$data[i]*100
}

per <- unlist(per, recursive = FALSE)

results2 <- data.table(
                     column = clmn,
                     row = r,
                     rssi = umapdata.rssi,
                     snr = umapdata.snr,
                     fspl = umapdata.fspl,
                     activation.map = result_usup$activation_map$data,
                     tag.map = result_usup$tag_map$data
)

results <- data.table(
                     column = clmn,
                     row = r,
                     rssi = mapdata.rssi,
                     snr = mapdata.snr,
                     fspl = mapdata.fspl,
                     tag.map = result_sup$tag_map$data,
                     tag.wins.real = tagwins.real,
                     tag.wins.fake = tagwins.fake,
                     activation.map = per,
                     total.wins = result_sup$tag_activation_map_intermed$data
)

hm10 <- function(res, name) {
  r <- range(res$activation.map)
  plot1 <- ggplot(data = res, aes(x = column, y = row, fill = activation.map)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "red", high = "green", mid = "white",
                               midpoint = 61, limit = c(r[1], r[2]), space = "Lab",
                               name = "total wins") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                        size = 12, hjust = 1)) +
    geom_text(aes(label = tag.map), size = 2) +
    coord_fixed()

  pdf(name)
  print(plot1)
  dev.off()
}

hm10(results, "som10_supervised.pdf")
hm10(results2, "som10_unsupervised.pdf")
