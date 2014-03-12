rm(list=ls())

data <- read.csv("~/Dropbox/Data/Replication/Ahlquist ISQ 2006 data.csv")
names(data)
head(data)

apply(data, 2, function(x) sum(is.na(x)) / length(x))
