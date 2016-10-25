setwd("/Users/BK/Documents/GitHub/KreisWk3")
library(plyr)
Bridges <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/bridges/bridges.data.version1", header = FALSE)
write.table(Bridges, file = "Bridges.csv", sep = ",")

head(Bridges)

names(Bridges) <- c("ID", "RIVER", "LOCATION", "ERECTED", "PURPOSE", "LENGTH", "LANES", "CLEAR-G", "T-OR-D", "MATERIAL", "SPAN", "REL-L", "TYPE")

head(Bridges)
