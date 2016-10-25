setwd("/Users/BK/Documents/GitHub/KreisWk3")

Bridges <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/bridges/bridges.data.version1", header = FALSE)
write.table(Bridges, file = "Bridges.csv", sep = ",")

names(Bridges) <- c("ID", "RIVER", "LOCATION", "ERECTED", "PURPOSE", "LENGTH", "LANES", "CLEAR-G", "T-OR-D", "MATERIAL", "SPAN", "REL-L", "TYPE")
is.factor(Bridges$LANES)
Bridges$LANES <- as.numeric(Bridges$LANES)

dim(Bridges)
head(Bridges)

df <- data.frame(Bridges$ID, Bridges$RIVER, Bridges$LANES, Bridges$LENGTH)

x <- subset(df, Bridges$LANES >= 4)
x
