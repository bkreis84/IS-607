library (stringr)

tournamentinfo <- read.csv("F:/Data/Project 1/tournamentinfo.txt")
View(tournamentinfo)
names(tournamentinfo) <- c("x")

name <- unlist(str_extract_all(tournamentinfo$x, "[[:alpha:]]{2,}.+[[:alpha:]]{2,}"))
name
name <- name[-(1:2)]
name

state <- unlist(str_extract_all(tournamentinfo$x, "( [[:alpha:]]{2,2} \\|)"))
state
state <- unlist(str_extract_all(state, "[[:alpha:]]{2}"))
state

points <- unlist(str_extract_all(tournamentinfo$x, "(.\\..)"))
points

prescore <- unlist(str_extract_all(tournamentinfo$x, ":.*[[:digit:]]{3,4}.*->"))
prescore
prescore <- unlist(str_extract_all(prescore, "[[:digit:]]{3,4}"))
prescore <- as.numeric(prescore)

id <- as.factor(1:64)

predf <- data.frame(id, prescore)

rounds <- unlist(str_extract_all(tournamentinfo$x, "(.\\..).* *\\d{1,2}"))
rounds
rounds <- str_replace_all(rounds, "W|L|D|H| ", replacement = "")
rounds

roundsdf <- data.frame(rounds)
roundsdf <- data.frame(do.call('rbind', strsplit(as.character(roundsdf$rounds),'|',fixed=TRUE)))
roundsdf

roundsdf$id <- as.factor(1:64)

names(roundsdf) <- c("points", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "id")


roundsdf$r1 <- predf$prescore[match(predf$id, roundsdf$r1)]
roundsdf$r2 <- predf$prescore[match(predf$id, roundsdf$r2)]
roundsdf$r3 <- predf$prescore[match(predf$id, roundsdf$r3)]
roundsdf$r4 <- predf$prescore[match(predf$id, roundsdf$r4)]
roundsdf$r5 <- predf$prescore[match(predf$id, roundsdf$r5)]
roundsdf$r6 <- predf$prescore[match(predf$id, roundsdf$r6)]
roundsdf$r7 <- predf$prescore[match(predf$id, roundsdf$r7)]

roundsdf$avg <- rowMeans(roundsdf[,2:8], na.rm = TRUE)
roundsdf$avg <- round(roundsdf$avg, digits=0) 


finaldf <- data.frame(name, state, points, prescore, roundsdf$avg)
colnames(finaldf)[5] <- "prescoreopp"

write.csv(finaldf, file = "Chess.csv",row.names=FALSE)
