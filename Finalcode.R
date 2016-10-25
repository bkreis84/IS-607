library(XML)
library(RCurl)
library(tidyr)
library(dplyr)
library(stringr)

url <- "https://en.wikipedia.org/wiki/List_of_Major_League_Baseball_postseason_teams"
url_source <- readLines(url, encoding = "UTF-8")
playoffs <- data.frame(readHTMLTable(url_source, stringsAsFactors = F, header = T)[2])

# column name change
names(playoffs) <- c("abbr","team","est","appearances")

# Here I had some issues because in the html code where there was a rowspan, all the 
# data shifted to the wrong column.
# I was unsure how to proceed, and wanted to use a mehtod other than regular expressions
# so I got some help on stack overflow (first time asking a question) and was impressed
# with the quality and speed of the feedback. I learned a lot just going through the 
# various methods presented. I decided to go with the one that was most efficient and
# it introduced me to the data.table package.

# Essentially this code performs a test to see if there are more than 3
# characters and if there are, it shifts the values of the columns based on references
# leaving an NA value in the abbr column. 


library(data.table)
library(zoo)

setDT(playoffs)[nchar(abbr) > 3, `:=` (abbr = NA,
                                            team = abbr,
                                            est = team,
                                            appearances = est)
                ]
#  fill the resulting NA's with the last observation, and we have the data extracted
playoffs <- playoffs %>% 
  fill(abbr)

#create a list of playoff appearances
split <- str_split(playoffs$appearances, ", ")


#had some issues here putting the data into a data frame when the lists vary in size
#this code got me on track
max <- max(sapply(split,length))
out <- do.call(rbind, lapply(split,function(x) x[1:max]))

#add an ID to both dataframes
playoffs2 <- as.data.frame(out) %>%
  mutate(ID = 1:41)
  
playoffs <- playoffs %>% 
  select(abbr, team, est) %>% 
  mutate(ID = 1:41)
 
#merge them together using ID
combined <- merge(playoffs, playoffs2, by = 'ID', all.x = T)

#Use the gather function to tidy up the data, get rid of the arbitrary n, sort,
#and get rid of rows with NA
ptidy <- gather(combined, "n", "year", 5:56) %>% 
  select(-n) %>% 
  arrange(abbr) %>% 
  na.omit() 

#remove any * values which indicate that the team lost in the wild card game
ptidy$year <- gsub("\\*", "", ptidy$year)  

#Filter the data so that it is more recent 
ptidy2 <- ptidy %>% 
  select(-team, -est, -ID) %>% 
  mutate(playoff = 1) %>% 
  filter(year >= 1950)


#Bring in raw data for team stats
url <- "https://raw.githubusercontent.com/bkreis84/FINAL/master/teams.csv"
teamdata <- read.csv(url, na.strings = "")

#bring in column header csv and add apply them to the dataframe
namesgit <- "https://raw.githubusercontent.com/bkreis84/FINAL/master/names2.csv"
names <- read.csv(namesgit, header = F, stringsAsFactors = F)

names(teamdata) <- c(names)

#Convert factors to numeric values using a function
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
teamdata[, c("SO","SB","CS","HBP","SF","DP")] <- 
  sapply(teamdata[, c("SO","SB","CS","HBP","SF","DP")], as.numeric.factor)

#Make it possible to add the WS win value by converting it to a number
teamdata$WSWin <- str_replace_all(teamdata$WSWin, "Y", "1")
teamdata$WSWin <- str_replace_all(teamdata$WSWin, "N", "0")
teamdata$WSWin <- as.integer(teamdata$WSWin)

#Data dates back to the 1870s, we will look at more recent info though.
teamdata <- teamdata %>% 
  filter(yearID >= 1950) %>% 
  arrange(franchID) %>% 
  select(-ID,-teamID, -divID:-Ghome, -DivWin:-LgWin, -name:-teamIDretro)

#need to match up the franchise IDs which are different from the two sources.
puniq <- sort(unique(ptidy$abbr))
tuniq <- sort(unique(teamdata$franchID))
unique <- data.frame(puniq,tuniq)
View(unique)

#My main ID will be the abbreviations so I needed to make them uniform.
teamdata$franchID <- str_replace_all(teamdata$franchID, "(ANA)", replacement = "LAA")
teamdata$franchID <- str_replace_all(teamdata$franchID, "(CHW)", replacement = "CWS")
teamdata$franchID <- str_replace_all(teamdata$franchID, "(FLA)", replacement = "MIA")
teamdata$franchID <- str_replace_all(teamdata$franchID, "(KCR)", replacement = "KC")
teamdata$franchID <- str_replace_all(teamdata$franchID, "(SDP)", replacement = "SD")
teamdata$franchID <- str_replace_all(teamdata$franchID, "(SFG)", replacement = "SF")
teamdata$franchID <- str_replace_all(teamdata$franchID, "(TBD)", replacement = "TB")
teamdata$franchID <- str_replace_all(teamdata$franchID, "(WSN)", replacement = "WSH")

teamdata$yearID <- as.character(teamdata$yearID)

#Merge using 2 variables
new <- merge(teamdata, ptidy2, by.x=c("franchID", "yearID"), 
             by.y=c("abbr", "year"), all.x = T)


salurl <- "https://raw.githubusercontent.com/bkreis84/FINAL/master/salaries.csv"
salaries <- read.csv(salurl, stringsAsFactors = F)

names(salaries) <- c("id", "year", "abbr", "league", "player", "salary")

nuniq <- sort(unique(new$franchID))
suniq <- sort(unique(salaries$abbr))
compare <- data.frame(cbind(nuniq,suniq))

#My main ID will be the abbreviations so I needed to make them uniform.
salaries$abbr <- str_replace_all(salaries$abbr, "(ANA)", replacement = "LAA")
salaries$abbr <- str_replace_all(salaries$abbr, "(CHA)", replacement = "CWS")
salaries$abbr <- str_replace_all(salaries$abbr, "(FLO)", replacement = "MIA")
salaries$abbr <- str_replace_all(salaries$abbr, "(KCA)", replacement = "KC")
salaries$abbr <- str_replace_all(salaries$abbr, "(SDN)", replacement = "SD")
salaries$abbr <- str_replace_all(salaries$abbr, "(SFN)", replacement = "SF")
salaries$abbr <- str_replace_all(salaries$abbr, "(TBA)", replacement = "TB")
salaries$abbr <- str_replace_all(salaries$abbr, "(WAS)", replacement = "WSH")
salaries$abbr <- str_replace_all(salaries$abbr, "(CHN)", replacement = "CHC")
salaries$abbr <- str_replace_all(salaries$abbr, "(LAN)", replacement = "LAD")
salaries$abbr <- str_replace_all(salaries$abbr, "(FLO)", replacement = "MIA")
salaries$abbr <- str_replace_all(salaries$abbr, "(NYN)", replacement = "NYM")
salaries$abbr <- str_replace_all(salaries$abbr, "(NYA)", replacement = "NYY")
salaries$abbr <- str_replace_all(salaries$abbr, "(SFN)", replacement = "SF")
salaries$abbr <- str_replace_all(salaries$abbr, "(SLN)", replacement = "STL")
salaries$abbr <- str_replace_all(salaries$abbr, "(ML4)", replacement = "MIL")
salaries$abbr <- str_replace_all(salaries$abbr, "(MON)", replacement = "WSH")
salaries$abbr <- str_replace_all(salaries$abbr, "(CAL)", replacement = "LAA")

#Because the salary data is listed by player, we need to obtain team totals
salaries2 <- salaries %>% 
  group_by(abbr, year) %>% 
  summarise(sum = sum(salary))

#Time to merge in the salary data
#There will be a number of NA values since our salary data begins with 1980.
all <- merge(new, salaries2, by.x = c("yearID", "franchID"),
               by.y = c("year", "abbr"), all.x = T)

all$playoff[is.na(all$playoff)] <- 0

summary(all)


#Add in some commonly used stats. BABIP is batting average on balls in play. 
#As far as team stats go, a high number is considered lucky 
all <- all %>% 
  mutate(winperc = W/(W + L)) %>% 
  mutate(BABIP = (H-HR)/(AB-SO-HR+SF)) %>% 
  mutate(OBP = (H + BB + HBP)/(AB + BB + SF + HBP)) %>% 
  mutate(SLG = (H - `2B` - `3B` - HR + `2B` * 2 + `3B` * 3 + HR * 4)/ AB) %>% 
  mutate(OPS = OBP + SLG) %>% 
  mutate(TB = (H - `2B` - `3B` - HR + `2B` * 2 + `3B` * 3 + HR * 4))

#BsR is a stat that predicts the number of runs a team "should" have scored
#based on the types of hits and number of walks.
all <- all %>% 
  mutate(BsR = (((H + BB - HR) * ((1.4*TB - .6*H -3*HR +0.1*BB)*1.02)) /
           (((1.4*TB - .6*H -3*HR +.1*BB)*1.02) + AB - H)) + HR)

runs <- all %>% 
  select(R, BsR) %>% 
  mutate (dif = R - BsR)

#The BsR is extremely close when look at the combined historical stats with a mean 
#difference between actual of -0.5 since 1950. 
summary(runs$dif)


library(ggplot2)     

#After running the following ggplot it becomes clear that there are certain outliers
#These are years that the league was on strike. We will remove these years
ggplot(all, aes(x=H, y=winperc, color=playoff)) + geom_point()

final <- all %>% 
  filter(yearID != 1972, yearID != 1981, yearID != 1994)

#New data set for salary info since the salary data wasn't available in the set until
#after 1985
salary <- final %>% 
  filter(yearID >= 1985)

#Take a look at Home runs per AB since 1950. 
year.totals <- final %>% 
  group_by(yearID) %>% 
  summarise(HR = sum(HR), AB = sum(AB))

year.totals <- year.totals %>% 
  mutate(HRperAB = HR/AB)

ggplot(year.totals, aes(x=yearID, y=HRperAB)) + geom_point()

# Will include strike shortened seasons for exploratory analysis 
team.counts <- all %>% 
  group_by(franchID) %>% 
  summarise(playoff_appearances = sum(playoff, na.rm=T), WSwin = sum(WSWin, na.rm=T))

ggplot(data=team.counts, 
       aes(x=franchID, y=playoff_appearances, label = playoff_appearances)) + geom_bar(
         stat = "identity") + coord_flip() + geom_text(color="green", size = 7)

#HR
final2<-final
final2$playoff <- as.factor(final2$playoff)

ggplot(final2, aes(x=HR, y=winperc)) + geom_point() + geom_smooth()
cor(final2$HR, final2$winperc)


cortests <- final2 %>% 
  select(winperc, R, H, HR, SB, ERA, SV, FP, BABIP:BsR) %>% 
  na.omit()

list <- list()
list[[1]] <- cortests
corframe <- data.frame(lapply(list,cor))
corframe <- corframe[1,]
corframe <- corframe[,-1]

#ERA
ggplot(final2, aes(x=ERA, y=winperc)) + geom_point() + geom_smooth()
cor(final$ERA, final$winperc)

#Interestingly salary had a lower correlation than the stats above
ggplot(salary, aes(x=sum, y=winperc, color=playoff)) + geom_point() + geom_smooth()
cor(salary$sum, salary$winperc)


ggplot(final, aes(x=H, y=winperc, color=playoff)) + geom_point() + geom_smooth()



#Multiple Regression
mfit = lm(winperc ~ OBP + OPS + ERA + HR + FP + SV + SB, final)
summary(mfit)

mco = coef(summary(mfit))
mco

                        