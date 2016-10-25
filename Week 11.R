url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data"
spambase <- read.csv(url, stringsAsFactors = F, header = F)
write.csv(spambase, file = "spambase.csv", row.names = F)
spambase <- read.csv("F:/Data/Week 11 HW/spambase.csv", header=TRUE)

library(RTextTools)
library(tm)
library(RWeka)
library(SnowballC)


spambase <- spambase[sample(nrow(spambase)),]
spam_labels <- unlist(spambase$V58)
spam_labels
N <- length(spam_labels)

container <- create_container(spambase, labels = spam_labels, trainSize = 1:1000, testSize = 1001:N, virgin = FALSE)

slotNames(container)

svm_model <- train_model(container, "SVM")
tree_model <- train_model(container, "TREE")
maxent_model <- train_model(container, "MAXENT")

svm_out <- classify_model(container, svm_model)
tree_out <- classify_model(container, tree_model)
maxent_out <- classify_model(container, maxent_model)


labels_out <- data.frame(
  correct_label = spam_labels[1001:N],
  svm = maxent_out[,1],
  tree = tree_out[,1],
  maxent = maxent_out[,1],
  stringsAsFactors = F)

##SVM Performance
table(labels_out[,1] == labels_out[,2])
prop.table(table(labels_out[,1] == labels_out[,2]))


##Random Forest Performance
table(labels_out[,1] == labels_out[,3])
prop.table(table(labels_out[,1] == labels_out[,3]))

##Maximum Entropy Performance
table(labels_out[,1] == labels_out[,4])
prop.table(table(labels_out[,1] == labels_out[,4]))



