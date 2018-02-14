library(caret)
library(dplyr)
library(rafalib)

# Read raw data
training <- read.csv("data/pml-training.csv")

# Remove columns containing mostly NAs
naCount <- apply(training, 2, function(col) {
  length(col[sapply(col, is.na)])
})
predVars1 <- training[naCount < 19000]

# Generate a ligther sample
predVars2 <- sample_n(predVars1, 500)
qplot(y=predVars2$X)

# Remove columns that won't be used as predictors:
# 1: row number
# 2: user_name
excludeColumns <- c(1, 2,
  grep("classe", names(predVars2)), 
  grep("timestamp", names(predVars2)), 
  grep("window", names(predVars2)), 
  grep("kurtosis", names(predVars2)), 
  grep("skewness", names(predVars2)))
predVars3 <- predVars2[, -excludeColumns]

# Remove remaining factor columns, which are predominantly empty
predVars4 <- predVars3[,lapply(predVars3, class) != "factor"]
