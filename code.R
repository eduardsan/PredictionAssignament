# load libraries
library(caret)
library(dplyr)
library(rafalib)

# source support code
source("cleanUpInput.R")
source("processCorMatrix.R")

# ensure results are repeatable
set.seed(123)

# Read data
training <- read.csv("data/pml-training.csv")

# Tidy data
clean <- cleanUpInput(training)

# Use correlation matrix to select variables
correlationMatrix <- cor(clean$df)
selVars <- processCorMatrix(correlationMatrix)
colnames(correlationMatrix)[selVars]

# Check important variables
qplot(1:length(classe), total_accel_forearm
      , data = rfDF) + geom_point(aes(color = rfDF$classe)) + geom_smooth()

# Slice data, so that the training can be done in a reasonable amount of time
rfDF <- cbind(clean$df[,selVars], 'classe' = clean$classe)

inTrain <- createDataPartition(y = clean$classe, p = .35, list = FALSE)
trn <- rfDF[inTrain, ]
tst <- rfDF[-inTrain, ]

# Build the model
fitControl <- trainControl(
  method = "cv",
  number = 10)

start.time <- Sys.time()

modFit <-
  train(classe ~ ., data = trn, method = "rf", trControl = fitControl)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Test with remaing data
pred <- predict(modFit, tst)
confusionMatrix(pred, tst$classe)

# Predict with test data
testing <- read.csv("data/pml-testing.csv")
clean_t <- cleanUpInput(testing)
pred <- predict(modFit, data = clean_t)
confusionMatrix(pred, rfDF$classe)

