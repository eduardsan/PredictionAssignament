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
clean <- cleanUpInput(training[training$new_window == "yes",])

# Use correlation matrix to select variables
correlationMatrix <- cor(clean$df)
selVars <- processCorMatrix(correlationMatrix, .8)
colnames(correlationMatrix)[selVars]


qplot(1:length(classe), amplitude_pitch_dumbbell
      , data = cbind(s1res$df, classe = s1res$classe)) + geom_point(aes(color = s1res$classe)) + geom_smooth()

