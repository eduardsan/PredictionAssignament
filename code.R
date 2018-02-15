library(caret)
library(dplyr)
library(rafalib)

source("cleanUpInput.R")

# Read raw data
training <- read.csv("data/pml-training.csv")

# Focus on aggregate values of one subject
sub1 <- training[training$user_name == "carlitos" & training$new_window == "yes",]

result <- cleanUpInput(sub1)
