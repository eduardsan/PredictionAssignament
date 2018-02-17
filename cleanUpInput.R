library(e1071)

# Non-aggregate variables and variables that are not going to be used for prediction:
ignoreList <- c("X", "user_name",    "new_window",           "num_window",
                "cvtd_timestamp",    "raw_timestamp_part_1", "raw_timestamp_part_2") 

# Cleans up the WLE dataset. Returns a new dataframe and a vector 
# with the classification values.
# Performs the following steps:
# - Remove variables included in ignore.
# - Remove variables with near zero variance
# - Turn factor variables into numeric variables
# - Remove variables that contain NAs
cleanUpInput <- function(inputDF, ignore = ignoreList) {

  classeValues <- inputDF$classe
  
  # Remove variables from ignore
  clean1 <- inputDF[, !(names(inputDF) %in% c(ignore, "classe"))]
  
  # Remove variables with near zero variance
  clean2 <- clean1[,-nearZeroVar(clean1)]

  # Turn factor variables to numeric
  clean3 <- clean2
  for(i in 1:ncol(clean2)) {
    if (class(clean2[,i]) == "factor") {
      clean3[,i] <- as.numeric(as.character(clean2[,i]))
    }
  }

  # Replace missing values
  clean4 <- as.data.frame(impute(clean3))
  
  # Repeat remove variables with near zero variance
  clean5 <- clean4[,-nearZeroVar(clean4)]
  
  return(list(df = clean5, classe = classeValues))
}
