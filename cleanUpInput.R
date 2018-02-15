# Receives a dataframe with the input data.
# Returns a dataframe and a vector.
# The dataframe of dimensions m x n is the result of the following steps:
# - Remove variables filled only with "DIV/0!"
# - Remove observations containing "DIV/0!" values
# - Remove variables that won't be used as predictors, i.e.,
#   variables row_number, user_name, classe, timestamp 
#   variables and window variables.
# - Turn factor variables into numeric variables
# - Remove variables that only contain 0s
# - Remove observations containing NAs
# The vector of length m contains the labels that classify the 
# rows (classe variable).
cleanUpInput <- function(inputDF) {

  # Remove variables containing only DIV/0!
  numDIV0 <- apply(inputDF, 2, function(col) {
    length(grep("DIV", col))
  })
  clean1 <- inputDF[,numDIV0 < max(numDIV0)]
  
  # Remove observations containing DIV/0!
  removeObs <- apply(clean1, 1, function(row) {
    length(grep("DIV", row)) > 0
  })
  clean2 <- clean1[!removeObs, ]
  
  # Remove variables that won't be used as predictors:
  # 1: row number
  # 2: user_name
  excludeColumns <- c(1, 2,
                      grep("classe", names(clean2)), 
                      grep("timestamp", names(clean2)), 
                      grep("window", names(clean2)))
  clean3 <- clean2[, -excludeColumns]
  
  
  # Turn factors into numeric values
  clean4 <- clean3
  for(i in 1:ncol(clean3)) {
    if (class(clean3[,i]) == "factor") {
      clean4[,i] <- as.numeric(as.character(clean3[,i]))
    }
  }
  
  # Remove columns which are all 0s
  all0 <- apply(clean4, 2, function(col) {
    min(col) == 0 && max(col) == 0
  })
  clean5 <- clean4[,!all0]
  
  # Remove observations containing NAs
  removeObs <- apply(clean5, 1, function(row) {
    length(row[is.na(row)]) > 0
  })
  
  return(list(df = clean5[!removeObs, ], classe = clean2$classe[!removeObs]))
}