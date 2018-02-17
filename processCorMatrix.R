# Returns a list of variables with correlation factors lower than
# the specified threshold.
processCorMatrix <- function(corMatrix, threshold = 0.75) {
  # Initialize selected and discarded sets
  select <- vector("numeric")
  discard <- vector("numeric")
  
  n <- nrow(corMatrix)

  # Iterate over all elements in upper-right part of the diagonal
  for (i in 1:(n - 1)) {
    # if var i has not been discarded, add to select
    if (!(i %in% discard)) {
      select <- c(select, i)
    }
    # discard all vars j with correlation factor greater or equal to 0.75
    for (j in (i + 1):n) {
      if (is.na(corMatrix[i, j])) {
        print(paste("AFDSGDSFG", i, j))
      }
        
      if (corMatrix[i, j] >= threshold) {
        discard <- c(discard, j)
        
      }
    }
  }
  
  select
}
