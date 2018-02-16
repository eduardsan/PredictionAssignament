# Non-aggregate variables and variables that are not going to be used for prediction:
ignoreList <- c("X", "user_name",    "new_window",           "num_window",
                "cvtd_timestamp",    "raw_timestamp_part_1", "raw_timestamp_part_2", 
                "roll_belt",         "pitch_belt",           "yaw_belt",
                "gyros_belt_x",      "gyros_belt_y",         "gyros_belt_z",
                "accel_belt_x",      "accel_belt_y",         "accel_belt_z",
                "magnet_belt_x",     "magnet_belt_y",        "magnet_belt_z",
                "roll_arm",          "pitch_arm",            "yaw_arm",
                "gyros_arm_x",       "gyros_arm_y",          "gyros_arm_z",
                "accel_arm_x",       "accel_arm_y",          "accel_arm_z",
                "magnet_arm_x",      "magnet_arm_y",         "magnet_arm_z",
                "roll_dumbbell",     "pitch_dumbbell",       "yaw_dumbbell",
                "gyros_dumbbell_x",  "gyros_dumbbell_y",     "gyros_dumbbell_z",
                "accel_dumbbell_x",  "accel_dumbbell_y",     "accel_dumbbell_z",
                "magnet_dumbbell_x", "magnet_dumbbell_y",    "magnet_dumbbell_z",
                "roll_forearm",      "pitch_forearm",        "yaw_forearm",
                "gyros_forearm_x",   "gyros_forearm_y",      "gyros_forearm_z",
                "accel_forearm_x",   "accel_forearm_y",      "accel_forearm_z",
                "magnet_forearm_x",  "magnet_forearm_y",     "magnet_forearm_z")

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
