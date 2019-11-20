# Setup -------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(stringr)
library(irr)
#library(stringi)

# Process transcripts -----------------------------------------------------

# ____For loop for extracting transcripts ---------------------------------

# Create file list vector
fileNames <- list.files(
  path = "./Data/transcripts",
  recursive=T,pattern=".txt")

dir <- getwd()
subdir <- file.path("Data", "transcripts")

# Function to be repeated for each file
for (fileName in fileNames) {
  
  statement <- read_lines(file.path(dir, subdir, fileName))
  
  tryCatch(
    {
      message(fileName)
      
      # Find where the two interfaces begin
      interfaceMarkers <- str_detect(statement, "\\w Version")
      
      # Find where each game begins
      gameMarkers <- str_detect(statement, "#>\\d")
      test<- as.data.frame(statement)
      test<- cbind(test,  interfaceMarkers, gameMarkers)
      
      # Add line numbers
      test <- mutate(test, lineNumber = seq_along(test$statement))
      
      # Add file name as variable
      test <- mutate(test, fileName = fileName)
      
      # Logical indexing for number of interface rows
      idx <- which(test$interfaceMarkers==T)
      test$interface[idx[1]:(idx[2]-1)] <- "joystick"
      test$interface[idx[2]:nrow(test)] <- "keyboard"
      
      
      # Split data by interface and add game number variable
      numx <- which(test$gameMarkers==T)
      testJoy <- filter(test, interface == "joystick")
      numxJoy <- which(testJoy$gameMarkers == T)
      testJoy <- testJoy %>% 
        mutate(gameNumber = 
                 case_when(
                   lineNumber < numxJoy[2] ~ 1,
                   lineNumber < numxJoy[3] & lineNumber >= numxJoy[2] ~ 2,
                   lineNumber < numxJoy[4] & lineNumber >= numxJoy[3] ~ 3,
                   lineNumber < numxJoy[5] & lineNumber >= numxJoy[4] ~ 4,
                   lineNumber < numxJoy[6] & lineNumber >= numxJoy[5] ~ 5,
                   lineNumber < numxJoy[7] & lineNumber >= numxJoy[6] ~ 6,
                   lineNumber < numxJoy[8] & lineNumber >= numxJoy[7] ~ 7,
                   lineNumber < numxJoy[9] & lineNumber >= numxJoy[8] ~ 8,
                   lineNumber < numxJoy[10] & lineNumber >= numxJoy[9] ~ 9,
                   lineNumber >= numxJoy[10] ~ 10))
      
      testKey <- filter(test, interface == "keyboard")
      numxKey <- which(testKey$gameMarkers == T) + (numx[11] - 2)
      testKey <- testKey %>% 
        mutate(gameNumber = 
                 case_when(
                   lineNumber < numxKey[2] ~ 1,
                   lineNumber < numxKey[3] & lineNumber >= numxKey[2] ~ 2,
                   lineNumber < numxKey[4] & lineNumber >= numxKey[3] ~ 3,
                   lineNumber < numxKey[5] & lineNumber >= numxKey[4] ~ 4,
                   lineNumber < numxKey[6] & lineNumber >= numxKey[5] ~ 5,
                   lineNumber < numxKey[7] & lineNumber >= numxKey[6] ~ 6,
                   lineNumber < numxKey[8] & lineNumber >= numxKey[7] ~ 7,
                   lineNumber < numxKey[9] & lineNumber >= numxKey[8] ~ 8,
                   lineNumber < numxKey[10] & lineNumber >= numxKey[9] ~ 9,
                   lineNumber >= numxKey[10] ~ 10))
      
      testFull <- bind_rows(testJoy, testKey)
      
      # Extract Participant_ID from fileName
      temp <- str_extract(testFull$fileName, "p[0-9]+")
      temp2 <- str_replace(temp, "[p][0]*", "")
      testFull <- mutate(testFull, Participant_ID = temp2)
      testFull$Participant_ID <- as.numeric(testFull$Participant_ID)
      
      # Write output to csv
      write_csv(testFull, "./Output/extracted-transcripts.csv", 
                append=TRUE, col_names = FALSE)
      
    },
    error=function(cond) {
      message(cond)
      return(err_or <- fileName)
    },
    warning=function(cond) {
      message(cond)
      return(war_ning <- fileName)
    },
    finally={
      message("Done")
    }
  )
}
