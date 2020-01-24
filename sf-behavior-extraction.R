# Setup -------------------------------------------------------------------

setwd("/Users/kyleharwell/Archive/SF_Game_Files/Gamer_Study_Data")

# Load libraries
library(stringr)
library(tidyverse)

# Initialize ticks
ticks <- seq(1:3600)


# Loop for extracting data from .gam files --------------------------------

# Create file list vector
fileNames <- list.files(
  path = "/Users/kyleharwell/Archive/SF_Game_Files/Gamer_Study_Data",
  recursive=T,pattern="*.gam")

# For loop to iterate across game files
for (fileName in fileNames) {
  
  sample <- readLines(fileName)
  
  tryCatch(
    {
      message(fileName)

  # Pull out number of clockwise turns
  v1 <- sample[grep("Ship1:TurnCW", sample)]
  CWvector <- str_extract(v1, "Ship1:TurnCW")
  CW <- length(CWvector)
  
  
  # Pull out number of counterclockwise turns
  v2 <- sample[grep("Ship1:TurnCCW", sample)]
  CCWvector <- str_extract(v2, "Ship1:TurnCCW")
  CCW <- length(CCWvector)
  
  
  # Pull out number of ship thrusts
  v3 <- sample[grep("Ship1:Thrust", sample)]
  ThrustVector <- str_extract(v3, "Ship1:Thrust")
  Thrust <- length(ThrustVector)
  
  
  # # Pull out number of times inside big hexagon
  # v4 <- sample[grep("Collision_Ship_Hexagon:1:7", sample)]
  # BigHexagon1Vector <- str_extract(sample, "Collision_Ship_Hexagon:1:7")
  # InBounds <- length(BigHexagon1Vector)
  
  # # Pull out number of times outside big hexagon
  # v5 <- sample[grep("Collision_Ship_Hexagon:2:3", sample)]
  # BigHexagon0Vector <- str_extract(sample, "Collision_Ship_Hexagon:2:3")
  # OutBounds <- length(BigHexagon0Vector)
  
  # Pull out number of WorldWraps
  v6 <- sample[grep("Ship_WorldWrap", sample)]
  WorldWrapVector <- str_extract(v6, "Ship_WorldWrap")
  WorldWrap <- length(WorldWrapVector)
  
  
  # Pull out number of Fortress Collisions
  v7 <- sample[grep("UpdateControl_CollisionShipWithNeutralHexagon:-5", sample)]
  FortressCollisionVector <- str_extract(v7, 
                            "UpdateControl_CollisionShipWithNeutralHexagon:-5")
  FortressCollision <- length(FortressCollisionVector)
  
  # Pull out number of Fortress Destructions
  v8 <- sample[grep("FortressDestroyed", sample)]
  fortressDestructionVector <- str_extract(v8, "FortressDestroyed")
  fortressDestructions <- length(fortressDestructionVector)
  
  # Pull out number of ship deaths
  vDie <- sample[grep("Ship_Die", sample)]
  shipDeathVector <- str_extract(vDie, "Ship_Die")
  shipDeaths <- length(shipDeathVector)
  
  
  # Pull out score data
  v9 <- sample[grep("/%", sample)]
  ScoreVector <- str_match(v9, 
    "([-+]?[0-9]+) ([-+]?[0-9]+) ([-+]?[0-9]+) ([-+]?[0-9]+) ([-+]?[0-9]+) ([-+]?[0-9]+) ([-+]?[0-9]+) ([-+]?[0-9]+)")
  Score <- ScoreVector[c(-1,-7)]
  Score <- as.numeric(Score)
  
  # Worldwrapper categorization
  v10 <- str_extract(sample, "Ship_WorldWrap|Ship1:Thrust|Ship1:Fire")
  v10 <- v10[!is.na(v10)]
  v10_flat <- str_flatten(v10, "-")
  WorldWrap_list <- str_locate_all(v10_flat,
  "((Ship_WorldWrap-)+(Ship1:Fire-)+){2}")
  WorldWrapper <- length(WorldWrap_list[[1]])/2
  
  # Worldwrapper categorization based on deliberate behavior for one life
  v11 <- str_extract(sample, "Ship_WorldWrap|Ship1:Thrust|Ship1:Fire|Ship_Die|FortressDestroyed|/%")
  v11 <- v11[!is.na(v11)]
  v11_flat <- str_flatten(v11, "-")
  delWorldWrap_list <- str_locate_all(v11_flat,
                                   "((Ship1:Thrust-)+(Ship1:Fire-)*((Ship_WorldWrap-)+(Ship1:Fire-)+){2,}(Ship_WorldWrap-)*(FortressDestroyed-|Ship_Die-|/%))")
  delWorldWrapper <- length(delWorldWrap_list[[1]])/2
  
  # Discrete Thrusts                           
  v11 <- sample[grep("Ship1:Thrust", sample)]
  v11_vector <- str_extract(v11, "<[0-9]+:")
  v11_vector <- str_replace(v11_vector, "<", "")
  v11_vector <- str_replace(v11_vector, ":", "")
  v11_vector <- as.numeric(v11_vector)
  v11_2 <- v11_vector[-length(v11_vector)]
  v11_2 <- c(0, v11_2)
  consecutive_thrusts <- v11_vector - v11_2
  discrete_thrusts <- length(consecutive_thrusts[consecutive_thrusts > 1])
  
  # Discrete CWrotations                           
  v12 <- sample[grep("Ship1:TurnCW", sample)]
  v12_vector <- str_extract(v12, "<[0-9]+:")
  v12_vector <- str_replace(v12_vector, "<", "")
  v12_vector <- str_replace(v12_vector, ":", "")
  v12_vector <- as.numeric(v12_vector)
  v12_2 <- v12_vector[-length(v12_vector)]
  v12_2 <- c(0, v12_2)
  consecutive_CW <- v12_vector - v12_2
  discrete_CWrotations <- length(consecutive_CW[consecutive_CW > 1])
  
  # Discrete CCWrotations                           
  v13 <- sample[grep("Ship1:TurnCCW", sample)]
  v13_vector <- str_extract(v13, "<[0-9]+:")
  v13_vector <- str_replace(v13_vector, "<", "")
  v13_vector <- str_replace(v13_vector, ":", "")
  v13_vector <- as.numeric(v13_vector)
  v13_2 <- v13_vector[-length(v13_vector)]
  v13_2 <- c(0, v13_2)
  consecutive_CCW <- v13_vector - v13_2
  discrete_CCWrotations <- length(consecutive_CCW[consecutive_CCW > 1])
  
  # Constant Accelerator Categorization
  no_thrusts <- setdiff(ticks, v11_vector)
  constant_accelerator <- c()
  for (i in no_thrusts){
    if (i == 1){
      next
    }
    constant_accelerator[i] <- no_thrusts[i] - no_thrusts[i-1]
    constant_accelerator[i] <- no_thrusts[i] - no_thrusts[i-1]
  }
  Constant_Accelerations <- length(na.omit(constant_accelerator[constant_accelerator >=50]))
  
  # # Time spent not moving/drifting
  # test <- no_thrusts[2:length(no_thrusts)]
  # test2 <- no_thrusts[1:(length(no_thrusts) - 1)]
  # test3 <- test - test2
  # test4 <- sum(test3[test3 == 1])
  
  # # Length of Average thrust event
  # consecutive_thrusts <- c()
  # for (i in v11_vector){
  #   if (i == 1){
  #     next
  #   }
  #  consecutive_thrusts[i] <- v11_vector[i] - v11_vector[i-1]
  # }
  # avg_interthrust_latency <- mean(na.omit(consecutive_thrusts))
  
  # Cumulative Distance from Spawn X Coordinate, and variance
  v14 <- sample[grep("sf.object.SF_Ship, Ship1,", sample)]
  xDistanceVector <- str_match(v14, 
                           "([0-9]+.[0-9]+), ([0-9]+.[0-9]+), ([0-9]+)")
  temp <- data.frame(na.omit(xDistanceVector[, 2]),stringsAsFactors=FALSE)
  colnames(temp) <- "x"
  temp$x <- as.numeric(temp$x)
  distanceX = (temp$x - 114.0)
  cumulativeDistanceX <- sum(distanceX)
  varianceX <- var(distanceX)
  
  # Put all values into a single data frame
  temp <- c(CW,CCW,Thrust,WorldWrap,FortressCollision,fortressDestructions, shipDeaths, Score, WorldWrapper,
            delWorldWrapper, discrete_thrusts, discrete_CWrotations, discrete_CCWrotations, 
            Constant_Accelerations, cumulativeDistanceX, varianceX)
  temp.df <- data.frame(temp)
  trans.temp.df <- data.frame(t(temp.df))
  
  # Add file names as variable
  rownames(trans.temp.df) <- fileName
  
  #Write to a text file
  #Remember to delete this file if running script again
  write.table(trans.temp.df, "Output.txt", append=TRUE, sep=" ",
              row.names=TRUE, col.names=FALSE)
  
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

# Add variable lables and write to csv ------------------------------------

a <- read.table("Output.txt")
colnames(a) <- c("FileName","CWrotations","CCWrotations","Thrusts", 
                 "WorldWraps","FortressCollisions", "fortressDestructions", "shipDeaths", 
                 "Total","Points","Velocity","Control","Speed", "Bpnt", "Bmis", "WorldWrapper", "delWorldWrapper",
                 "discrete_thrusts", "discrete_CWrotations", 
                 "discrete_CCWrotations", "Constant_Accelerations", "cumulativeDistanceX",
                 "varianceX")

write_csv(a,
  "/Users/kyleharwell/Desktop/Research/sf-gamer-interface-study/Output/sf-behavior-extraction.csv")
