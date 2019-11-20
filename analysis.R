# Setup -------------------------------------------------------------------

# Load libraries
library(tidyverse)

# Load data
data <- read_csv("./Data/all-games.csv")


# Analyze data ------------------------------------------------------------

# _____Descriptive Statistics ---------------------------------------------
df <- filter(data, Exclude == 0)

t1 <- df %>% 
  grouped_df(c("Sex_by_Interface", "Gamer_by_Interface", "Participant_ID")) %>% 
  summarize(ConstantAccelerations = sum(Constant_Accelerations), 
            WorldWrappers = sum(WorldWrapper)) %>%
  ungroup()
 
t2 <- df %>% 
  grouped_df(c("Sex_by_Interface", "Gamer_by_Interface")) %>% 
  summarize(ConstantAccelerations = mean(Constant_Accelerations), 
            WorldWrappers = mean(WorldWrapper)) %>%
  ungroup()

#write_csv(t1, "./Output/Strategy_use_descriptives.csv")


