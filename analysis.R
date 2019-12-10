# Setup -------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(irr)

# Load data
data <- read_csv("./Data/all-games.csv")
fortressStatements <- read_csv("./Data/fortress-statements-with-coding.csv")
bonusStatements <- read_csv("./Data/bonus-statements-with-coding.csv")
alltrans <- read_csv("./Data/transcripts-all-subs.csv")
shipControlConsensus <- read_csv("./Data/cRelevantAgreement.csv")

# Descriptive statistics --------------------------------------------------

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

# Total number of statements, number of subjects with protocols
df <- alltrans %>% 
  filter(Sex == 1) %>% 
  filter(Gamer != 99) %>%
  filter(cumulativeGameNumber < 11)

groupDF <- df %>%
  group_by(Gamer) %>% 
  summarise(count = n_distinct(Participant_ID))

# Calculate IRR for protocol coding ---------------------------------------

# ____Fortress-destruction statements -------------------------------------

# Restrict statements to men, only gamers and non-gamers, and first 10 games
df <- fortressStatements %>% 
  filter(Sex == 1) %>% 
  filter(Gamer != 99) %>%
  filter(cumulativeGameNumber < 11)

# Calculate agreement as Cohen's Kappa
qcMatFortress <- select(df, Patricia:Alex)
kappa2(qcMatFortress)


# ____Bonus-collection statements -----------------------------------------

# Restrict statements to men, only gamers and non-gamers, and first 10 games
df <- bonusStatements %>% 
  filter(Sex == 1) %>% 
  filter(Gamer != 99) %>%
  filter(cumulativeGameNumber < 11)

# Calculate agreement as Cohen's Kappa
qcMatBonus <- select(df, Patricia:Alex)
kappa2(qcMatBonus)


# ____Ship-control statements ---------------------------------------------

# Calculate agreement as Cohen's Kappa
qcMatBonus <- select(shipControlConsensus, cRelevantAlex, cRelevantPatricia)
kappa2(qcMatBonus)

