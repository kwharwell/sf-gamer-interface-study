# Setup -------------------------------------------------------------------

# Load libraries
library(tidyverse)

# Load data
data <- read_csv("./Data/all-games.csv")
fortressStatements <- read_csv("./Data/fortressAgreement-consensus.csv")
bonusStatements <- read_csv("./Data/bonusAgreement-consensus.csv")

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

# Calculate IRR for protocol coding ---------------------------------------

# ____Fortress-destruction statements -------------------------------------

# Generate list of participant IDs to be excluded
subjectList <- data %>%
  filter(Sex == "Men") %>% 
  filter(Exclude == 0) %>% 
  select(Participant_ID) %>% 
  unique()


men10 <- fortressStatements %>% 
  filter(Participant_ID %in% subjectList$Participant_ID) %>% 
  filter(cumulativeGameNumber < 11)

qcMatFortress <- select(men10, Patricia:Alex)
kappa2(qcMatFortress)


# ____Bonus-collection statements -----------------------------------------

men10 <- bonusStatements %>% 
  filter(Participant_ID %in% subjectList$Participant_ID) %>% 
  filter(cumulativeGameNumber < 11)

qcMatBonus <- select(men10, Patricia:Alex)
kappa2(qcMatBonus)

qcAgree <- bonus %>%
  mutate(agreement = if_else(Alex == Patricia, 1, 0))

qcDisagree <- filter(qcAgree, agreement == 0)


# ____Ship-control statements ---------------------------------------------


