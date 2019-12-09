# Setup -------------------------------------------------------------------

# Load libraries
library(tidyverse)

# Load data
data <- read_csv("./Data/all-games.csv")
fortressStatements <- read_csv("./Data/fortress-statements-with-coding.csv")
bonusStatements <- read_csv("./Data/bonusAgreement-consensus.csv")
alltrans <- read_csv("./Data/transcripts-all-subs.csv")

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

# Generate list of participant IDs to be excluded
subjectList <- data %>%
  filter(Sex == "Men") %>% 
  filter(Exclude == 0) %>% 
  select(Participant_ID) %>% 
  unique()


men10 <- fortressStatements %>% 
  filter(Participant_ID %in% subjectList$Participant_ID) %>% 
  filter(cumulativeGameNumber < 11)

df <- fortressStatements %>% 
  filter(Sex == 1) %>% 
  filter(Gamer != 99) %>%
  filter(cumulativeGameNumber < 11)



qcMatFortress <- select(df, Patricia:Alex)
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


