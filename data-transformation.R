# Setup -------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(irr)

# Load data
df <- read_csv("./Data/sf-behavior-extraction-2019-10-10.csv")
subInfo <- read_csv("./Data/subject-information-2019-08-16.csv")
irr <- read.csv("./Data/IRR_Combined_Subjects_Questionnaire_Responses.csv")
trans <- read_csv("./Data/extracted-transcripts.csv", col_names = FALSE)

# Transform SF behavioral data --------------------------------------------

# Break file name into separate variables and combine with other data
temp <- df$FileName %>% 
  as.character() %>% 
  str_replace(".gam","") %>% 
  str_replace("[0-9]+/", "") %>% 
  str_replace("p[0]+", "") %>% 
  str_replace("p", "") %>% 
  str_split_fixed("[-]",3) 

colnames(temp) <- c("Participant_ID", "Interface", "Game_Number")
temp <- data.frame(temp)
temp$Participant_ID <- as.numeric(levels(temp$Participant_ID))[temp$Participant_ID]
temp$Game_Number <- as.numeric(levels(temp$Game_Number))[temp$Game_Number]
temp2 <- cbind(temp, df)

# Combine subject info with game data
finalFrame <- full_join(temp2, subInfo, by = "Participant_ID")

# Restrict to first 10 games
# joyFirst <- filter(data, Joystick_First == 1, Interface == "joy")
# keyFirst <- filter(data, Joystick_First == 0, Interface == "key")
# finalFrame <- rbind(joyFirst, keyFirst)

finalFrame <- finalFrame %>%
  mutate(Exclude = ifelse((Sex == 2 & Gamer == 1) | Gamer == 99, 1, 0))

finalFrame <- finalFrame %>% 
  mutate(Sex_by_Interface = case_when(Sex == 1 & Interface == "joy" ~ "Men_joystick",
                                      Sex == 1 & Interface == "key" ~ "Men_keyboard",
                                      Sex == 2 & Interface == "joy" ~ "Women_joystick",
                                      Sex == 2 & Interface == "key" ~ "Women_keyboard"))

finalFrame <- finalFrame %>% 
  mutate(Gamer_by_Interface = case_when(Gamer == 0 & Interface == "joy" ~ "nVGP_joystick",
                                      Gamer == 0 & Interface == "key" ~ "nVGP_keyboard",
                                      Gamer == 1 & Interface == "joy" ~ "VGP_joystick",
                                      Gamer == 1 & Interface == "key" ~ "VGP_keyboard"))

finalFrame$Sex <- recode(finalFrame$Sex, "1" = "Men", "2" = "Women" )
finalFrame$Gamer <- recode(finalFrame$Gamer, "0" = "nVGP", "1" = "VGP", "99" = "In-between")

# finalFrame <- finalFrame %>% 
#   group_by(Participant_ID) %>% 
#   mutate(world_wrapping_proportion = mean(WorldWrapper)) %>% 
#   mutate(world_wrapper_category = ifelse(world_wrapping_proportion >= .5, "WorldWrapper", "Non-WorldWrapper")) %>% 
#   ungroup()
# 
# finalFrame <- finalFrame %>% 
#   mutate(thrust_ratio = Thrusts/discrete_thrusts) %>% 
#   mutate(CW_ratio = CWrotations/discrete_CWrotations) %>% 
#   mutate(CCW_ratio = CCWrotations/discrete_CCWrotations)

# Questionnaire Responses

# IRR
kappa2(irr[ ,c(2:3)], "unweighted")

# Score
irr2 <- irr %>% 
  group_by(Participant.ID) %>% 
  mutate(questionnaire_score = sum(rater2_m)) %>% 
  ungroup()
irr2_unique <- unique(irr2[, c(1, 4)])
colnames(irr2_unique) <- c("Participant_ID", "questionnaire_score")

finalFrame2 <- inner_join(finalFrame, irr2_unique, by = "Participant_ID")

finalFrame2 <- mutate(finalFrame2, cumulativeGameNumber = case_when(
                      Interface == "joy" & Joystick_First == "1" ~ Game_Number,
                      Interface == "joy" & Joystick_First == "0" ~ (Game_Number + 10),
                      Interface == "key" & Joystick_First == "1" ~ (Game_Number + 10),
                      Interface == "key" & Joystick_First == "0" ~ Game_Number))

finalFrame3 <- finalFrame2 %>%
  select(-VGES16) %>% 
  rename(gamingComputer = ownComputer, hoursTwoWeeks = VGES8, lastFiveGames = VGES10, gamesSerious = VGES16_1_TEXT) %>% 
  mutate(totalBonus = Bpnt + Bmis)

  

# finalFrame2$world_wrapping_count <- finalFrame2$world_wrapping_proportion * 10
# finalFrame2$Game_Number0 <- finalFrame2$Game_Number - 1
# finalFrame2 <- finalFrame2 %>% 
#   mutate(control_minus_worldwraps = Control + WorldWraps*35)


# Have 10 games for everybody
#testCount <- count(finalFrame, Participant_ID)


# ____Write data ----------------------------------------------------------

write_csv(finalFrame3, "./Output/all-games.csv")


# Transform transcript data -----------------------------------------------

# ____All subjects --------------------------------------------------------

# Add subject information
colnames(trans) <- c("statement", "interfaceMarkers", "gameMarkers", "lineNumber", 
                 "fileName", "interface", "gameNumber", "Participant_ID")

# Combine subject info with transcript data
data <- full_join(subInfo[, 1:6], trans, by = "Participant_ID")

# Clean the transcripts
data <- mutate(data, lowerStatement = str_to_lower(statement))
data <- mutate(data, statementClean = str_replace_all(lowerStatement, 
                                                      "[^[:alnum:][:blank:]]", ""))
data$phrase <- str_detect(data$statementClean, "\\w \\w")

# Add cumulative game number
data <- mutate(data, cumulativeGameNumber = case_when(
  interface == "joystick" & Joystick_First == 1 ~ gameNumber,
  interface == "joystick" & Joystick_First == 0 ~ (gameNumber + as.integer(10)),
  interface == "keyboard" & Joystick_First == 1 ~ (gameNumber + as.integer(10)),
  interface == "keyboard" & Joystick_First == 0 ~ gameNumber))

# ________Write data ------------------------------------------------------

write_csv(data, "./Output/transcripts-all-subs.csv")

