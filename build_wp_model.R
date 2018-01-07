# File for building WP model

# Access tidyverse:
library(tidyverse)
library(magrittr)

# Load and stack the play-by-play data:
pbp_data <-   map_dfr(c(2009:2016), function(x) {
  suppressMessages(readr::read_csv(paste("https://raw.github.com/ryurko/nflscrapR-data/master/data/season_play_by_play/pbp_",
                                         x, ".csv", sep = "")))
})

# Remove error game from 2011 that is coded incorrectly in raw JSON data:
pbp_data <- pbp_data %>% filter(GameID != "2011121101") %>% 
  # Create an indicator for which half it is:
  mutate(Half_Ind = ifelse(qtr %in% c(1,2),"Half1",
                           ifelse(qtr %in% c(3,4),"Half2","Overtime")))

# Load and stack the games data:
games_data <-   map_dfr(c(2009:2016), function(x) {
  suppressMessages(readr::read_csv(paste("https://raw.github.com/ryurko/nflscrapR-data/master/data/season_games/games_",
                                         x, ".csv", sep = "")))
})

# Create a dataset of only the GameID and winner for each game,
win_data <- games_data %>%
  # Create a column, Winner for the games_data, that allows for tied games:
  mutate(Winner = ifelse(homescore > awayscore, home,
                         ifelse(homescore < awayscore, away, "TIE")),
         GameID = as.character(GameID)) %>%
  select(GameID, Winner)
  
# Left join Winner column to pbp_data then create the model dataset: 
pbp_data_win <- pbp_data %>%
  mutate(GameID = as.character(GameID))
pbp_data_win <- pbp_data_win %>% left_join(win_data, by = "GameID") %>%
  # Create an indicator column if the posteam wins the game:
  mutate(Win_Indicator = ifelse(posteam == Winner, 1, 0),
         # Calculate the Expected Score Differential by taking the sum
         # fo the Expected Points for a play and the score differential:
         ExpScoreDiff = ExpPts + ScoreDiff,
         # Create a variable that is time remaining until end of half and game:
         TimeSecs_Remaining = ifelse(qtr %in% c(1,2), TimeSecs - 1800,
                                      ifelse(qtr == 5, TimeSecs + 900,
                                             TimeSecs)),
         # Create a column with the opponents timeouts remaining at
         # the start of the play (opposite of posteam_timeouts_pre):
         oppteam_timeouts_pre = ifelse(posteam == HomeTeam, AwayTimeouts_Remaining_Pre,
                                       HomeTimeouts_Remaining_Pre),
         # Due to NFL errors make a floor at 0 for the timeouts:
         oppteam_timeouts_pre <- ifelse(oppteam_timeouts_pre < 0, 0, 
                                        oppteam_timeouts_pre),
         # Under two-minute warning indicator
         Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120, 1, 0),
         # Define a form of the TimeSecs_Adj that just takes the original TimeSecs but
         # resets the overtime back to 900:
         TimeSecs_Adj = ifelse(qtr == 5, TimeSecs + 900, TimeSecs)) %>%
  # Remove the rows that don't have any ExpScoreDiff and missing Win Indicator:
  filter(!is.na(ExpScoreDiff) & !is.na(Win_Indicator) & PlayType != "End of Game") %>%
  # Turn win indicator into a factor:
  mutate(Win_Indicator = as.factor(Win_Indicator),
         # Define a new variable, ratio of Expected Score Differential to TimeSecs_Adj
         # which is the same variable essentially as in Lock's random forest model
         # but now including the expected score differential instead:
         ExpScoreDiff_Time_Ratio = ExpScoreDiff / (TimeSecs_Adj + 1)) %>%
  # Remove overtime rows since overtime has special rules regarding the outcome:
  filter(qtr != 5) %>%
  # Turn the Half indicator into a factor:
  mutate(Half_Ind = as.factor(Half_Ind))

nrow(pbp_data_win)
# 335973

# Fit the win probability model (takes some time to run):
wp_model <- mgcv::bam(Win_Indicator ~ s(ExpScoreDiff) + s(TimeSecs_Remaining,by=Half_Ind) + s(ExpScoreDiff_Time_Ratio) + 
                  Under_TwoMinute_Warning*posteam_timeouts_pre*Half_Ind + 
                  Under_TwoMinute_Warning*oppteam_timeouts_pre*Half_Ind,
                               data = pbp_data_win, family = "binomial")

# Save the model (commented out due to file size limit)
# save(wp_model, file="wp_model.RData")

