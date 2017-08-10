# File for building WP model

# Access tidyverse:
library(tidyverse)

# Load and stack the play-by-play data:
pbp_data <- rbind(readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2009.csv"),
                  readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2010.csv"),
                  readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2011.csv"),
                  readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2012.csv"),
                  readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2013.csv"),
                  readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2014.csv"),
                  readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2015.csv"),
                  readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2016.csv"))

# Remove error game from 2011 that is coded incorrectly in raw JSON data:

pbp_data <- pbp_data %>% filter(GameID != "2011121101")

# Create an indicator for which half it is:

pbp_data$Half_Ind <- with(pbp_data,ifelse(qtr %in% c(1,2),"Half1",
                                          ifelse(qtr %in% c(3,4),"Half2","Overtime")))

# Access nflscrapR

library(nflscrapR)

# Create datasets with the results of each game:
# (this takes some time to run)

games_2009 <- season_games(2009)
games_2010 <- season_games(2010)
games_2011 <- season_games(2011)
games_2012 <- season_games(2012)
games_2013 <- season_games(2013)
games_2014 <- season_games(2014)
games_2015 <- season_games(2015)
games_2016 <- season_games(2016)
 
# Bind together and save:
 
games_data <- bind_rows(games_2009, games_2010,
                        games_2011, games_2012,
                        games_2013, games_2014,
                        games_2015, games_2016)


# Create a column, Winner for the games_data, that allows for tied games:

games_data$Winner <- ifelse(games_data$homescore > games_data$awayscore,
                            games_data$home,
                            ifelse(games_data$homescore < games_data$awayscore,
                                   games_data$away,"TIE"))

# Dataset of only the GameID and Winner column
win_data <- games_data %>% select(GameID, Winner)
win_data$GameID <- as.character(win_data$GameID)

# Left join Winner column to pbp_data:
pbp_data_win <- pbp_data
pbp_data_win$GameID <- as.character(pbp_data_win$GameID)
pbp_data_win <- left_join(pbp_data_win, win_data, by="GameID")

# Create an indicator column if the posteam wins the game:

pbp_data_win$Win_Indicator <- ifelse(pbp_data_win$posteam == pbp_data_win$Winner,
                                     1,0)

# Create ExpScoreDiff:

pbp_data_win <- pbp_data_win %>% mutate(ExpScoreDiff = ExpPts + ScoreDiff)

# Create a variable that is time remaining until end of half and game:
pbp_data_win$TimeSecs_Remaining <- ifelse(pbp_data_win$qtr %in% c(1,2),
                                          pbp_data_win$TimeSecs - 1800,
                                          ifelse(pbp_data_win$qtr == 5,
                                                 pbp_data_win$TimeSecs + 900,
                                                 pbp_data_win$TimeSecs))

# Create a column with the opponents timeouts remaining at
# the start of the play (opposite of posteam_timeouts_pre):
pbp_data_win$oppteam_timeouts_pre <- ifelse(pbp_data_win$posteam == pbp_data_win$HomeTeam,
                                            pbp_data_win$AwayTimeouts_Remaining_Pre,
                                            pbp_data_win$HomeTimeouts_Remaining_Pre)

# Remove the rows that don't have any ExpScoreDiff and 
# missing Win Indicator:
pbp_data_win_model_data <- pbp_data_win %>% filter(!is.na(ExpScoreDiff) &
                                                     !is.na(Win_Indicator) &
                                                     PlayType != "End of Game")

# Under two-minute warning indicator:
pbp_data_win_model_data$Under_TwoMinute_Warning <- ifelse(pbp_data_win_model_data$TimeSecs_Remaining < 120,1,0)


# Due to NFL errors make a floor at 0 for the timeouts:
pbp_data_win_model_data$oppteam_timeouts_pre <- ifelse(pbp_data_win_model_data$oppteam_timeouts_pre < 0,
                                                       0,pbp_data_win_model_data$oppteam_timeouts_pre)

# Turn win indicator into a factor:
pbp_data_win_model_data$Win_Indicator <- as.factor(pbp_data_win_model_data$Win_Indicator)

# Define a form of the TimeSecs_Adj that just takes the original TimeSecs but
# resets the overtime back to 900:

pbp_data_win_model_data$TimeSecs_Adj <- ifelse(pbp_data_win_model_data$qtr == 5,
                                               pbp_data_win_model_data$TimeSecs + 900,
                                               pbp_data_win_model_data$TimeSecs)

# Define a new variable, ratio of Expected Score Differential to TimeSecs_Adj:

pbp_data_win_model_data <- pbp_data_win_model_data %>% 
  mutate(ExpScoreDiff_Time_Ratio = ExpScoreDiff / (TimeSecs_Adj + 1))


# Filter out overtime:
pbp_data_win_model_data_regular <- dplyr::filter(pbp_data_win_model_data, qtr != 5)

# Turn Half_Ind into a factor:
pbp_data_win_model_data_regular$Half_Ind <- as.factor(pbp_data_win_model_data_regular$Half_Ind)


# Fit the win probability model (takes some time to run):
library(mgcv)
wp_model <- bam(Win_Indicator ~ s(ExpScoreDiff) + s(TimeSecs_Remaining,by=Half_Ind) + s(ExpScoreDiff_Time_Ratio) + Under_TwoMinute_Warning*posteam_timeouts_pre*Half_Ind + Under_TwoMinute_Warning*oppteam_timeouts_pre*Half_Ind,
                               data = pbp_data_win_model_data_regular, family="binomial")

# Save the model (commented out due to file size limit)
# save(wp_model, file="wp_model.RData")

