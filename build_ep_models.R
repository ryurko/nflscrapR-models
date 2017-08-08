# File for building EP models (and their corresponding FG models)

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

# Define a function that takes in a play-by-play data set and returns
# what the type of next score is and the drive number only within a half:

find_game_next_score_half <- function(pbp_dataset) {
  # Which rows are the scoring plays:
  score_plays <- which(pbp_dataset$sp == 1 & pbp_dataset$PlayType != "No Play")
  # Define a helper function that takes in
  # the current play index, a vector of the
  # scoring play indices, play-by-play data,
  # and returns the score type and 
  # drive number for the next score:
  find_next_score <- function(play_i,score_plays_i,pbp_df){
    # Find the next score index for the current play
    # based on being the first next score index:
    next_score_i <- score_plays_i[which(score_plays_i >= play_i)[1]]
    # If next_score_i is NA (no more scores after current play)
    # or if the next score is in another half,
    # then return No_Score and the current drive number
    if (is.na(next_score_i) | (pbp_df$qtr[play_i] %in% c(1,2) & pbp_df$qtr[next_score_i] %in% c(3,4,5)) | (pbp_df$qtr[play_i] %in% c(3,4) & pbp_df$qtr[next_score_i] == 5)){
      score_type <- "No_Score"
      # Make it the current play index
      score_drive <- pbp_df$Drive[play_i]
      # Else return the observed next score type and drive number:
    } else {
      # Store the score_drive number
      score_drive <- pbp_df$Drive[next_score_i]
      
      # Then check the play types to decide what to return
      # based on several types of cases for the next score:
      
      # 1: Return TD
      if (identical(pbp_df$ReturnResult[next_score_i],"Touchdown")){
        # For return touchdowns the current posteam would not have
        # possession at the time of return, so it's flipped:
        if (identical(pbp_df$posteam[play_i],pbp_df$posteam[next_score_i])){
          score_type <- "Opp_Touchdown"
        } else{
          score_type <- "Touchdown"
        }
      } else if (identical(pbp_df$FieldGoalResult[next_score_i],"Good")){
        # 2: Field Goal
        # Current posteam made FG
        if (identical(pbp_df$posteam[play_i],pbp_df$posteam[next_score_i])){
          score_type <- "Field_Goal"
          # Opponent made FG
        } else {
          score_type <- "Opp_Field_Goal"
        }
        # 3: Touchdown (returns already counted for)
      } else if (pbp_df$Touchdown[next_score_i]==1){
        # Current posteam TD
        if (identical(pbp_df$posteam[play_i],pbp_df$posteam[next_score_i])){
          score_type <- "Touchdown"
          # Opponent TD
        } else {
          score_type <- "Opp_Touchdown"
        }
        # 4: Safety (similar to returns)
      } else if (pbp_df$Safety[next_score_i]==1){
        if (identical(pbp_df$posteam[play_i],pbp_df$posteam[next_score_i])){
          score_type <- "Opp_Safety"
        } else{
          score_type <- "Safety"
        }
        # 5: Extra Points
      } else if (identical(pbp_df$ExPointResult[next_score_i],"Made")){
        # Current posteam Extra Point
        if (identical(pbp_df$posteam[play_i],pbp_df$posteam[next_score_i])){
          score_type <- "Extra_Point"
          # Opponent Extra Point
        } else {
          score_type <- "Opp_Extra_Point"
        }
        # 6: Two Point Conversions
      } else if (identical(pbp_df$TwoPointConv[next_score_i],"Success")){
        # Current posteam Two Point Conversion
        if (identical(pbp_df$posteam[play_i],pbp_df$posteam[next_score_i])){
          score_type <- "Two_Point_Conversion"
          # Opponent Two Point Conversion
        } else {
          score_type <- "Opp_Two_Point_Conversion"
        }
        # 7: Defensive Two Point (like returns)
      } else if(identical(pbp_df$DefTwoPoint[next_score_i],"Success")){
        if (identical(pbp_df$posteam[play_i],pbp_df$posteam[next_score_i])){
          score_type <- "Opp_Defensive_Two_Point"
        } else{
          score_type <- "Defensive_Two_Point"
        }
        # 8: Errors of some sort so return NA (but shouldn't take place)
      } else {
        score_type <- NA
      }
    }
    return(as.data.frame(list(Next_Score_Half = score_type,
                              Drive_Score_Half = score_drive)))
  }
  # Now apply this helper function to each play:
  return(bind_rows(lapply(c(1:nrow(pbp_dataset)), find_next_score, 
                          score_plays_i = score_plays, pbp_df = pbp_dataset)))
}

# Apply to each game (ignore the warning messages here):
pbp_next_score_half <- lapply(unique(pbp_data$GameID), FUN = function(x){
  find_game_next_score_half(filter(pbp_data, GameID == x))}) %>% bind_rows()

# Join to the pbp_data:
pbp_data_next_score <- cbind(pbp_data, pbp_next_score_half)

# Reference level should be No_Score:

pbp_data_next_score$Next_Score_Half <- factor(pbp_data_next_score$Next_Score_Half,
                                              levels = c("No_Score","Opp_Field_Goal",
                                                         "Opp_Safety","Opp_Touchdown",
                                                         "Field_Goal","Safety","Touchdown",
                                                         "Extra_Point","Opp_Extra_Point",
                                                         "Two_Point_Conversion",
                                                         "Opp_Two_Point_Conversion",
                                                         "Opp_Defensive_Two_Point"))

# Create the EP model dataset that only includes plays with basic 7 next score types
# and the following play types: Field Goal, No Play, Pass, Punt, Run, Sack, Spike:

pbp_ep_model_data <- pbp_data_next_score %>% filter(Next_Score_Half %in% c("Opp_Field_Goal",
                                                                           "Opp_Safety",
                                                                           "Opp_Touchdown",
                                                                           "Field_Goal",
                                                                           "No_Score",
                                                                           "Safety",
                                                                           "Touchdown") & 
                                                      PlayType %in% c("Field Goal",
                                                                      "No Play",
                                                                      "Pass",
                                                                      "Punt",
                                                                      "Run",
                                                                      "Sack",
                                                                      "Spike") &
                                                      is.na(TwoPointConv) &
                                                      is.na(ExPointResult) &
                                                      !is.na(down))

# Drop the unused levels:
pbp_ep_model_data$Next_Score_Half <- factor(pbp_ep_model_data$Next_Score_Half,
                                            levels = c("No_Score","Opp_Field_Goal",
                                                       "Opp_Safety","Opp_Touchdown",
                                                       "Field_Goal","Safety","Touchdown"))

# Create a variable that is time remaining until end of half and game:
pbp_ep_model_data$TimeSecs_Remaining <- ifelse(pbp_ep_model_data$qtr %in% c(1,2),
                                               pbp_ep_model_data$TimeSecs - 1800,
                                               ifelse(pbp_ep_model_data$qtr == 5,
                                                      pbp_ep_model_data$TimeSecs + 900,
                                                      pbp_ep_model_data$TimeSecs))

# Create log_ydstogo:
pbp_ep_model_data <- pbp_ep_model_data %>% mutate(log_ydstogo = log(ydstogo))

# Create Under_TwoMinute_Warning indicator
pbp_ep_model_data$Under_TwoMinute_Warning <- ifelse(pbp_ep_model_data$TimeSecs_Remaining < 120,1,0)

# Changing down into a factor variable: 
pbp_ep_model_data$down <- factor(pbp_ep_model_data$down)

# Calculate the drive difference between the score drive and the play drive:
pbp_ep_model_data <- pbp_ep_model_data %>% mutate(Drive_Score_Dist = Drive_Score_Half - Drive)

# Generate the weights

# Create a weight column based on difference in drives between play and next score:
pbp_ep_model_data <- pbp_ep_model_data %>% 
  mutate(Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) / (max(Drive_Score_Dist) - min(Drive_Score_Dist)))

# Create a weight column based on score differential:
pbp_ep_model_data <- pbp_ep_model_data %>% 
  mutate(ScoreDiff_W = (max(abs(ScoreDiff)) - abs(ScoreDiff)) / (max(abs(ScoreDiff)) - min(abs(ScoreDiff))))

# Add these weights together and scale again:
pbp_ep_model_data <- pbp_ep_model_data %>% 
  mutate(Total_W = Drive_Score_Dist_W + ScoreDiff_W,
         Total_W_Scaled = (Total_W - min(Total_W)) / (max(Total_W) - min(Total_W)))

# Fit the expected points model:

ep_model <- nnet::multinom(Next_Score_Half ~ TimeSecs_Remaining + yrdline100 + down + log_ydstogo + GoalToGo + log_ydstogo*down + yrdline100*down + GoalToGo*log_ydstogo + Under_TwoMinute_Warning, 
                           data=pbp_ep_model_data,weights = Total_W_Scaled,maxit=300)

# Save the model (commented out due to file size limit)
# save(ep_model, file="ep_model.RData")

# Following code is used to build the models that hold
# the season out for predictions, and weights the observations
# by the difference in Season as well

# Define the build_LOSO_EP_Model() function that takes in
# season to holdout and the data:

build_LOSO_EP_Model <- function(holdout_season, data = pbp_ep_model_data){
  # Filter out the holdout season:
  train <- filter(data, Season != holdout_season)
  
  # Generate the weights
  
  # Create a weight column based on difference in drives between play and next score:
  train <- train %>% 
    mutate(Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) / (max(Drive_Score_Dist) - min(Drive_Score_Dist)))
  
  # Create a weight column based on score differential:
  train <- train %>% 
    mutate(ScoreDiff_W = (max(abs(ScoreDiff)) - abs(ScoreDiff)) / (max(abs(ScoreDiff)) - min(abs(ScoreDiff))))
  
  # Create a weight column based on season difference from holdout season:
  train <- train %>%
    mutate(Season_Diff = abs(Season - holdout_season),
           Season_Diff_W = (max(Season_Diff) - Season_Diff) / (max(Season_Diff) - min(Season_Diff)))
  
  # Add the three together and scale again:
  train <- train %>% 
    mutate(Total_Season_W = Drive_Score_Dist_W + ScoreDiff_W + Season_Diff_W,
           Total_Season_W_Scaled = (Total_Season_W - min(Total_Season_W)) / (max(Total_Season_W) - min(Total_Season_W)))
  
  # Build model:
  ep_model <- nnet::multinom(Next_Score_Half ~ TimeSecs_Remaining + yrdline100 + down + log_ydstogo + GoalToGo + log_ydstogo*down + yrdline100*down + GoalToGo*log_ydstogo + Under_TwoMinute_Warning, 
                             data=train,weights = Total_Season_W_Scaled,maxit=300)
  
  # Return
  return(ep_model)
}

# Fit each season's model by holding out that season:

ep_model_09 <- build_LOSO_EP_Model(2009)
ep_model_10 <- build_LOSO_EP_Model(2010)
ep_model_11 <- build_LOSO_EP_Model(2011)
ep_model_12 <- build_LOSO_EP_Model(2012)
ep_model_13 <- build_LOSO_EP_Model(2013)
ep_model_14 <- build_LOSO_EP_Model(2014)
ep_model_15 <- build_LOSO_EP_Model(2015)
ep_model_16 <- build_LOSO_EP_Model(2016)
ep_model_17 <- build_LOSO_EP_Model(2017)

# Code to save the models is commented out because of the push limit:
# save(ep_model_09, file="ep_model_09.RData")
# save(ep_model_10, file="ep_model_10.RData")
# save(ep_model_11, file="ep_model_11.RData")
# save(ep_model_12, file="ep_model_12.RData")
# save(ep_model_13, file="ep_model_13.RData")
# save(ep_model_14, file="ep_model_14.RData")
# save(ep_model_15, file="ep_model_15.RData")
# save(ep_model_16, file="ep_model_16.RData")
# save(ep_model_17, file="ep_model_17.RData")

# ----------------------------------------------------------------------------

# Filter the pbp_data_next_score dataset to the rows needed for the field goal model:

fg_data <- filter(pbp_data_next_score,
                  PlayType %in% c("Field Goal","Extra Point","Run") & (!is.na(ExPointResult) | !is.na(FieldGoalResult)))

# Fit the field goal model:
fg_model <- mgcv::bam(sp ~ s(yrdline100),data=fg_data,family="binomial")

# Save the model (commented out due to file size limit)
# save(fg_model, file="fg_model.RData")

# Following code is used to build the models that hold
# the season out for predictions, and weights the observations
# by the difference in Season as well

# Define the build_LOSO_FG_Model() function that takes in
# season to holdout and the data:

build_LOSO_FG_Model <- function(holdout_season, data = fg_data){
  # Filter out the holdout season and down to the :
  train <- filter(data, Season != holdout_season)
  
  # Build model:
  fg_model <- mgcv::bam(sp ~ s(yrdline100),data=train,family="binomial")

  # Return
  return(fg_model)
}

# Fit each season's model by holding out that season:

fg_model_09 <- build_LOSO_FG_Model(2009)
fg_model_10 <- build_LOSO_FG_Model(2010)
fg_model_11 <- build_LOSO_FG_Model(2011)
fg_model_12 <- build_LOSO_FG_Model(2012)
fg_model_13 <- build_LOSO_FG_Model(2013)
fg_model_14 <- build_LOSO_FG_Model(2014)
fg_model_15 <- build_LOSO_FG_Model(2015)
fg_model_16 <- build_LOSO_FG_Model(2016)
fg_model_17 <- build_LOSO_FG_Model(2017)

# Code to save the models is commented out because of the push limit:
# save(fg_model_09, file="fg_model_09.RData")
# save(fg_model_10, file="fg_model_10.RData")
# save(fg_model_11, file="fg_model_11.RData")
# save(fg_model_12, file="fg_model_12.RData")
# save(fg_model_13, file="fg_model_13.RData")
# save(fg_model_14, file="fg_model_14.RData")
# save(fg_model_15, file="fg_model_15.RData")
# save(fg_model_16, file="fg_model_16.RData")
# save(fg_model_17, file="fg_model_17.RData")



