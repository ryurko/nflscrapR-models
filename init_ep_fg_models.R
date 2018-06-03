# File for building EP models (and their corresponding FG models)

# Access tidyverse:
# install.packages("tidyverse")
library(tidyverse)

# Access nflWAR:
# install.packages("devtools")
# devtools::install.packages("ryurko/nflWAR")

library(nflWAR)

# Load data from 2009 to 2016 from the nflscrapR-data repository:
pbp_data <- get_pbp_data(2009:2016)

# Remove error game from 2011 that is coded incorrectly in raw JSON data:
pbp_data <- pbp_data %>% filter(GameID != "2011121101")

nrow(pbp_data)
# 362263

#' Define a function that takes in a play-by-play data set and returns
#' what the type of next score is and the drive number only within same half.
#' @param pbp_dataset Play-by-play dataset with the following columns:
#' sp - scoring play indicator, PlayType - what type of play, qtr - quarter
#' of the game, Drive - drive number for the play, ReturnResult - indicates
#' what happened on return type plays, posteam - indicates the possession team
#' for the play, and columns for ReturnResult, FieldGoalResult, ExPointResult,
#' TwoPointConv, DefTwoPoint, Touchdown.
#' @return Data frame with two columns: Next_Score_Half denoting the type of 
#' the next scoring event occurring within the half of each play and 
#' Drive_Score_Half denoting the drive number of the next scoring play.

find_game_next_score_half <- function(pbp_dataset) {
  
  # Which rows are the scoring plays:
  score_plays <- which(pbp_dataset$sp == 1 & pbp_dataset$PlayType != "No Play")
  
  # Define a helper function that takes in the current play index, 
  # a vector of the scoring play indices, play-by-play data,
  # and returns the score type and drive number for the next score:
  find_next_score <- function(play_i, score_plays_i,pbp_df) {
    
    # Find the next score index for the current play
    # based on being the first next score index:
    next_score_i <- score_plays_i[which(score_plays_i >= play_i)[1]]
    
    # If next_score_i is NA (no more scores after current play)
    # or if the next score is in another half,
    # then return No_Score and the current drive number
    if (is.na(next_score_i) | 
        (pbp_df$qtr[play_i] %in% c(1, 2) & pbp_df$qtr[next_score_i] %in% c(3, 4, 5)) | 
        (pbp_df$qtr[play_i] %in% c(3, 4) & pbp_df$qtr[next_score_i] == 5)) {
          
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
      if (identical(pbp_df$ReturnResult[next_score_i], "Touchdown")) {
        
        # For return touchdowns the current posteam would not have
        # possession at the time of return, so it's flipped:
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Opp_Touchdown"
          
        } else {
          
          score_type <- "Touchdown"
          
        }
      } else if (identical(pbp_df$FieldGoalResult[next_score_i], "Good")) {
        
        # 2: Field Goal
        # Current posteam made FG
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Field_Goal"
          
          # Opponent made FG
        } else {
          
          score_type <- "Opp_Field_Goal"
          
        }
        
        # 3: Touchdown (returns already counted for)
      } else if (pbp_df$Touchdown[next_score_i] == 1) {
        
        # Current posteam TD
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Touchdown"
          
        # Opponent TD
        } else {
          
          score_type <- "Opp_Touchdown"
          
        }
        # 4: Safety (similar to returns)
      } else if (pbp_df$Safety[next_score_i] == 1) {
        
        if (identical(pbp_df$posteam[play_i],pbp_df$posteam[next_score_i])) {
          
          score_type <- "Opp_Safety"
          
        } else {
          
          score_type <- "Safety" 
          
        }
        # 5: Extra Points
      } else if (identical(pbp_df$ExPointResult[next_score_i], "Made")) {
        
        # Current posteam Extra Point
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Extra_Point"
          
          # Opponent Extra Point
        } else {
          
          score_type <- "Opp_Extra_Point"
          
        }
        # 6: Two Point Conversions
      } else if (identical(pbp_df$TwoPointConv[next_score_i], "Success")) {
        
        # Current posteam Two Point Conversion
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Two_Point_Conversion"
          
        # Opponent Two Point Conversion
        } else {
          
          score_type <- "Opp_Two_Point_Conversion"
          
        }
        
        # 7: Defensive Two Point (like returns)
      } else if (identical(pbp_df$DefTwoPoint[next_score_i], "Success")) {
        
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Opp_Defensive_Two_Point"
          
        } else {
          
          score_type <- "Defensive_Two_Point"
          
        }
        
        # 8: Errors of some sort so return NA (but shouldn't take place)
      } else {
        
        score_type <- NA
        
      }
    }
    
    return(data.frame(Next_Score_Half = score_type,
                      Drive_Score_Half = score_drive))
  }
  
  # Using lapply and then bind_rows is much faster than
  # using map_dfr() here:
  lapply(c(1:nrow(pbp_dataset)), find_next_score, 
         score_plays_i = score_plays, pbp_df = pbp_dataset) %>%
    bind_rows() %>%
    return
}

# Apply to each game (ignore the warning messages here):
pbp_next_score_half <- map_dfr(unique(pbp_data$GameID), 
                               function(x) {
                                 pbp_data %>%
                                   filter(GameID == x) %>%
                                   find_game_next_score_half()
                               })

# Join to the pbp_data:
pbp_data_next_score <- bind_cols(pbp_data, pbp_next_score_half)

# Create the EP model dataset that only includes plays with basic seven 
# types of next scoring events along with the following play types:
# Field Goal, No Play, Pass, Punt, Run, Sack, Spike

pbp_ep_model_data <- pbp_data_next_score %>% 
  filter(Next_Score_Half %in% c("Opp_Field_Goal", "Opp_Safety", "Opp_Touchdown",
                                "Field_Goal", "No_Score", "Safety", "Touchdown") & 
        PlayType %in% c("Field Goal", "No Play", "Pass", "Punt", "Run", "Sack",
                        "Spike") & is.na(TwoPointConv) & is.na(ExPointResult) &
        !is.na(down))

nrow(pbp_ep_model_data)
# 304896

# Now adjust and create the model variables:
pbp_ep_model_data <- pbp_ep_model_data %>%
  
         # Reference level should be No_Score:
  mutate(Next_Score_Half = fct_relevel(factor(Next_Score_Half), "No_Score"),
         
         # Create a variable that is time remaining until end of half:
         # (only working with up to 2016 data so can ignore 2017 time change)
         TimeSecs_Remaining = ifelse(qtr %in% c(1,2), TimeSecs - 1800,
                                      ifelse(qtr == 5, TimeSecs + 900, 
                                             TimeSecs)),
         
         # log transform of yards to go and indicator for two minute warning:
         log_ydstogo = log(ydstogo),
         Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120, 1, 0),
         
         # Changing down into a factor variable: 
         down = factor(down),
         
         # Calculate the drive difference between the next score drive and the 
         # current play drive:
         Drive_Score_Dist = Drive_Score_Half - Drive,
         
         # Create a weight column based on difference in drives between play and next score:
         Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) / 
           (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
         # Create a weight column based on score differential:
         ScoreDiff_W = (max(abs(ScoreDiff)) - abs(ScoreDiff)) / 
           (max(abs(ScoreDiff)) - min(abs(ScoreDiff))),
         # Add these weights together and scale again:
         Total_W = Drive_Score_Dist_W + ScoreDiff_W,
         Total_W_Scaled = (Total_W - min(Total_W)) / 
           (max(Total_W) - min(Total_W)))

# Save dataset in data folder as pbp_ep_model_data.csv
# (NOTE: this dataset is not pushed due to its size exceeding
# the github limit but will be referenced in other files)
# write_csv(pbp_ep_model_data, "data/pbp_ep_model_data.csv")

# Fit the expected points model:
# install.packages("nnet")
ep_model <- nnet::multinom(Next_Score_Half ~ TimeSecs_Remaining + yrdline100 + 
                           down + log_ydstogo + GoalToGo + log_ydstogo*down + 
                           yrdline100*down + GoalToGo*log_ydstogo + 
                           Under_TwoMinute_Warning, data = pbp_ep_model_data, 
                           weights = Total_W_Scaled, maxit = 300)

# Save the model (commented out due to file size limit)
# save(ep_model, file="ep_model.RData")

# ----------------------------------------------------------------------------

# Filter the pbp_data_next_score dataset for the field goal model:
fg_model_data <-  pbp_data_next_score %>% 
  filter(PlayType %in% c("Field Goal","Extra Point","Run") & 
           (!is.na(ExPointResult) | !is.na(FieldGoalResult)))

nrow(fg_model_data)
# 16906

# Save dataset in data folder as fg_model_data.csv
# (NOTE: this dataset is not pushed due to its size exceeding
# the github limit but will be referenced in other files)
# write_csv(fg_model_data, "data/fg_model_data.csv")

# Fit the field goal model:
# install.packages("mgcv")
fg_model <- mgcv::bam(sp ~ s(yrdline100), 
                      data = fg_model_data, family = "binomial")

# Save the model (commented out due to file size limit)
# save(fg_model, file="fg_model.RData")




