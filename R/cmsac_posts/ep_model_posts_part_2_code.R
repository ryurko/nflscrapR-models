library(tidyverse)
library(ggplot2)

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


pbp_ep_model_data$Next_Score_Points_Half <- with(pbp_ep_model_data,
                                                 ifelse(Next_Score_Half == "Touchdown",7,
                                                        ifelse(Next_Score_Half == "Opp_Field_Goal",-3,
                                                               ifelse(Next_Score_Half == "Opp_Safety",-2,
                                                                      ifelse(Next_Score_Half == "Opp_Touchdown",-7,
                                                                             ifelse(Next_Score_Half == "Field_Goal",3,
                                                                                    ifelse(Next_Score_Half == "Safety",2,0)))))))


# Create the dataset for the bar chart:
next_score_chart_df <- pbp_ep_model_data %>% select(Next_Score_Half,Next_Score_Points_Half)
next_score_chart_df$Next_Score_Half <- factor(next_score_chart_df$Next_Score_Half,
                                         levels = c("Opp_Touchdown","Opp_Field_Goal","Opp_Safety",
                                                    "No_Score","Safety","Field_Goal","Touchdown"))

next_score_chart_df$Who <- with(next_score_chart_df, ifelse(Next_Score_Half %in% c("Touchdown","Field_Goal","Safety"),
                                                            "For",
                                                            ifelse(Next_Score_Half %in% c("Opp_Touchdown","Opp_Field_Goal","Opp_Safety"),"Against","No Score")))

ggplot(next_score_chart_df,aes(Next_Score_Half)) + geom_bar(aes(fill=Who,alpha=abs(Next_Score_Points_Half)),color="black") + coord_flip() +
  scale_fill_manual(values=alpha(c("red","blue","white")),
                    guide = guide_legend(title = NULL)) + 
  scale_alpha_continuous(guide=FALSE) + theme_bw() + scale_x_discrete(labels=c("Touchdown"="Touchdown (7)",
                                                                               "Field_Goal"= "Field Goal (3)",
                                                                               "Safety"="Safety (2)",
                                                                               "No_Score"="No Score (0)",
                                                                               "Opp_Safety"="-Safety (-2)",
                                                                               "Opp_Field_Goal"="-Field Goal (-3)",
                                                                               "Opp_Touchdown"="-Touchdown (-7)")) + 
  ylab("Number of Plays") + xlab("Type of Next Score") +
  labs(title="Type of Next Score for All Plays from 2009-16", subtitle= "(with respect to possession team)") + 
  theme(plot.title = element_text(hjust = 0.5,size=20),plot.subtitle = element_text(hjust = 0.5,size=16),
        axis.text.x = element_text(size=16), axis.text.y = element_text(size=16),
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        legend.text = element_text(size=16))


next_score_chart_df$Next_Score_Half <- factor(next_score_chart_df$Next_Score_Half,
                                              levels = c("Opp_Touchdown","-Six","-Five",
                                                         "-Four","Opp_Field_Goal","Opp_Safety","-One",
                                                         "No_Score","One","Safety","Field_Goal",
                                                         "Four","Five","Six","Touchdown"))

ggplot(next_score_chart_df,aes(Next_Score_Half)) + geom_bar(aes(fill=Who,alpha=abs(Next_Score_Points_Half)),color="black") + coord_flip() +
  scale_fill_manual(values=alpha(c("red","blue","white")),
                    guide = guide_legend(title = NULL)) + 
  scale_alpha_continuous(guide=FALSE) + theme_bw() + scale_x_discrete(labels=c("Touchdown"="Touchdown (7)",
                                                                                "Six" = "",
                                                                                 "Five" = "",
                                                                                 "Four" = "",
                                                                               "Field_Goal"= "Field Goal (3)",
                                                                               "Safety"="Safety (2)",
                                                                              "One" = "",
                                                                               "No_Score"="No Score (0)",
                                                                               "-One" = "",
                                                                               "Opp_Safety"="-Safety (-2)",
                                                                               "Opp_Field_Goal"="-Field Goal (-3)",
                                                                               "-Four" = "",
                                                                               "-Five" = "",
                                                                               "-Six" = "",
                                                                               "Opp_Touchdown"="-Touchdown (-7)"),
                                                                      drop=FALSE) + 
  ylab("Number of Plays") + xlab("Type of Next Score") +
  labs(title="Type of Next Score for All Plays from 2009-16", subtitle= "(with respect to possession team)") + 
  theme(plot.title = element_text(hjust = 0.5,size=20),plot.subtitle = element_text(hjust = 0.5,size=16),
        axis.text.x = element_text(size=16), axis.text.y = element_text(size=16),
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        legend.text = element_text(size=16))


# Regression residual diagnostics:

# First initialize the variables to be the same in the current EP model:
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

# Fit linear regression model:
ep_reg <- lm(Next_Score_Points_Half ~ TimeSecs_Remaining + yrdline100 + down + log_ydstogo + GoalToGo + log_ydstogo*down + yrdline100*down + GoalToGo*log_ydstogo + Under_TwoMinute_Warning,
             data=pbp_ep_model_data)

# Create a dataframe for the residual chart:
reg_resid_chart_df <- cbind(as.data.frame(list("Fitted"=fitted(ep_reg),
                                               "Residuals"=resid(ep_reg))),
                            next_score_chart_df[which(!is.na(pbp_ep_model_data$TimeSecs_Remaining)),])
# Create the chart:
ggplot(reg_resid_chart_df,aes(x=Fitted,y=Residuals,fill=Next_Score_Points_Half)) + 
  scale_fill_gradient2(low="red",mid="white",high="blue",breaks=c(-7,-3,-2,0,2,3,7),
                       labels=c("-Touchdown (-7)","-Field Goal (-3)","-Safety (-2)","No Score (0)","Safety (2)","Field Goal (3)","Touchdown (7)"),
                       guide = guide_legend(title=NULL,reverse = TRUE)) +
  geom_point(color="black",pch=21, alpha=.7,size=4) + theme_bw() + 
  labs(title="Residuals vs Fitted of Linear Regression Expected Points Model") + 
  xlab("Fitted Values (Expected Points)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept=0,color="black",lty=2)

