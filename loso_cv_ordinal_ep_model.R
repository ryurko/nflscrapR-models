# File generates the Leave-One-Season-Out cross validation calibration results
# for ordinal logistic regression model.

# Access tidyverse
# install.packages("tidyverse")
library(tidyverse)

# Access MASS
library(MASS)

# Load the pbp_ep_model_data generated in the init_ep_fg_models.R script:
# (NOTE this file was not pushed since due to the GitHub file size limit)
# (For some reason read_csv fails to read in the TimeSecs_Remaining values
# for 138 plays so we use read.csv instead)
pbp_ep_model_data <- read.csv("data/pbp_ep_model_data.csv") %>%
  mutate(down = factor(down))

# Reorder the levels of Next_Score_Half so that it goes from the most negative
# scoring event to the most positive, setting ordered = TRUE:
pbp_ep_model_data <- pbp_ep_model_data %>%
  mutate(Next_Score_Half = factor(Next_Score_Half,
                                  levels = c("Opp_Touchdown",
                                             "Opp_Field_Goal",
                                             "Opp_Safety",
                                             "No_Score",
                                             "Safety",
                                             "Field_Goal",
                                             "Touchdown"),
                                  ordered = TRUE))

#' Function to compute leave-one-season-out cross validation predictions
#' for the expected points model using ordinal logistic regression.
#' @param ep_formula Expected points model formula (as formula type).
#' @param weight_type What type of weighting scheme is used for the modeling,
#' with each number choice standing for the following (all of which are scaled
#' between 0 and 1):
#' \itemize{
#' \item{1} - Difference in drives between play and next scoring event
#' \item{2} - Absolute score differential
#' \item{3} - Combination of 1 and 2 (default option)
#' \item{4} - Season difference from holdout season
#' \item{5} - Combination of 1, 2, and 4
#' \item{6} - No weights
#' }
#' @param ep_model_data Play-by-play data ready for EP models assumed to have
#' the variables in the formula as well as Season, Drive_Score_Dist, and 
#' ScoreDiff for the possible weighting schemes.
#' @return Data frame with the next score probability predictions for each
#' of the scoring events for each held-out season.

calc_ep_ordinal_loso_cv <- function(ep_formula, weight_type = 3, 
                                     ep_model_data) {
  
  # Create vector of seasons to generate hold out results for:
  seasons <- unique(ep_model_data$Season)
  
  # Generate the predictions for each holdout season:
  map_dfr(seasons, 
          function(x) {
            
            # Separate test and training data:
            test_data <- ep_model_data %>%
              filter(Season == x)
            train_data <- ep_model_data %>%
              filter(Season != x)
            
            # Create the various weight columns only using
            # the train_data:
            train_data <- train_data %>%
              # 1 - drive difference
              mutate(Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) / 
                       (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
                     
                     # 2 - score differential
                     ScoreDiff_W = (max(abs(ScoreDiff)) - abs(ScoreDiff)) / 
                       (max(abs(ScoreDiff)) - min(abs(ScoreDiff))),
                     
                     # 3 - combo of 1 and 2
                     Total_W = Drive_Score_Dist_W + ScoreDiff_W,
                     Total_W_Scaled = (Total_W - min(Total_W)) / 
                       (max(Total_W) - min(Total_W)),
                     
                     # 4 - difference from holdout season
                     Season_Diff = abs(Season - x),
                     Season_Diff_W = (max(Season_Diff) - Season_Diff) / 
                       (max(Season_Diff) - min(Season_Diff)),
                     
                     # 5 - combo of 1, 2, and 4
                     Total_Season_W = Drive_Score_Dist_W + ScoreDiff_W + 
                       Season_Diff_W,
                     Total_Season_W_Scaled = (Total_Season_W - min(Total_Season_W)) / 
                       (max(Total_Season_W) - min(Total_Season_W)))
            
            # Type of weighting:
            if (weight_type == 1){
              # Drive difference weight
              train_data$model_weights <- train_data$Drive_Score_Dist_W
              
            } else if (weight_type == 2) {
              # Score differential weight
              train_data$model_weights <- train_data$ScoreDiff_W
              
            } else if (weight_type == 3) {
              # Combined weight
              train_data$model_weights <- train_data$Total_W_Scaled
              
            } else if (weight_type == 4) {
              # Season difference
              train_data$model_weights <- train_data$Season_Diff_W
              
            } else if (weight_type == 5) {
              # Combined with season
              train_data$model_weights <- train_data$Total_Season_W_Scaled
              
            } else {
              # No weights
              train_data$model_weights <- rep(1, nrow(train_data))
            }
            
            # Build model:
            ep_model <- polr(ep_formula, data = train_data,
                             weights = model_weights)
            
            # Generate and return prediction dataset (can add columns to
            # return from the test_data in the mutate function below but
            # only necessary variables are the predicted probabilities 
            # and the actual events):
            data.frame(predict(ep_model, newdata = test_data, type = "p")) %>%
              mutate(Next_Score_Half = test_data$Next_Score_Half) %>%
              return
            
          }) %>%
    return
}

# Create the LOSO predictions for the selected nflscrapR model:

ep_model_loso_preds <- calc_ep_ordinal_loso_cv(as.formula("Next_Score_Half ~ 
                                                           TimeSecs_Remaining + 
                                                           yrdline100 + down + 
                                                           log_ydstogo + GoalToGo + 
                                                           log_ydstogo*down + 
                                                           yrdline100*down + 
                                                           GoalToGo*log_ydstogo + 
                                                           Under_TwoMinute_Warning"),
                                                ep_model_data = pbp_ep_model_data)

# Use the following pipeline to create a dataset used for charting the
# cross-validation calibration results:

ep_cv_loso_calibration_results <- ep_model_loso_preds %>%
  # Create a row index column:
  mutate(play_index = 1:n()) %>%
  # Gather the columns for the different scoring event probabilities:
  gather(next_score_type, pred_prob, -Next_Score_Half, - play_index) %>%
  # Create binned probability column:
  mutate(bin_pred_prob = round(pred_prob / 0.05) * .05) %>%
  # Group by both the next_score_type and bin_pred_prob:
  group_by(next_score_type, bin_pred_prob) %>%
  # Calculate the calibration results:
  summarize(n_plays = n(), 
            n_scoring_event = length(which(Next_Score_Half == next_score_type)),
            bin_actual_prob = n_scoring_event / n_plays)


# Create a label data frame for the chart:
ann_text <- data.frame(x = c(.25, 0.75), y = c(0.75, 0.25), 
                       lab = c("More times\nthan expected", "Fewer times\nthan expected"),
                       next_score_type = factor("No Score"))

# Create the calibration chart:
ep_cv_loso_calibration_results %>%
  ungroup() %>%
  mutate(next_score_type = fct_relevel(next_score_type,
                                       "Opp_Field_Goal",
                                       "Opp_Safety","Opp_Touchdown",
                                       "Field_Goal","Safety","Touchdown",
                                       "No_Score"),
         next_score_type = fct_recode(next_score_type,
                                      "-Field Goal" = "Opp_Field_Goal",
                                      "-Safety" = "Opp_Safety",
                                      "-Touchdown" = "Opp_Touchdown",
                                      "Field Goal" = "Field_Goal",
                                      "No Score" = "No_Score")) %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +   geom_text(data = ann_text,aes(x = x, y = y, label = lab)) +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + 
  labs(title = "Leave-One-Season-Out Cross Validation Calibration for\nExpected Points Model by Scoring Event",
       size = "Number of plays",
       x = "Estimated Next Score Probability",
       y = "Observed Next Score Probability") + 
  #scale_color_brewer(palette = "Spectral", name = NULL, guide = FALSE) +
  geom_text(data = ann_text, aes(x = x, y = y, label = lab)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ next_score_type, ncol = 3)

# Calculate the calibration error values:  
cv_cal_error <- ep_cv_loso_calibration_results %>% 
  ungroup() %>%
  mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
  group_by(next_score_type) %>% 
  summarize(weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
            n_scoring_event = sum(n_scoring_event, na.rm = TRUE))

# Overall weighted calibration error:
with(cv_cal_error, weighted.mean(weight_cal_error, n_scoring_event))
# 0.02233487
