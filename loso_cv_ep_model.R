# File generates the Leave-One-Season-Out cross validation calibration results

# Access tidyverse
# install.packages("tidyverse")
library(tidyverse)

# Access nnet
# install.packages("nnet")
library(nnet)

# Load the pbp_ep_model_data generated in the init_ep_fg_models.R script:
# (NOTE this file was not pushed since due to the GitHub file size limit)
# (For some reason read_csv fails to read in the TimeSecs_Remaining values
# for 138 plays so we use read.csv instead)
pbp_ep_model_data <- read.csv("data/pbp_ep_model_data.csv") %>%
  mutate(down = factor(down))

#' Function to compute leave-one-season-out cross validation predictions
#' for the expected points model using multinomial logistic regression in nnet.
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

calc_ep_multinom_loso_cv <- function(ep_formula, weight_type = 3, 
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
            
            # Build model using maxit at 300 for now:
            ep_model <- nnet::multinom(ep_formula, 
                                       data = train_data,
                                       weights = model_weights, maxit = 300)
            
            # Generate and return prediction dataset (can add columns to
            # return from the test_data in the mutate function below but
            # only necessary variables are the predicted probabilities 
            # and the actual events):
            data.frame(predict(ep_model, newdata = test_data, type="probs")) %>%
              mutate(Next_Score_Half = test_data$Next_Score_Half) %>%
              return
            
  }) %>%
    return
}

# Create the LOSO predictions for the selected nflscrapR model:

ep_model_loso_preds <- calc_ep_multinom_loso_cv(as.formula("Next_Score_Half ~ 
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
                       next_score_type = factor("No Score (0)"))

# Create the calibration chart:
ep_cv_loso_calibration_results %>%
  ungroup() %>%
  mutate(next_score_type = fct_relevel(next_score_type,
                                       "Opp_Safety", "Opp_Field_Goal", 
                                       "Opp_Touchdown", "No_Score", "Safety",
                                       "Field_Goal", "Touchdown"
                                      ),
         next_score_type = fct_recode(next_score_type,
                                      "-Field Goal (-3)" = "Opp_Field_Goal",
                                      "-Safety (-2)" = "Opp_Safety",
                                      "-Touchdown (-7)" = "Opp_Touchdown",
                                      "Field Goal (3)" = "Field_Goal",
                                      "No Score (0)" = "No_Score",
                                      "Touchdown (7)" = "Touchdown")) %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +   geom_text(data = ann_text,aes(x = x, y = y, label = lab)) +
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(size = "Number of plays",
       x = "Estimated next score probability",
       y = "Observed next score probability") + 
  geom_text(data = ann_text, aes(x = x, y = y, label = lab)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 90),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.position = c(1, .05), legend.justification = c(1, 0)) +
  facet_wrap(~ next_score_type, ncol = 4)
  
# Calculate the calibration error values:  
cv_cal_error <- ep_cv_loso_calibration_results %>% 
  ungroup() %>%
  mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
  group_by(next_score_type) %>% 
  summarize(weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
            n_scoring_event = sum(n_scoring_event, na.rm = TRUE))
  
# Overall weighted calibration error:
with(cv_cal_error, weighted.mean(weight_cal_error, n_scoring_event))
# 0.01309723

# ------------------------------------------------------------------

# Need the mgcv package
# install.packages("mgcv")

#' Function to compute leave-one-season-out cross validation predictions
#' for the expected points model using multinomial logistic regression in nnet
#' and combining the predictions from the field goal model.
#' @param ep_formula Expected points model formula (as formula type).
#' @param fg_formula
#' @param weight_type What type of weighting scheme is used for the EP modeling,
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
#' of the scoring events for each held-out season using the logic for overriding
#' field goal attempts.

calc_ep_multinom_fg_loso_cv <- function(ep_formula, fg_formula, weight_type = 3, 
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
            
            # Build model using maxit at 300 for now:
            ep_model <- nnet::multinom(ep_formula, 
                                       data = train_data,
                                       weights = model_weights, maxit = 300)
            
            
            
            # Generate prediction dataset (can add columns to
            # return from the test_data in the mutate function below but
            # only necessary variables are the predicted probabilities 
            # and the actual events) from only using the ep_model
            ep_preds <- data.frame(predict(ep_model, 
                                           newdata = test_data, 
                                           type = "probs")) %>%
              mutate(Next_Score_Half = test_data$Next_Score_Half)
            
            # Build field goal model:
            fg_train_data <- train_data %>%
              filter(PlayType %in% c("Field Goal","Extra Point","Run") & 
                       (!is.na(ExPointResult) | !is.na(FieldGoalResult)))
            fg_model <- mgcv::bam(fg_formula, 
                                  data = fg_train_data, family = "binomial")
            
            # Now make a copy of the test data to then get EP probabilites 
            # as if the field goals were missed:
            missed_fg_test_data <- test_data %>%
              
              # Subtract 5.065401 from TimeSecs since average time for FG att:
              mutate(TimeSecs_Remaining = TimeSecs_Remaining - 5.065401,
                     
                     # Correct the yrdline100:
                     yrdline100 = 100 - (yrdline100 + 8),
                     
                     # Not GoalToGo:
                     GoalToGo = rep(0, n()),
                     
                     # Now first down:
                     down = rep("1", n()),
                     
                     # 10 yards to go
                     log_ydstogo = rep(log(10), n()),
                     
                     # Create Under_TwoMinute_Warning indicator
                     Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120,
                                                      1, 0))
            
            # Get the missed test data predictions:
            missed_fg_ep_preds <- data.frame(predict(ep_model, 
                                                     newdata = missed_fg_test_data, 
                                                     type = "probs"))
            
            # Find the rows where TimeSecs_Remaining became 0 or negative and 
            # make all the probs equal to 0:
            end_game_i <- which(missed_fg_test_data$TimeSecs_Remaining <= 0)
            missed_fg_ep_preds[end_game_i,] <- rep(0,
                                                   ncol(missed_fg_ep_preds))
            
            # Get the probability of making the field goal:
            make_fg_prob <- as.numeric(mgcv::predict.bam(fg_model, 
                                                         newdata = test_data, 
                                                         type = "response"))
            
            # Multiply each value of the missed_fg_ep_preds by the 1 - make_fg_prob
            missed_fg_ep_preds <- missed_fg_ep_preds * (1 - make_fg_prob)
            
            # Find the FG attempts in the test data:
            fg_attempt_i <- which(test_data$PlayType %in% c("Field Goal","Run") & 
                                    !is.na(test_data$FieldGoalResult))
            
            # Now update the probabilities for the FG attempts 
            # (also includes Opp_Field_Goal probability from missed_fg_ep_preds)
            ep_preds[fg_attempt_i, "Field_Goal"] <- make_fg_prob[fg_attempt_i] + 
              missed_fg_ep_preds[fg_attempt_i,"Opp_Field_Goal"]
            # Update the other columns based on the opposite possession:
            ep_preds[fg_attempt_i, "Touchdown"] <- 
              missed_fg_ep_preds[fg_attempt_i,"Opp_Touchdown"]
            ep_preds[fg_attempt_i, "Opp_Field_Goal"] <- 
              missed_fg_ep_preds[fg_attempt_i,"Field_Goal"]
            ep_preds[fg_attempt_i, "Opp_Touchdown"] <-
              missed_fg_ep_preds[fg_attempt_i,"Touchdown"]
            ep_preds[fg_attempt_i, "Safety"] <-
              missed_fg_ep_preds[fg_attempt_i,"Opp_Safety"]
            ep_preds[fg_attempt_i, "Opp_Safety"] <-
              missed_fg_ep_preds[fg_attempt_i,"Safety"]
            ep_preds[fg_attempt_i, "No_Score"] <- 
              missed_fg_ep_preds[fg_attempt_i,"No_Score"]
            
            return(ep_preds)
          }) %>%
    return
}


# Create the LOSO predictions for the selected nflscrapR models:
ep_fg_model_loso_preds <- calc_ep_multinom_fg_loso_cv(as.formula("Next_Score_Half ~ 
                                                           TimeSecs_Remaining + 
                                                           yrdline100 + down + 
                                                           log_ydstogo + GoalToGo + 
                                                           log_ydstogo*down + 
                                                           yrdline100*down + 
                                                           GoalToGo*log_ydstogo + 
                                                           Under_TwoMinute_Warning"),
                                                   as.formula("sp ~ s(yrdline100)"),
                                                ep_model_data = pbp_ep_model_data)

# Use the following pipeline to create a dataset used for charting the
# cross-validation calibration results:

ep_fg_cv_loso_calibration_results <- ep_fg_model_loso_preds %>%
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
ep_fg_cv_loso_calibration_results %>%
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
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
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
cv_fg_cal_error <- ep_fg_cv_loso_calibration_results %>% 
  ungroup() %>%
  mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
  group_by(next_score_type) %>% 
  summarize(weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
            n_scoring_event = sum(n_scoring_event, na.rm = TRUE))

# Overall weighted calibration error:
with(cv_fg_cal_error, weighted.mean(weight_cal_error, n_scoring_event))
# 0.01424929


