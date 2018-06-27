# File generates the Leave-One-Season-Out cross validation calibration results
# for the win probability model

# Access tidyverse
# install.packages("tidyverse")
library(tidyverse)

# Access mgcv
# install.packages("mgcv")
library(mgcv)

# Load the pbp_wp_model_data generated in the init_wp_model.R script:
# (NOTE this file was not pushed since due to the GitHub file size limit)
pbp_wp_model_data <- read.csv("data/pbp_wp_model_data.csv") %>%
  mutate(down = factor(down))

#' Function to compute leave-one-season-out cross validation predictions
#' for the win probability model using a GAM.
#' @param wp_formula Win probability model formula (as formula type).
#' @param wp_model_data Play-by-play data ready for WP model assumed to have
#' the variables in the formula.
#' @return Data frame with the win probability predictions for each held-out
#' season.

calc_wp_gam_loso_cv <- function(wp_formula, wp_model_data) {
  
  # Create vector of seasons to generate hold out results for:
  seasons <- unique(wp_model_data$Season)
  
  # Generate the predictions for each holdout season:
  map_dfr(seasons, 
          function(x) {
            
            # Separate test and training data:
            test_data <- wp_model_data %>%
              filter(Season == x)
            train_data <- wp_model_data %>%
              filter(Season != x)
            
            # Build model:
            wp_model <- bam(wp_formula, data = train_data, family = "binomial")
            
            # Generate and return prediction dataset (can add columns to
            # return from the test_data in the mutate function below but
            # only necessary variables are the predicted probabilities 
            # and the actual events):
            data.frame(win_prob = predict(wp_model, newdata = test_data, 
                                          type = "response")) %>%
              mutate(win_ind = test_data$Win_Indicator,
                     qtr = test_data$qtr) %>%
              return
            
          }) %>%
    return
}

# Create the LOSO predictions for the selected nflscrapR model:
wp_model_loso_preds <- calc_wp_gam_loso_cv(as.formula("Win_Indicator ~ 
                                                       s(ExpScoreDiff) + 
                                                       s(TimeSecs_Remaining, by = Half_Ind) + 
                                                       s(ExpScoreDiff_Time_Ratio) + 
                                                       Under_TwoMinute_Warning*posteam_timeouts_pre*Half_Ind + 
                                                       Under_TwoMinute_Warning*oppteam_timeouts_pre*Half_Ind"),
                                            wp_model_data = pbp_wp_model_data)

# Use the following pipeline to create a dataset used for charting the
# cross-validation calibration results:

wp_cv_loso_calibration_results <- wp_model_loso_preds %>%
  # Create binned probability column:
  mutate(bin_pred_prob = round(win_prob / 0.05) * .05) %>%
  # Group by both the qtr and bin_pred_prob:
  group_by(qtr, bin_pred_prob) %>%
  # Calculate the calibration results:
  summarize(n_plays = n(), 
            n_wins = length(which(win_ind == 1)),
            bin_actual_prob = n_wins / n_plays)


# Create a label data frame for the chart:
ann_text <- data.frame(x = c(.25, 0.75), y = c(0.75, 0.25), 
                       lab = c("More times\nthan expected", "Fewer times\nthan expected"),
                       qtr = factor("1st Quarter"))

# Create the calibration chart:
wp_cv_loso_calibration_results %>%
  ungroup() %>%
  mutate(qtr = fct_recode(factor(qtr), "1st Quarter" = "1", "2nd Quarter" = "2",
                               "3rd Quarter" = "3", "4th Quarter" = "4")) %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() + geom_text(data = ann_text,aes(x = x, y = y, label = lab)) +
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(size = "Number of plays",
       x = "Estimated win probability",
       y = "Observed win probability") + 
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
        legend.position = "bottom") +
  facet_wrap(~ qtr, ncol = 4)

# Calculate the calibration error values:  
wp_cv_cal_error <- wp_cv_loso_calibration_results %>% 
  ungroup() %>%
  mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
  group_by(qtr) %>% 
  summarize(weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
            n_wins = sum(n_wins, na.rm = TRUE))

# Overall weighted calibration error:
with(wp_cv_cal_error, weighted.mean(weight_cal_error, n_wins))
# 0.01309723

