# Script for generating the expected points related figures in the nflWAR paper,
# excluding the LOSO CV results since those are generated in the loso_cv_ep_model.R file.

# Access tidyverse:
# install.packages("tidyverse")
library(tidyverse)

# Load the pbp_ep_model_data generated in the init_ep_fg_models.R script:
# (NOTE this file was not pushed since due to the GitHub file size limit)
pbp_ep_model_data <- read.csv("data/pbp_ep_model_data.csv") %>%
  mutate(down = factor(down))

# ----------------------------------------------------------------------
# Next score distribution bar chart:

# Create a column with the associated point value:
pbp_ep_model_data %>%
  mutate(Next_Score_Points_Half = case_when(
                                    Next_Score_Half == "Touchdown" ~ 7,
                                    Next_Score_Half == "Opp_Field_Goal" ~ -3,
                                    Next_Score_Half == "Opp_Safety" ~ -2,
                                    Next_Score_Half == "Opp_Touchdown" ~ -7,
                                    Next_Score_Half == "Field_Goal" ~ 3,
                                    Next_Score_Half == "Safety" ~ 2,
                                    TRUE ~ 0
                                  ),
         # Variable for who scored:
         Who = case_when(
                 Next_Score_Half %in% c("Touchdown", "Field_Goal", 
                                        "Safety") ~ "For",
                 Next_Score_Half %in% c("Opp_Touchdown", "Opp_Field_Goal",
                                        "Opp_Safety") ~ "Against",
                 TRUE ~ "No Score"
               ),
         # Relevel the Next_Score_Half to have the additional levels for plotting:
         Next_Score_Half = factor(Next_Score_Half,
                                  levels = c("Opp_Touchdown", "-Six", "-Five",
                                             "-Four", "Opp_Field_Goal", 
                                             "Opp_Safety", "-One",
                                             "No_Score", "One", "Safety", 
                                             "Field_Goal", "Four", "Five", 
                                             "Six", "Touchdown"),
                                  ordered = TRUE)) %>%
  dplyr::select(Next_Score_Half, Next_Score_Points_Half, Who) %>%
  # Create the chart:
  ggplot(aes(x = Next_Score_Half, fill = Who, 
             alpha = abs(Next_Score_Points_Half))) + geom_bar(color = "black") + 
  coord_flip() +
  scale_fill_manual(values = alpha(c("darkorange4", "darkslateblue", "white")),
                    guide = guide_legend(title = NULL)) + 
  scale_alpha_continuous(guide = FALSE) + theme_bw() + 
  scale_x_discrete(labels=c("Touchdown" = "Touchdown (7)", "Six" = "", "Five" = "",
                            "Four" = "", "Field_Goal" = "Field Goal (3)",
                            "Safety" = "Safety (2)", "One" = "",
                            "No_Score" = "No Score (0)", "-One" = "",
                            "Opp_Safety" = "-Safety (-2)", 
                            "Opp_Field_Goal" = "-Field Goal (-3)",  "-Four" = "",
                            "-Five" = "", "-Six" = "",
                            "Opp_Touchdown" = "-Touchdown (-7)"), drop = FALSE) + 
  labs(x = "Type of next score", y = "Number of plays") + 
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 20))

# -----------------------------------------------------------------------------
# Generate the figure showing the distribution for the score differential and
# the difference in drives from the next scoring event:

score_diff_hist <- pbp_ep_model_data %>%
  ggplot(aes(x = abs(ScoreDiff))) + 
  geom_histogram(aes(y=..count.. / sum(..count..)), 
                 fill = "darkslateblue", color = "white") +
  ylab("Proportion") + xlab("Absolute Score Differential") + 
  labs(x = "Absolute score differential", 
       y = "Proportion") +
  theme_bw() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))

# Exclude plays where that did not have a scoring event:
drive_diff_hist <- pbp_ep_model_data %>%
  filter(Next_Score_Half != "No_Score") %>%
  ggplot(aes(x = Drive_Score_Dist)) + 
  geom_histogram(aes(y = ..count.. / sum(..count..)), 
                 binwidth = 1, fill = "darkorange4", color = "white") +
  labs(x = "Difference between next score drive and current play drive",
       y = "Proportion") + 
  theme_bw() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))

# Use cowplot plot_grid:
# install.packages("cowplot")
library(cowplot)

plot_grid(score_diff_hist, drive_diff_hist, labels = c("A", "B"), 
          ncol = 2, label_size = 18)

# -----------------------------------------------------------------------------

# Display plots showing the relationships between the expected points and 
# yards from end zone, both by down and broken up by the scoring event:


# Access nflWAR:
# install.packages("devtools")
# devtools::install.packages("ryurko/nflWAR")
library(nflWAR)

# Load data from 2009 to 2016 from the nflscrapR-data repository using the
# get_pbp_data() function from the nflWAR package:
pbp_data <- get_pbp_data(2009:2016)

# Remove error game from 2011 that is coded incorrectly in raw JSON data:
pbp_data <- pbp_data %>% filter(GameID != "2011121101")

# Expected points relationships, for the historical models,
# first the Carter model:
carter_data <- data.frame("yards_away" = c(95, 85, 75, 65, 55, 45, 35, 25, 15, 5),
                          "ep" = c(-1.245, -.637, .236, .923, 1.538, 2.392, 
                                   3.167, 3.681, 4.572, 6.041)) %>%
  mutate(model = "Carter")

# and Hidden Game of Football model:
hgf_data <- data.frame("yards_away" = c(100, 75, 50, 25, 0),
                       "ep" = c(-2, 0, 2, 4, 6)) %>%
  mutate(model = "Hidden Game of Football")

pbp_chart_data <- pbp_data %>% 
  filter(!is.na(ExpPts)) %>% 
  rename(ep = ExpPts, yards_away = yrdline100) %>%
  mutate(model = "nflscrapR")

# Display the nflscrapR model results by down and then compare to the historical
# models from Carter and the Hidden Game of Football:
pbp_chart_data %>%
  filter(!is.na(down)) %>%
  ggplot(aes(x = yards_away, y = ep,
             color = as.factor(down))) + 
  geom_smooth(size = 2) + 
  labs(x = "Yards from opponent's end zone",
       y = "Expected points value",
       color = "Model") +
  theme_bw() + 
  scale_y_continuous(limits = c(-4,6),breaks = seq(-4, 6, 2)) + 
  geom_line(data = bind_rows(carter_data, hgf_data),
            aes(x = yards_away, y = ep, color = model),
            size = 2, linetype = "dashed") + 
  geom_point(data = bind_rows(carter_data, hgf_data),
             aes(x = yards_away, y = ep, color = model),
             size = 5, alpha = 0.5) + 
  scale_x_continuous(breaks = seq(from = 5, to = 95, by = 10)) +
  scale_color_manual(values = c("#0000FF",
                                "#5537AA",
                                "#AA6E55",
                                "#FFA500",
                                "seagreen4",
                                "darkred"),
                     labels = c("nflscrapR - 1st down",
                                "nflscrapR - 2nd down",
                                "nflscrapR - 3rd down",
                                "nflscrapR - 4th down",
                                "Carter",
                                "Hidden Game of Football")) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.position = "bottom")

# Create the facetted version for each events probability based on down:
pbp_chart_data %>% 
  filter(!is.na(down)) %>%
  dplyr::select(yards_away, down, No_Score_Prob,
                Touchdown_Prob, Field_Goal_Prob,
                Safety_Prob, Opp_Field_Goal_Prob,
                Opp_Safety_Prob, Opp_Touchdown_Prob) %>%
  gather(key = event, value = probability, -yards_away, -down) %>%
  mutate(event_value = case_when(
                         event == "No_Score_Prob" ~ 0,
                         event == "Touchdown_Prob" ~ 7,
                         event == "Field_Goal_Prob" ~ 3,
                         event == "Safety_Prob" ~ 2,
                         event == "Opp_Field_Goal_Prob" ~ -3,
                         event == "Opp_Safety_Prob" ~ -2,
                         TRUE ~ -7),
         down_label = case_when(
                        down == 1 ~ "1st down",
                        down == 2 ~ "2nd down",
                        down == 3 ~ "3rd down",
                        TRUE ~ "4th down")) %>%
  ggplot(aes(x = yards_away, y = probability, color = event_value,
             group = event)) + 
  geom_smooth(se = FALSE) + ylim(0,1) + facet_wrap( ~ down_label, ncol = 4) + 
  theme_bw() +
  labs(x = "Yards from opponent's end zone", y = "Predicted probability") +
  scale_color_gradient2(low = "darkorange4", mid = "gray",
                        high = "darkslateblue", 
                        breaks = c(-7, -3, -2, 0, 2, 3, 7),
                        labels=c(" -Touchdown (-7) ", " -Field Goal (-3) ",
                                 " -Safety (-2) ", " No Score (0) ",
                                 " Safety (2) ", " Field Goal (3) ", 
                                 " Touchdown (7) "),
                        guide = guide_legend(title = NULL, ncol = 7,
                                             reverse = TRUE,
                                             override.aes = list(size = 5))) +
  theme(legend.background = element_rect(fill = "white"),
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18))


