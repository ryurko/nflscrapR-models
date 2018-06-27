# Create win probability chart for 2017 playoff game between the Kansas City
# Chiefs and the Tennessee Titans

# Access nflscrapR
# install.packages("devtools")
# devtools::install_github("maksimhorowitz/nflscrapR")
library(nflscrapR)

# Access tidyverse
# install.packages("tidyverse")
library(tidyverse)

# Access the teamcolors package by Ben Baumer and Greg Matthews:
# devtools::install_github("beanumber/teamcolors")
library(teamcolors)

# Get the KC color:
kc_color <- teamcolors %>%
  filter(league == "nfl", 
         name == "Kansas City Chiefs") %>%
  pull(primary)

# Get the TEN color:
ten_color <- teamcolors %>%
  filter(league == "nfl",
         name == "Tennessee Titans") %>%
  pull(primary)

# Extract the first game of the 2017 playoffs using nflscrapR, which corresponds
# to the game between Kansas City and Tennessee:
playoffs_2017 <- extracting_gameids(2017, playoffs = TRUE)
ten_kc_game_data <- game_play_by_play(playoffs_2017[1])

# Create the win probability chart for the game:
ten_kc_game_data %>%
  filter(!is.na(TimeSecs), !is.na(Home_WP_pre), !is.na(Away_WP_pre),
         !(PlayType %in% c("Timeout", "Quarter End", "Half End",
                           "Two Minute Warning"))) %>%
  ggplot(aes(x = TimeSecs)) + 
  geom_hline(yintercept = 0.5, color = "black") + 
  geom_line(aes(y = Away_WP_pre), color = ten_color, size = 2) + 
  geom_line(aes(y = Home_WP_pre), color = kc_color, size = 2) + 
  annotate("text", x = 900, y = 0.1, label = "TEN", color = ten_color, size = 8) + 
  annotate("text", x = 900, y = 0.9, label = "KC", color = kc_color, size = 8) + 
  scale_x_reverse(breaks = seq(0, 3600, 300)) + ylim(c(0, 1))+ 
  labs( x = "Time remaining in seconds", y = "Win probability") + theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))
