library(tidyverse)
library(ggplot2)

# Create plot of Carter's expected points model:
carter_data <- as.data.frame(list("YardsAway"=c(95,85,75,65,55,45,35,25,15,5),
                                  "EP"=c(-1.245,-.637,.236,.923,1.538,2.392,3.167,3.681,4.572,6.041)))
carter_data$Model <- "Carter"

ggplot(carter_data, aes(x=YardsAway,y=EP)) + geom_line() + geom_point() + geom_text(aes(label=EP),vjust = 0, nudge_y = 0.5) +
  xlab("Yards from Opponent's End Zone") + ylab("Expected Points") + theme_bw() +
  scale_x_continuous(breaks=seq(from=5,to=95,by=10)) +
  labs(title="Virgil Carter's Expected Points Model")

# Add Hidden Game of Football model:
hgf_data <- as.data.frame(list("YardsAway"=c(100,75,50,25,0),
                               "EP"=c(-2,0,2,4,6)))
hgf_data$Model <- "Hidden Game of Football"

ggplot(bind_rows(carter_data,hgf_data), 
       aes(x=YardsAway,y=EP,color=Model)) + geom_line() + 
  geom_point() + 
  xlab("Yards from Opponent's End Zone") + ylab("Expected Points") + theme_bw() +
  scale_x_continuous(breaks=seq(from=5,to=95,by=10)) +
  labs(title="Expected Points Model Comparison: Carter vs Hidden Game of Football") +
  scale_color_brewer(palette = "Set1")

# At this point load the pbp_data from the build_ep_models.R file
# and display the expected points relationship, comparing to the 
# other two:

# Load and stack the play-by-play data:
# pbp_data <- rbind(readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2009.csv"),
#                   readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2010.csv"),
#                   readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2011.csv"),
#                   readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2012.csv"),
#                   readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2013.csv"),
#                   readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2014.csv"),
#                   readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2015.csv"),
#                   readr::read_csv("~/Documents/nflscrapR-data/data/season_play_by_play/pbp_2016.csv"))

pbp_chart_data <- pbp_data %>% filter(!is.na(ExpPts))  %>% rename(EP = ExpPts, YardsAway = yrdline100) %>%
  mutate(Model = "nflscrapR")

ggplot(filter(pbp_chart_data,!is.na(down)),aes(x=YardsAway,y=EP,color=as.factor(down))) + 
  geom_smooth() + xlab("Yards from Opponent's End Zone") + ylab("Expected Points Value") +
  theme_bw() + 
  scale_y_continuous(limits = c(-4,6),breaks=seq(-4, 6, 2)) + 
  labs(title="Expected Points Model Comparison: nflscrapR versus Carter and Hidden Game of Football") +
  geom_line(data=bind_rows(carter_data,hgf_data),aes(x=YardsAway,y=EP,color=Model)) + 
  geom_point(data=bind_rows(carter_data,hgf_data),aes(x=YardsAway,y=EP,color=Model)) + 
  scale_x_continuous(breaks=seq(from=5,to=95,by=10)) +
  scale_color_brewer(palette = "Dark2",name="Model",
                     labels=c("nflscrapR - 1st down",
                              "nflscrapR - 2nd down",
                              "nflscrapR - 3rd down",
                              "nflscrapR - 4th down",
                              "Carter","Hidden Game of Football"))

# View the event probabilities

pbp_chart_data_2 <- pbp_chart_data %>% select(YardsAway, down, No_Score_Prob,
                                              Touchdown_Prob, Field_Goal_Prob,
                                              Safety_Prob, Opp_Field_Goal_Prob,
                                              Opp_Safety_Prob, Opp_Touchdown_Prob) %>%
  gather(key=Event,value=Probability,-YardsAway, -down)

ggplot(filter(pbp_chart_data_2,!is.na(down)), aes(x=YardsAway,y=Probability,color=Event)) + 
  geom_smooth() + ylim(0,1) + theme_bw() + facet_wrap(~down,ncol=2) +
  xlab("Yards from Opponent's End Zone") + ylab("Expected Points Value") +
  labs(title="Relationship between Next Score Probabilities and Field Position by Down")+
  scale_color_brewer(palette = "Dark2",name="Next Score Type",
                     labels=c("Field Goal",
                              "No Score",
                              "Opponent Field Goal",
                              "Opponent Safety",
                              "Opponent Touchdown","Safety","Touchdown"))





