# OLD CODE FILE FOR GENERATING EP AND FG MODELS BASED ON LOSO CODE

# Following code is used to build the models that hold
# the season out for predictions, and weights the observations
# by the difference in Season as well

# Define the build_LOSO_EP_Model() function that takes in
# season to holdout and the data:

build_LOSO_EP_Model <- function(holdout_season, data = pbp_ep_model_data){
  # Filter out the holdout season:
  train <- data %>%
    filter(Season != holdout_season)
  
  # Generate the weights
  
  # Create a weight column based on difference in drives between play and next score:
  train <- train %>% 
    mutate(Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) / 
             (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
           # Create a weight column based on score differential:
           ScoreDiff_W = (max(abs(ScoreDiff)) - abs(ScoreDiff)) / 
             (max(abs(ScoreDiff)) - min(abs(ScoreDiff))),
           # Create a weight column based on season difference from holdout season:
           Season_Diff = abs(Season - holdout_season),
           Season_Diff_W = (max(Season_Diff) - Season_Diff) / 
             (max(Season_Diff) - min(Season_Diff)),
           # Add the three together and scale again:
           Total_Season_W = Drive_Score_Dist_W + ScoreDiff_W + Season_Diff_W,
           Total_Season_W_Scaled = (Total_Season_W - min(Total_Season_W)) / 
             (max(Total_Season_W) - min(Total_Season_W)))
  
  # Build model:
  ep_model <- nnet::multinom(Next_Score_Half ~ TimeSecs_Remaining + 
                               yrdline100 + down + log_ydstogo + GoalToGo + 
                               log_ydstogo*down + yrdline100*down + 
                               GoalToGo*log_ydstogo + Under_TwoMinute_Warning, 
                             data = train, weights = Total_Season_W_Scaled, 
                             maxit = 300)
  
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

# Code to save the models is commented out because of the push limit:
# save(ep_model_09, file="ep_model_09.RData")
# save(ep_model_10, file="ep_model_10.RData")
# save(ep_model_11, file="ep_model_11.RData")
# save(ep_model_12, file="ep_model_12.RData")
# save(ep_model_13, file="ep_model_13.RData")
# save(ep_model_14, file="ep_model_14.RData")
# save(ep_model_15, file="ep_model_15.RData")
# save(ep_model_16, file="ep_model_16.RData")

# ----------------------------------------------------

# Following code is used to build the models that hold
# the season out for predictions, and weights the observations
# by the difference in Season as well

# Define the build_LOSO_FG_Model() function that takes in
# season to holdout and the data:

build_LOSO_FG_Model <- function(holdout_season, data = fg_data){
  # Filter out the holdout season and down to the :
  train <- data %>%
    filter(Season != holdout_season)
  
  # Build model:
  fg_model <- mgcv::bam(sp ~ s(yrdline100), data = train,
                        family = "binomial")
  
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

# Code to save the models is commented out because of the push limit:
# save(fg_model_09, file="fg_model_09.RData")
# save(fg_model_10, file="fg_model_10.RData")
# save(fg_model_11, file="fg_model_11.RData")
# save(fg_model_12, file="fg_model_12.RData")
# save(fg_model_13, file="fg_model_13.RData")
# save(fg_model_14, file="fg_model_14.RData")
# save(fg_model_15, file="fg_model_15.RData")
# save(fg_model_16, file="fg_model_16.RData")

