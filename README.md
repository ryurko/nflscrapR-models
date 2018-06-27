# `nflscrapR`-models repository

This repository contains the following folders for code inside the [R folder](https://github.com/ryurko/nflscrapR-models/tree/master/R):

+ [cmsac_posts](https://github.com/ryurko/nflscrapR-models/tree/master/R/cmsac_posts) -
code for posts on [Carnegie Mellon Sports Analytics Club site](https://www.cmusportsanalytics.com/nfl-expected-points-nflscrapr-part-1-introduction-expected-points/).
+ [init_models](https://github.com/ryurko/nflscrapR-models/tree/master/R/init_models) - 
code for creating the same expected points (EP), field goal, and win probability (WP) models.
that are used in the [`nflscrapR` package](https://github.com/maksimhorowitz/nflscrapR)
+ [legacy_model_code](https://github.com/ryurko/nflscrapR-models/tree/master/R/legacy_model_code) -
old version of code for creating `nflscrapR` models.
+ [loso_cv_calibration](https://github.com/ryurko/nflscrapR-models/tree/master/R/loso_cv_calibration) -
code for calculating the leave-one-season-out cross validation calibration results 
and generating the calibration plots in the [`nflWAR` paper](https://arxiv.org/abs/1802.00998)
for the `nflscrapR` models. Includes code for an alternate form of the EP model using
ordinal logistic regression, which yields dramatically worse calibration results.
+ [nflWAR_paper_figures_code](https://github.com/ryurko/nflscrapR-models/tree/master/R/nflWAR_paper_figures_code) -
code for generating the figures related to the EP and WP models seen in the [`nflWAR` paper](https://arxiv.org/abs/1802.00998).
