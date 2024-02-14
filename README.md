# On board games and how to build them: a study on the next best board game
A Nonparametric Statistics project by Stefania Colombo, Silvia D'Amicantonio, Federico Lancellotti.

## Data
The snapshot data can be found here: https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-25/readme.md
The raw historical data can be found here: https://github.com/beefsack/bgg-ranking-historicals
The preprocessed historical data can be found here: https://shorturl.at/hzGV8

## Analysis
The workflow is divided in three major parts: 
1. a model for the snapshot data, 
2. a model for the categories, exploiting the historical data
3. a survival approach

### 1. A model for the snapshot data
The main scripts can be found in the folders "analysis-snapshot" and "analysis-snapshot-gam".
Some preprocessing is performed.
Semiparametric and full nonparametric generalized additive models are fitted, discussing the effects of the categories (as dummy variable) and the characteristcs of the games (as numerical variables). For the final model, a robust approach is adopted discarding outlying observation via MCD.

### 2. A model for the categories
The main scripts can be found in the folder "analysis-historical".
A heavy preprocessing is performed, merging the raw datasets, cleaning the dataframe from NAs and inconsistences and smoothing the curves.
Two kinds of ANOVA tests are performed, fANOVA and ANOVA on the ranks, on both the curves and their derivative, to assess which categories are significative.
Some preliminary steps towards Functional Regression Analysis are also hinted.
Most of the tests are hardcoded and preformed in a permutational fashion.

### 3. Suvrival approach
The main scripts can be found in the folder "analysis-historical/survival".
The longevity of a game in the market is studied, as a function of both the relative categories and its gameplay characteristics.