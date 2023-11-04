library(tidytuesdayR)
library(bggAnalytics)


# Import of the data ----
boardgames <- tidytuesdayR::tt_load('2022-01-25')
ratings <- boardgames$ratings
details <- boardgames$details


# Histograms ----
hist(ratings$average)

vars <- bgg_variables
