library(roahd)
library(bggAnalytics)
library(dplyr)
library(tidyr)
library(sets)


# Import data ----
data <- read.csv("./data/usersRated.csv")


# Set of categories ----
categories <- factor(c())
for (i in 1:length(data)) {
  new_cat <- strsplit(data[i,]$Category, ", ")[[1]]
  categories <- c(categories, factor(new_cat))
}
categories <- unique(categories)
length(categories)  # 82


# Preprocess ----
## Fill Category ----
empty_cat.index <- which(data$Category == "")
to_remove <- c()
for (i in empty_cat.index) {
  if (data[i,]$Family == "")
    to_remove <- c(to_remove, i)
  else
    data[i,]$Category <- data[i,]$Family
}
data <- data[-to_remove,]  # remove rows without Category and Family

## Fill Family ----
empty_fam.index <- which(data$Family == "")
to_remove <- c()
for (i in empty_fam.index) {
  if (data[i,]$Family == "")
    to_remove <- c(to_remove, i)
  else {
    categories <- strsplit(data[i,]$Category, ", ")[[1]]
    data[i,]$Family <- categories[1]
  }
}
data <- data[-to_remove,]  # remove rows without Category and Family


## Fill NA with 0 ----
### (we should think about something better... look at the plots!)
data <- data %>%
  mutate_at(vars(-all_of(c("ID", "Year"))), ~replace(., is.na(.), 0))


## Sum grouping by Category ----
data.by_category <- data %>%
  group_by(Category) %>%
  summarise(across(where(is.numeric) & !c(ID, Year), sum))

data.by_family <- data %>%
  group_by(Family) %>%
  summarise(across(where(is.numeric) & !c(ID, Year), sum))


# Plots ----
length(unique(data.by_category$Category))
length(unique(data.family$Family))

P <- ncol(data.by_category) - 1
grid <-  seq(0, 1, length.out=P)

f_data <- fData(grid, log(data.by_category[, -1]))
plot(f_data)





