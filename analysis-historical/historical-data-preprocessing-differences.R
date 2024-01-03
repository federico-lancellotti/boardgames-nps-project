library(progress)
library(lubridate)

# Import data -----------------------------------------------------------------
data <- read.csv("../data-historical/usersRated_preproc.csv")


# Generate dataset of the differences -----------------------------------------
## References ----
index.start <- which(colnames(data) == "X2016.10.12")
index.end <- ncol(data)
M <- index.end - index.start
head(data[,c(index.start, index.end)])

## Preparation ----
usersRated_diff <- data

generate_diff <- function(i) {
  newcol <- data[,i] - data[,i-1]
  idx <- which(newcol > 0)
  newcol <- ifelse(newcol < 0, 0, newcol)
}

## Computation ----
for (i in (index.start+1):index.end) {
  usersRated_diff[,i] <- generate_diff(i)
}


# Aggregation by week ---------------------------------------------------------
## Get knot dates ----
get_weekday <- function(col_name) {
  day <- gsub("X", "", col_name)
  day <- as.Date(day, format = "%Y.%m.%d")
  weekdays(day)
}

days <- colnames(data)[index.start:index.end]
idx.names <- days[1]
for (d in days) {
  if (get_weekday(d) == "Monday") {
    idx.names <- c(idx.names, d)
  }
}
idx.names <- c(idx.names, days[length(days)])
idx.names <- unique(idx.names)

idx <- which(colnames(data) %in% idx.names)

## Computations ----
usersRated_diff_weekly <- data[,1:index.start]

generate_diff_weakly <- function(i, j) {
  newcol <- data[,j] - data[,i]
  idx <- which(newcol > 0)
  newcol <- ifelse(newcol < 0, 0, newcol)
}

for (i in 2:length(idx)) {
  idx.1 <- idx[i-1]
  idx.2 <- idx[i]
  
  newcol <- generate_diff_weakly(idx.1, idx.2)
  newcol.name <- idx.names[i]
    
  usersRated_diff_weekly <- usersRated_diff_weekly %>% mutate(!!newcol.name := newcol)
}


# Save the new datasets -------------------------------------------------------
write.csv(usersRated_diff, file = "../data-historical/usersRated_diff.csv", row.names = FALSE)
write.csv(usersRated_diff_weekly, file = "../data-historical/usersRated_diff_weekly.csv", row.names = FALSE)

