library(tidytuesdayR)
library(bggAnalytics)


# Import of the data ----------------------------------------------------------
boardgames <- tidytuesdayR::tt_load('2022-01-25')
ratings <- boardgames$ratings
details <- boardgames$details

historical.usersRated <- read.csv("../data-historical/usersRated_preproc.csv")

ratings.cols = c(1:3, 5:6, 8)
details.cols = c(2, 5:8, 11, 19, 20)
data <- merge(ratings[, ratings.cols], details[, details.cols], by="id")

historical.cols = c(1, 4:88)
data <- merge(data, historical.usersRated[, historical.cols], by.x="id", by.y="ID", all.x=TRUE)
data <- data[!is.na(data$Category),]    # WARNING: 267 rows removed. It may be better to reconsider them.

## Check
sums <- rowSums(data[, 15:98])
sum(sums == 0)


# Weights (aka complexity of the game) ----------------------------------------
Weights <- data.frame(id=integer(), weight=integer())  # empty dataframe

store_weights <- function(df, start, finish) {
  # Downloads the info about the weight from the online database and stores
  # them in a copy of the passed dataframe.
  # Returns a modified copy of the dataframe with columns ID and weight, 
  # ordered by ID.
  
  games <- bggGames$new(ids = data[start:finish,]$id)
  new_weight <- games$fetch(c("objectid","avgweight"))
  
  for (i in 1:length(new_weight[[1]])) {
    id <- as.numeric(new_weight[[1]][i])
    weight <- as.numeric(new_weight[[2]][i])
    new_row <- data.frame(id = id, weight = weight)
    df <- rbind(df, new_row)
  }
  
  return(df)
}

Weights <- store_weights(Weights, 1, 5000)
Weights <- store_weights(Weights, 5001, 10000)
Weights <- store_weights(Weights, 10001, 15000)
Weights <- store_weights(Weights, 15001, 20000)
Weights <- store_weights(Weights, 20001, nrow(data))

sum(is.na(Weights$weight))

# Merge of the dataset by ID
data <- merge(data, Weights, by = "id")
data <- data[, c(1:11, ncol(data), 13:ncol(data)-1)]
data <- data[!is.na(data$weight),]    # WARNING: 128 rows removed.


## Check of the weight ----
### We randomly chose N games and check if the recorded weight is the same
### as the true weight stored on the internet.
N <- 20
idx <- sample(1:nrow(data), N)
for (i in idx) {
  id <- data[i,]$id
  game <- bggGames$new(ids = id)
  weight1 <- game$fetch("avgweight")
  weight2 <- data[i,]$weight
  if (weight1 != weight2)
    print(paste("real: ", weight1, ", saved: ", weight2))
  else
    print("OK.")
}

# Export of the data ----------------------------------------------------------
write.csv(data, file = "../data-snapshot/boardgames-weights.csv", row.names = FALSE)


# Publishers ------------------------------------------------------------------
data <- read.csv("../data-snapshot/boardgames-weights.csv")

## Remove games without a publisher (only 1 row)
data <- data[!is.na(data$boardgamepublisher),]

## Parsing function
library(jsonlite)
# publishers <- "['GP Games', 'Lakeside', \"OPEN'N PLAY\", 'Pelikan']"
# publishers <- data$boardgamepublisher[data$id == 25729]
# publishers <- data$boardgamepublisher[data$id == 235512]

parsePublishers <- function(publishers){
  publishers <- gsub("^\\[|\\]$", "", publishers)
  publishers <- unlist(strsplit(publishers, "', |, '"))
  publishers <- gsub("'", "", publishers)
  publishers <- gsub("[\"']", "", publishers)

  return(publishers)
}

## Get unique publishers ----
publishers.names <- factor(c())
for (i in 1:nrow(data)) {
  new_pub <- data$boardgamepublisher[i]
  parsed_list <- parsePublishers(new_pub)
  publishers.names <- unique(c(publishers.names, parsed_list))
}
publishers.names <- unique(publishers.names)
length(publishers.names)

## Dummy variables for each publisher ----
publishers <- data[,c(1, 3, 13)]
for (name in publishers.names) {
  publishers[[name]] <- 0
}

for (i in 1:nrow(publishers)) {
  pub_list <- parsePublishers(data$boardgamepublisher[i])
  
  for (pub in pub_list) {
    publishers[i,pub] <- 1
  }
}

## Check
data[!complete.cases(data), ]

## Frequencies ----
publishers.freq <- colSums(publishers[,4:ncol(publishers)])
publishers.freq <- sort(publishers.freq, decreasing = TRUE)
publishers.freq[1:20]

selfpublished <- c("(Public Domain)", "(Self-Published)", "(Web published)")
publishers.freq[selfpublished] <- 0
sort(publishers.freq, decreasing = FALSE)[1:20]

for (i in 1:nrow(data)) {
  pub_list <- pub_list <- parsePublishers(data$boardgamepublisher[i])
  
  data$dimpublisher[i] <- sum(publishers.freq[pub_list])  # sum of the frequencies of each contributing publisher
  avgpublisher <- mean(publishers.freq[pub_list])         # average of the frequencies
  data$avgpublisher[i] <- ifelse(is.na(avgpublisher), 0, avgpublisher)
}

## Check
data[!complete.cases(data), ]

## Move the new columns
data <- data[, c(1:13, ncol(data)-1, ncol(data), 16:ncol(data)-2)]


# Export of the data ----------------------------------------------------------
write.csv(data, file = "../data-snapshot/boardgames-weights-publishers.csv", row.names = FALSE)


