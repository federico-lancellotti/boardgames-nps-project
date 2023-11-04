library(tidytuesdayR)
library(bggAnalytics)
library(useful)


# Import of the data ----
starting_date <- as.Date("2023-11-04")
filepath <- "bgg-ranking-historicals/"
filename <- paste(filepath, toString(starting_date), ".csv", sep="")
current_historical <- read.csv(filename)


# Build the dataset skeleton for the number of ratings ----
usersRated <- data.frame(current_historical$ID, current_historical$Name, current_historical$Year, current_historical$Users.rated)
names(usersRated)[1] <- "ID"
names(usersRated)[2] <- "Name"
names(usersRated)[3] <- "Year"
names(usersRated)[4] <- toString(starting_date)

# Build the dataset skeleton for the rank ----
Rank <- data.frame(current_historical$ID, current_historical$Name, current_historical$Year, current_historical$Rank)
names(Rank)[1] <- "ID"
names(Rank)[2] <- "Name"
names(Rank)[3] <- "Year"
names(Rank)[4] <- toString(starting_date)

# Build the dataset skeleton for the average ----
Average <- data.frame(current_historical$ID, current_historical$Name, current_historical$Year, current_historical$Average)
names(Average)[1] <- "ID"
names(Average)[2] <- "Name"
names(Average)[3] <- "Year"
names(Average)[4] <- toString(starting_date)


# Build the Category column ----
### WARNING: we are collecting data from the internet and the process may fail,
###         throwing an http error. To ease the process, I divided it in a few 
###         chunks of 10,000 rows.
###         If it fails, just run it again.

Category <- data.frame(ID=integer(), Category=character())  # empty dataframe

store_categories <- function(df, start, finish) {
  # Downloads the info about the category from the online database and stores
  # them in a copy of the passed dataframe.
  # Returns a modified copy of the dataframe with columns ID and Category, 
  # ordered by ID.
  
  games <- bggGames$new(ids = usersRated[start:finish,]$ID)
  new_cat <- games$fetch(c("objectid","category"))
  
  for (i in 1:length(new_cat[[1]])) {
    id <- as.numeric(new_cat[[1]][i])
    category <- paste(unlist(new_cat[[2]][i]), collapse = ", ")
    new_row <- data.frame(ID = id, Category = category)
    df <- rbind(df, new_row)
  }
  
  return(df)
}

Category <- store_categories(Category, 1, 10000)
Category <- store_categories(Category, 10001, 20000)
Category <- store_categories(Category, 20001, nrow(usersRated))

# Merge of the datasets by ID
usersRated <- merge(usersRated, Category, by = "ID")
Rank       <- merge(Rank, Category, by = "ID")


### NOTE: the elements of the column "Category" are strings, and not lists of
###       strings. Therefore, they should be converted in lists of strings in
###       order to better handle them.
###       The following script provides a list of strings:

# list_of_strings <- strsplit(string, ", ")
# list_of_strings <- list_of_strings[[1]]
# print(list_of_strings)


## Check of the category ----
### We randomly chose N games and check if the recorded category is the same
### as the true category stored on the internet.
N <- 5
idx <- sample(1:nrow(usersRated), N)
for (i in idx) {
  id <- usersRated[i,]$ID
  game <- bggGames$new(ids = id)
  category1 <- game$fetch("category")
  category1 <- paste(unlist(category1[[1]]), collapse = ", ")
  category2 <- usersRated[i,]$Category
  print(category1 == category2)
}


# Historical data ----
filepath <- "bgg-ranking-historicals"
list_of_files <- list.files(filepath, pattern = ".csv", full.names = TRUE)
filenames <- tools::file_path_sans_ext(basename(list_of_files))
filenames <- setdiff(filenames, toString(starting_date))

for (f in filenames) {
  filename <- filename <- paste(filepath, "/", f, ".csv", sep="")
  current_historical <- read.csv(filename)
  
  new_col.Ratings <- data.frame(current_historical$ID, current_historical$Users.rated)
  names(new_col.Ratings)[1] <- "ID"
  names(new_col.Ratings)[2] <- f
  
  new_col.Rank <- data.frame(current_historical$ID, current_historical$Rank)
  names(new_col.Rank)[1] <- "ID"
  names(new_col.Rank)[2] <- f
  
  new_col.Average <- data.frame(current_historical$ID, current_historical$Average)
  names(new_col.Average)[1] <- "ID"
  names(new_col.Average)[2] <- f
  
  usersRated <- merge(usersRated, new_col.Ratings, by = "ID", all.x = TRUE)
  Rank <- merge(Rank, new_col.Rank, by = "ID", all.x = TRUE)
  Average <- merge(Average, new_col.Average, by = "ID", all.x = TRUE)
}

# Remove duplicates
usersRated <- usersRated[!duplicated(usersRated$ID), ]
Rank       <- Rank[!duplicated(Rank$ID), ]
Average    <- Average[!duplicated(Average$ID), ]

# Move the most recent column at the end
usersRated <- usersRated[, c(1:3, 5:ncol(usersRated), 4)]
Rank       <- Rank[, c(1:3, 5:ncol(Rank), 4)]
Average    <- Average[, c(1:3, 5:ncol(Average), 4)]


# Save the data ----
write.csv(usersRated, file = "data/usersRated.csv", row.names = TRUE)
write.csv(Rank, file = "data/Rank.csv", row.names = TRUE)
write.csv(Average, file = "data/Average.csv", row.names = TRUE)

