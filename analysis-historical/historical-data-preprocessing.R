library(progress)


# Import data -----------------------------------------------------------------
usersRated <- read.csv("../data-historical/usersRated.csv")


# Family ----------------------------------------------------------------------
# families.names <- factor(c())
# for (i in 1:nrow(usersRated)) {
#   new_fam <- strsplit(usersRated[i,]$Family, ", ")[[1]]
#   families.names <- c(families.names, factor(new_fam))
# }
# families.names <- unique(families.names)
# length(families.names)  # 8


# Categories ------------------------------------------------------------------
## (~5min)
N <- nrow(usersRated)
pb <- progress_bar$new(
  format = "  processing [:bar] :percent eta: :eta",
  total = N,
  clear = FALSE)

categories.names <- factor(c())
for (i in 1:N) {
  new_cat <- strsplit(usersRated[i,]$Category, ", ")[[1]]
  categories.names <- c(categories.names, factor(new_cat))
  pb$tick()
}
categories.names <- unique(categories.names)
length(categories.names)  # 84


categories <- usersRated[,c(1:3, 5)]
for (name in categories.names) {
  categories[[name]] <- 0
}


for (i in 1:nrow(categories)) {
  cat_list <- strsplit(categories[i,]$Category, ", ")[[1]]
  for (cat in cat_list) {
    categories[i,cat] <- 1
  }
}


# New dataset -----------------------------------------------------------------
## Discard the family and keep the new dummy variables.
data <- merge(categories, usersRated[,c(1,6:ncol(usersRated))], by="ID")


# Remove NAs ------------------------------------------------------------------
## NAs in categories
empty_cat.index <- which(data$Category == "")   # rows with empty category
data <- data[-empty_cat.index, ]  # remove NAs

vert_sum <- colSums(data[,5:88])
sort(vert_sum, decreasing = TRUE)  # number of entries for each category

## NAs in usersRated on the first day (2016/10/12)
idx <- which(is.na(data$X2016.10.12))
data$X2016.10.12[idx] <- 0

## NAs in usersRated after the first day (~40min)
start_col <- which(colnames(data) == "X2016.10.12") + 1

pb <- progress_bar$new(
  format = "  processing [:bar] :percent eta: :eta",
  total = ncol(data) - start_col + 1,
  clear = FALSE)

for (j in start_col:ncol(data)) {
  idx.na <- which(is.na(data[,j]))
  for (i in idx.na) {
    data[i,j] <- data[i, j-1]
  }
  
  idx.minor <- which(data[,j] < data[,j-1])
  for (i in idx.minor) {
    data[i,j] <- data[i, j-1]
  }
  
  pb$tick()
}

## Check
which(is.na(data$X2016.10.13))


# Save the preprocessed data --------------------------------------------------
write.csv(data, file = "../data-historical/usersRated_preproc.csv", row.names = FALSE)

