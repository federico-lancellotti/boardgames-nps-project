library(roahd)
library(dplyr)
library(tidyr)
library(fda)
library(fields)
library(progress)
library(fdANOVA)
library(ggplot2)

seed <- 2024
alpha <- 0.05

# Import data -----------------------------------------------------------------
data <- read.csv("../data-historical/usersRated_preproc.csv")

## How many categories
catList.index <- which(colnames(data) == "Category")
start.index <- which(colnames(data) == "X2016.10.12")
n_categories <- start.index - catList.index - 1
n_categories

N <- nrow(data)
P <- ncol(data) - 4 - n_categories  # from 2016.10.12 to 2023.11.04 should be 2579
abscissa <-  seq(0, 1, length.out=P)

## Bad rows
rows.all_equal <- apply(data[, start.index:ncol(data)], 1, function(x) all(x == x[start.index]))
which(rows.all_equal)
data.broken <- data[rows.all_equal,]
data <- data[-which(rows.all_equal),]

rows.all_equal <- apply(data[, start.index:ncol(data)], 1, function(x) all(x == x[start.index]))
which(rows.all_equal)  # if "named integer(0)", we are good


# Load smoothed data ----------------------------------------------------------
## Set parameters
m <- 5           # spline order 
degree <- m-1    # spline degree 
nbasis <- 10

## Create the basis ----
basis <- create.bspline.basis(rangeval=c(0,1), nbasis=nbasis, norder=m)
plot(basis)

t <- seq(0, 1, length.out=100)
y <- t(as.matrix(data[,start.index:ncol(data)]))


## Load data ----
data.smoothed <- readRDS("../data-historical/smoothed-data.RDS")
Xsp0 <- data.smoothed$Xsp0
Xsp1 <- data.smoothed$Xsp1
Xsp2 <- data.smoothed$Xsp2
err <- data.smoothed$err_idx


## Problematic curves ----
for (i in 1:length(err)) {
  Xsp <- smooth.basis(argvals=abscissa, y=y[,err[i]], fdParobj=basis)
  Xsp0[,err[i]] <- eval.fd(t, Xsp$fd) #  the curve smoothing the data
  Xsp1[,err[i]] <- eval.fd(t, Xsp$fd, Lfd=1) # first derivative
  Xsp2[,err[i]] <- eval.fd(t, Xsp$fd, Lfd=2) # second derivative
}


## Smoothed curves ----
Xsp0.fd <- fData(t, t(Xsp0))
Xsp0.fd.median <- median_fData(fData = Xsp0.fd, type = "MBD")

### First derivative ----
Xsp1.fd <- fData(t, t(Xsp1))
Xsp1.fd.median <- median_fData(fData = Xsp1.fd, type = "MBD")

### Second derivative ----
Xsp2.fd <- fData(t, t(Xsp2))
Xsp2.fd.median <- median_fData(fData = Xsp2.fd, type = "MBD")


# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# ANOVA for the ranks ---------------------------------------------------------
groups <- data[,(catList.index+1):(catList.index+n_categories)]
covariates <- paste(colnames(groups), collapse=" + ")

alpha <- 0.05

## Multi-way ANOVA with Hypograph, without interactions ----
### functions ----
modified_hypograph_index.0 <- MHI(Data = Xsp0.fd)

model0.formula <- as.formula(paste("modified_hypograph_index.0", covariates, sep=" ~ "))
model0.anova <- aov(model0.formula, data=data)
summ0 <- summary(model0.anova)

# covariates.reduced.0 <- names(which(summ0$coefficients[-1,4] <= alpha/length(colnames(groups))))
# covariates.reduced.0 <- paste(covariates.reduced.0, collapse=" + ")
# 
# model0.formula.reduced <- as.formula(paste("modified_hypograph_index.0", covariates.reduced.0, sep=" ~ "))
# model0.anova.reduced <- aov(model0.formula.reduced, data=data)
# summ0.reduced <- summary(model0.anova.reduced)
# summ0.reduced
# 
# good_categories.0 <- names(which(summ0$coefficients[-1,1] >= 0))

### derivatives ----
modified_hypograph_index.1 <- MHI(Data = Xsp1.fd)

model1.formula <- as.formula(paste("modified_hypograph_index.1", covariates, sep=" ~ "))
model1.anova <- lm(model1.formula, data=data)
summ1 <- summary(model1.anova)

# covariates.reduced.1 <- names(which(summ1$coefficients[-1,4] <= alpha/length(colnames(groups))))
# covariates.reduced.1 <- paste(covariates.reduced.1, collapse=" + ")
# 
# model1.formula.reduced <- as.formula(paste("modified_hypograph_index.1", covariates.reduced.1, sep=" ~ "))
# model1.anova.reduced <- lm(model1.formula.reduced, data=data)
# summ1.reduced <- summary(model1.anova.reduced)
# summ1.reduced
# 
# good_categories.1 <- names(which(summ1$coefficients[-1,1] >= 0))
# 
# good_categories <- intersect(good_categories.0, good_categories.1)


## Permutational multi-way ANOVA with Hypograph, without interactions ----
B <- 1000

permutational_anova <- function(y, groups, B) {
  covariates <- paste(colnames(groups), collapse=" + ")
  model.formula <- as.formula(paste("y", covariates, sep=" ~ "))
  model <- aov(model.formula, data=groups)
  summ <- summary(model)
  T0 <- summ[[1]]$`F value`[1:ncol(groups)]
  names(T0) <- colnames(groups)
  
  T_stat <- matrix(nrow=B, ncol=ncol(groups))
  colnames(T_stat) <- colnames(groups)
  
  pb <- progress_bar$new(format="  processing [:bar] :percent eta: :eta", total=B, clear=F)
  set.seed(seed)
  for(perm in 1:B){
    # Permutation:
    permutation <- sample(1:length(y))
    y_perm <- y[permutation]
    model.formula.perm <- as.formula(paste("y_perm", covariates, sep=" ~ "))
    model.perm <- aov(model.formula.perm, data=groups)
    summ.perm <- summary(model.perm)
    
    # Test statistic:
    T_stat[perm,] <- summ.perm[[1]]$`F value`[1:ncol(groups)]
    
    pb$tick()
  }
  
  pvalues <- rowMeans(t(T_stat) >= T0)
  names(pvalues) <- colnames(groups)
  
  output <- list(T0=T0, T_stat=T_stat, pvalues=pvalues)
}

### functions ----
anova0 <- permutational_anova(modified_hypograph_index.0, groups, B)

good_categories.0 <- names(which(anova0$pvalues <= alpha/ncol(groups)))
groups0.reduced <- groups[, which(colnames(groups) %in% good_categories.0)]
anova0.reduced <- permutational_anova(modified_hypograph_index.0, groups0.reduced, B)

good_categories.0 <- names(which(anova0.reduced$pvalues <= alpha/ncol(groups0.reduced)))
groups0.reduced <- groups[, which(colnames(groups) %in% good_categories.0)]
anova0.reduced <- permutational_anova(modified_hypograph_index.0, groups0.reduced, B)

good_categories.0 <- names(which(anova0.reduced$pvalues <= alpha/ncol(groups0.reduced)))
good_categories.0

### derivatives ----
anova1 <- permutational_anova(modified_hypograph_index.1, groups, B)

good_categories.1 <- names(which(anova1$pvalues <= alpha/ncol(groups)))
groups1.reduced <- groups[, which(colnames(groups) %in% good_categories.1)]
anova1.reduced <- permutational_anova(modified_hypograph_index.1, groups1.reduced, B)

good_categories.1 <- names(which(anova1.reduced$pvalues <= alpha/ncol(groups1.reduced)))

### intersection ----
good_categories <- intersect(good_categories.0, good_categories.1)
covariates.good <- paste(good_categories, collapse=" + ")
good_categories

# > good_categories
# [1] "Economic"               "Fantasy"                "Medieval"               "Ancient"               
# [5] "Territory.Building"     "Civilization"           "City.Building"          "Exploration"           
# [9] "Farming"                "Bluffing"               "Science.Fiction"        "Collectible.Components"
# [13] "Fighting"               "Print...Play"           "Renaissance"            "Horror"                
# [17] "Novel.based"            "Aviation...Flight"      "Real.time"              "Book"  


## Multi-way ANOVA with Hypograph, with interactions ----
cat <- good_categories[20]
interactions <- c()
for (other_cat in good_categories) {
  newcomb <- paste(cat, other_cat, sep=":")
  interactions <- c(interactions, newcomb)
}
covariates.int <- paste(interactions, collapse=" + ")
covariates.winteractions <- paste(covariates.good, covariates.int, sep=" + ")


### functions ----
model0.interactions.formula <- as.formula(paste("modified_hypograph_index.0", covariates.winteractions, sep=" ~ "))
model0.interactions.anova <- aov(model0.interactions.formula, data=data)
summ0.interactions <- summary(model0.interactions.anova)
summ0.interactions

# Economic:Territory.Building
# Medieval:Ancient, Medieval:Renaissance
# Ancient:City.Building
# Territory.Building:Exploration, Territory.Building:Print...Play
# Exploration:Print...Play
# Science.Fiction:Collectible.Components
# Print...Play:Real.time


### derivatives ----
model1.interactions.formula <- as.formula(paste("modified_hypograph_index.1", covariates.winteractions, sep=" ~ "))
model1.interactions.anova <- aov(model1.interactions.formula, data=data)
summ1.interactions <- summary(model1.interactions.anova)
summ1.interactions

# Aviation...Flight è significativo solo nella versione permutational per le derivate

# Fantasy:Fighting
# Medieval:Ancient
# Territory.Building:Farming, Territory.Building:Bluffing, Territory.Building:Print...Play
# Civilization:City.Building
# Exploration:Print...Play
# Bluffing:Horror
# Collectible.Components:Fighting, Collectible.Components:Aviation...Flight


## Permutational multi-way ANOVA with Hypograph, with interactions ----
interactions.0 <- c("Economic:Territory.Building",
                  "Medieval:Ancient", "Medieval:Renaissance",
                  "Ancient:City.Building",
                  "Territory.Building:Exploration", "Territory.Building:Print...Play",
                  "Exploration:Print...Play",
                  "Science.Fiction:Collectible.Components",
                  "Print...Play:Real.time")

interactions.1 <- c("Fantasy:Fighting",
                    "Medieval:Ancient",
                    "Territory.Building:Farming", "Territory.Building:Bluffing", "Territory.Building:Print...Play",
                    "Civilization:City.Building", 
                    "Exploration:Print...Play", 
                    "Bluffing:Horror",
                    "Collectible.Components:Fighting", "Collectible.Components:Aviation...Flight")

interactions <- union(interactions.0, interactions.1)

groups.reduced <- groups[, which(colnames(groups) %in% good_categories)]

p <- length(good_categories) + length(interactions)

permutational_anova.interactions <- function(y, groups, interactions, B) {
  covariates.alone <- paste(colnames(groups), collapse=" + ")
  covariates.interactions <- paste(interactions, collapse=" + ")
  covariates <- paste(covariates.alone, covariates.interactions, sep=" + ")
  p <- ncol(groups) + length(interactions)
  
  model.formula <- as.formula(paste("y", covariates, sep=" ~ "))
  model <- aov(model.formula, data=groups)
  summ <- summary(model)
  T0 <- summ[[1]]$`F value`[1:p]
  names(T0) <- c(colnames(groups), interactions)

  T_stat <- matrix(nrow=B, ncol=p)
  colnames(T_stat) <- names(T0)
  
  pb <- progress_bar$new(format="  processing [:bar] :percent eta: :eta", total=B, clear=F)
  set.seed(seed)
  for(perm in 1:B){
    # Permutation:
    permutation <- sample(1:length(y))
    y_perm <- y[permutation]
    model.formula.perm <- as.formula(paste("y_perm", covariates, sep=" ~ "))
    model.perm <- aov(model.formula.perm, data=groups)
    summ.perm <- summary(model.perm)

    # Test statistic:
    T_stat[perm,] <- summ.perm[[1]]$`F value`[1:p]

    pb$tick()
  }

  pvalues <- numeric(length(T0))
  for (i in 1:length(T0)) {
    pvalues[i] <- sum(T_stat[,i] >= T0[i])/B
  }
  names(pvalues) <- names(T0)

  output <- list(T0=T0, T_stat=T_stat, pvalues=pvalues)
}

anova0.interactions <- permutational_anova.interactions(modified_hypograph_index.0, groups.reduced, interactions, B)
which(anova0.interactions$pvalues <= alpha/p)

anova1.interactions <- permutational_anova.interactions(modified_hypograph_index.1, groups.reduced, interactions, B)
which(anova1.interactions$pvalues <= alpha/p)





