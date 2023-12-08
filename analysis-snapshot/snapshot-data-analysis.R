library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)


# Set global parameters -------------------------------------------------------
set.seed(1)
B <- 1e2


# Import the data -------------------------------------------------------------
data <- read.csv("../data-snapshot/boardgames-weights.csv")


# Exploration -----------------------------------------------------------------
with(data, scatterplotMatrix(data.frame(log(users_rated), average, weight)))

hist(log(data$users_rated))
hist(data$average)
hist(data$weight)
hist(data$minplayers)
hist(log(data$playingtime))
hist(data$minage)


# Categories once again -------------------------------------------------------
plot(data[data$Medieval==1,]$Economic)


# Publishers ------------------------------------------------------------------
publishers <- table(data$boardgamepublisher)
publishers <- sort(publishers, decreasing = TRUE)

publishers.big <- names(publishers[publishers >= 20])
publishers.big <- setdiff(publishers.big, c("['(Public Domain)']", "['(Self-Published)', '(Web published)']",
                                            "['(Self-Published)']", "['(Web published)']"))
sum(publishers[1:length(publishers.big)]) / nrow(data)

data$bigpublisher <- 0
data$bigpublisher[data$boardgamepublisher %in% publishers.big] <- 1


# Model -----------------------------------------------------------------------
catList.index <- which(colnames(data) == "Category") + 1
covariates.categories <- colnames(data[,catList.index:ncol(data)])
# covariates.categories <- paste0("s(", covariates.categories, ", bs='re')")

covariates.others <- c("average", "minplayers", "maxplayers", "weight", "log(playingtime+1)", "minage")
covariates.others <- paste0("s(", covariates.others, ", bs='tp', m=3)")

covariates <- paste(c(covariates.others, covariates.categories), collapse=" + ")

target <- "log(users_rated + 1)"
formula <- as.formula(paste(c(target, "~", covariates), collapse=" "))

model <- gam(formula, data=data)
summary(model)


# Variable selection ----------------------------------------------------------
covariates.categories.toremove <- c("Fan.Expansion", "Expansion.for.Base.game", "Korean.War",
                                    "Vietnam.War", "Mature...Adult", "Video.Game.Theme",
                                    "American.Revolutionary.War", "American.Indian.Wars",
                                    "Civil.War", "Game.System", "Religious", "Pike.and.Shot",
                                    "Arabian", "Environmental", "Music", "Comic.Book...Strip",
                                    "Movies...TV...Radio.theme", "Modern.Warfare", "Miniatures",
                                    "Post.Napoleonic", "Medical", "Transportation", "Murder.Mystery",
                                    "Pirates", "Trains", "World.War.II", "Trivia",
                                    "Word.Game", "Electronic", "Humor", "Space.Exploration",
                                    "Adventure", "American.West", "Racing", "Maze",
                                    "Collectible.Components", "Dice", "Mythology",
                                    "Travel", "Card.Game", "Negotiation",
                                    "Puzzle", "Industry...Manufacturing", "American.Civil.War",
                                    "Zombies")
covariates.categories.reduced <- setdiff(colnames(data[,catList.index:ncol(data)]),
                                         covariates.categories.toremove)
covariates.reduced <- paste(c(covariates.others, covariates.categories.reduced), collapse=" + ")
formula.reduced <- as.formula(paste(c(target, "~", covariates.reduced), collapse=" "))

model.reduced = gam(formula.reduced, data=data)
summary(model.reduced)
residuals.H0 <- model.reduced$residuals

T0 <- sum(abs(summary(model)$p.t[covariates.categories.toremove])) 
T0

T_H0 <- numeric(B)

for(perm in 1:B){
  permutation <- sample(nrow(data))
  
  residuals.H0.perm <- residuals.H0[permutation]
  Y.perm.H0 <- model.reduced$fitted + residuals.H0.perm
  
  formula.new <-  as.formula(paste(c("Y.perm.H0", "~", covariates), collapse=" "))
  T_H0[perm] <- sum(abs(summary(gam(formula.new, data=data))$p.t[covariates.categories.toremove]))
}

sum(T_H0>=T0)/B


# Model with interactions -----------------------------------------------------
covariates.categories.reduced <- setdiff(colnames(data[,catList.index:ncol(data)]),
                                         covariates.categories.toremove)

covariates.others <- c("average", "minplayers", "maxplayers", "weight", "log(playingtime+1)", "minage", "year")
covariates.others <- paste0("s(", covariates.others, ", bs='cr')")

covariates.interactions <- c("I(average*weight)", "I(average*log(playingtime+1))", "I(weight*log(playingtime+1))")
covariates.interactions <- paste0("s(", covariates.interactions, ", bs='cr')")

covariates <- paste(c(covariates.others, covariates.interactions, covariates.categories.reduced), collapse=" + ")

target <- "log(users_rated + 1)"
formula <- as.formula(paste(c(target, "~", covariates), collapse=" "))

model <- gam(formula, data=data)
summary(model)  # R-sq.(adj) =  0.272   Deviance explained = 27.6%
                # GCV = 1.5468  Scale est. = 1.5384    n = 21236


