library(ISLR2)
library(car)
library(mgcv)
# library(rgl)
library(splines)
library(pbapply)


# Set global parameters -------------------------------------------------------
set.seed(1)
B <- 1e2


# Import the data -------------------------------------------------------------
data <- read.csv("../data-snapshot/boardgames-weights-publishers.csv")
data[!complete.cases(data), ]


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


# Model -----------------------------------------------------------------------
catList.index <- which(colnames(data) == "Category") + 1
covariates.categories <- colnames(data[,catList.index:ncol(data)])
# covariates.categories <- paste0("s(", covariates.categories, ", bs='re')")

covariates.others <- c("average", "minplayers", "maxplayers", "weight", "log(playingtime+1)", "minage", "dimpublisher", "avgpublisher")
covariates.others <- paste0("s(", covariates.others, ", bs='tp', m=3)")

covariates <- paste(c(covariates.others, covariates.categories), collapse=" + ")

target <- "log(users_rated + 1)"
formula <- as.formula(paste(c(target, "~", covariates), collapse=" "))

model <- gam(formula, data=data)
summary(model)


# Variable selection ----------------------------------------------------------
covariates.categories.toremove <- c("Travel", "Mythology", "Print...Play",
                                    "Maze", "Racing", "American.West", 
                                    "Space.Exploration", "Novel.based",
                                    "Party.Game", "Real.time", "World.War.II",
                                    "Pirates", "Sports", "Number", "Medical",
                                    "Music", "Pike.and.Shot", "Vietnam.War",    
                                    "Korean.War", "Expansion.for.Base.game", 
                                    "Fan.Expansion")
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

covariates.others <- c("maxplayers", "weight", "log(playingtime+1)", "minage", "year", "dimpublisher", "avgpublisher")
covariates.others <- paste0("s(", covariates.others, ", bs='cr')")

covariates.interactions <- c("I(year*weight)", "I(minplayers*log(playingtime+1))", "I(weight*log(playingtime+1))")
covariates.interactions <- paste0("s(", covariates.interactions, ", bs='cr')")

covariates <- paste(c(covariates.others, covariates.interactions, covariates.categories.reduced), collapse=" + ")

target <- "log(users_rated + 1)"
formula <- as.formula(paste(c(target, "~", covariates), collapse=" "))

model <- gam(formula, data=data)
summary(model)  # R-sq.(adj) =   0.54   Deviance explained = 54.3%
                # GCV = 0.97871  Scale est. = 0.97206   n = 21235


