library(ISLR2)
library(car)
library(mgcv)
# library(rgl)
library(splines)
library(pbapply)
library(progress)

library(robustbase)
library(psych)
library(MASS)
library(ellipse)
library(here)
library(DescTools)
library(knitr)
library(RobStatTM)


# Set global parameters -------------------------------------------------------
set.seed(1)
B <- 1e2


# Import the data -------------------------------------------------------------
data <- read.csv("../data-snapshot/boardgames-weights-publishers.csv")
data[!complete.cases(data), ]


# Publishing year -------------------------------------------------------------
boardgames <- tidytuesdayR::tt_load('2022-01-25')
details <- boardgames$details

merged_df <- merge(data, details[,c(2,5)], by = "id", all.x = TRUE)
merged_df <- merged_df[, c(1:3, ncol(merged_df), 5:(ncol(merged_df)-1))]
data <- merged_df

rm(merged_df)
rm(details)
rm(boardgames)


# Exploration -----------------------------------------------------------------
with(data, scatterplotMatrix(data.frame(log(users_rated), average, weight)))

hist(log(data$users_rated))
hist(data$yearpublished)
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

covariates.others <- c("minplayers", "weight", "log(playingtime+1)", "minage", "dimpublisher", "avgpublisher", "yearpublished")
covariates.others <- paste0("s(", covariates.others, ", bs='tp', m=3)")

covariates <- paste(c(covariates.others, covariates.categories), collapse=" + ")

target <- "log(users_rated + 1)"
formula <- as.formula(paste(c(target, "~", covariates), collapse=" "))

model <- gam(formula, data=data)
model.summary <- summary(model)
model.summary


# Variable selection ----------------------------------------------------------
covariates.categories.toremove <- names(which(model.summary$p.pv > 0.05))
covariates.categories.toremove

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

pb <- progress_bar$new(format = "  processing [:bar] :percent eta: :eta", total = B, clear = FALSE)
set.seed(1)
for(perm in 1:B){
  permutation <- sample(nrow(data))
  
  residuals.H0.perm <- residuals.H0[permutation]
  Y.perm.H0 <- model.reduced$fitted + residuals.H0.perm
  
  formula.new <-  as.formula(paste(c("Y.perm.H0", "~", covariates), collapse=" "))
  T_H0[perm] <- sum(abs(summary(gam(formula.new, data=data))$p.t[covariates.categories.toremove]))

  pb$tick()
}

sum(T_H0>=T0)/B


# Model with interactions -----------------------------------------------------
covariates.categories.reduced <- setdiff(colnames(data[,catList.index:ncol(data)]),
                                         covariates.categories.toremove)

covariates.others <- c("average", "maxplayers", "weight", "log(playingtime+1)", "minage", "dimpublisher", "avgpublisher", "yearpublished")
covariates.others <- paste0("s(", covariates.others, ", bs='cr')")

covariates.interactions <- c("I(year*weight)", "I(minplayers*log(playingtime+1))", "I(weight*log(playingtime+1))")
covariates.interactions <- paste0("s(", covariates.interactions, ", bs='cr')")

covariates <- paste(c(covariates.others, covariates.interactions, covariates.categories.reduced), collapse=" + ")

target <- "log(users_rated + 1)"
formula <- as.formula(paste(c(target, "~", covariates), collapse=" "))

model <- gam(formula, data=data)
summary(model)  # R-sq.(adj) =   0.54   Deviance explained = 54.3%
                # GCV = 0.97871  Scale est. = 0.97206   n = 21235

plot(model)


# Outliers  -------------------------------------------------------------------
covariates.MCD <- c("maxplayers", "playingtime", "yearpublished")
covariates.MCD.ind <- which(colnames(data) %in% covariates.MCD)
covariates.MCD.ind

fit_MCD <- covMcd(x = data[,covariates.MCD.ind], alpha = .95, nsamp = 1000)
fit_MCD

ind_out_obs <- setdiff(1:nrow(data), fit_MCD$best)
length(ind_out_obs)

# plot(fit_MCD,classic=TRUE)

data.out <- data[-ind_out_obs,]
range(data.out$yearpublished)
range(data.out$playingtime)
range(data.out$maxplayers)



## Robust model ----
covariates.categories <- colnames(data[,catList.index:ncol(data)])

covariates.others <- c("maxplayers", "weight", "log(playingtime+1)", "minage", "dimpublisher", "yearpublished")
covariates.others <- paste0("s(", covariates.others, ", bs='tp', m=3)")

covariates <- paste(c(covariates.others, covariates.categories), collapse=" + ")

target <- "log(users_rated + 1)"
formula <- as.formula(paste(c(target, "~", covariates), collapse=" "))

model <- gam(formula, data=data.out)
model.summary <- summary(model)
model.summary


## Robust model, reduced ----
covariates.categories.toremove <- names(which(model.summary$p.pv > 0.05))
covariates.categories.toremove
covariates.categories.reduced <- setdiff(colnames(data[,catList.index:ncol(data)]),
                                         covariates.categories.toremove)

covariates <- paste(c(covariates.others, covariates.categories.reduced), collapse=" + ")
target <- "log(users_rated + 1)"
formula <- as.formula(paste(c(target, "~", covariates), collapse=" "))

model.reduced <- gam(formula, data=data.out)
summary(model.reduced)

par(mfrow = c(1,2))
plot(model.reduced)





#------------------------------------------------------------------------------
data.out$log_users=log(data.out$users_rated + 1)
data.out$log_pltime=log(data.out$playingtime + 1)

covariates.others <- c("log_pltime", "minage", "yearpublished", "dimpublisher", "maxplayers", "weight")
covariates.others <- paste0("s(", covariates.others, ", bs='tp', m=3)")

covariates <- paste(c(covariates.others, covariates.categories.reduced), collapse=" + ")
target <- "log_users"
formula <- as.formula(paste(c(target, "~", covariates), collapse=" "))

model.reduced <- gam(formula, data=data.out)
summary(model.reduced)


# #Plot (all):
# library(gratia)
# draw(model.reduced)


#Plot (all):
library(gratia)
draw(model.reduced, residuas=TRUE)


# log_pltime, minage, yearpublished, dimpublisher, maxplayers, weight


#1) log_pltime:
p_obj=plot(model.reduced, residuals = TRUE)
p_obj1=p_obj[[1]] #prende solo log_pltime
sm_df=as.data.frame(p_obj1[c("x", "se", "fit")])
data_df=as.data.frame(p_obj1[c("raw", "p.resid")])

## plot
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_rug(data = data_df, mapping = aes(x = raw, y = NULL), sides = "b") +
  #geom_point(data = data_df, mapping = aes(x = raw, y = p.resid, color = "salmon")) +  # Aggiunto color = "skyblue" --> viene comunque salmone
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL), alpha = 0.3) +
  geom_line() +
  labs(x = "log(PlayingTime+1)", y = "Partial effect") +
  theme(legend.position = "none")


#2) minage:
# p_obj=plot(model, residuals = TRUE)
p_obj2=p_obj[[2]] #prende solo minage
sm_df=as.data.frame(p_obj2[c("x", "se", "fit")])
data_df=as.data.frame(p_obj2[c("raw", "p.resid")])

## plot
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_rug(data = data_df, mapping = aes(x = raw, y = NULL), sides = "b") +
  #geom_point(data = data_df, mapping = aes(x = raw, y = p.resid, color = "salmon")) +  
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL), alpha = 0.3) +
  geom_line() +
  labs(x = "Min Age", y = "Partial effect") +
  theme(legend.position = "none")


#3) yearpublished:
# p_obj=plot(model, residuals = TRUE)
p_obj3=p_obj[[3]] #prende solo yearpublished
sm_df=as.data.frame(p_obj3[c("x", "se", "fit")])
data_df=as.data.frame(p_obj3[c("raw", "p.resid")])

## plot
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_rug(data = data_df, mapping = aes(x = raw, y = NULL), sides = "b") +
  #geom_point(data = data_df, mapping = aes(x = raw, y = p.resid, color = "salmon")) +  
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL), alpha = 0.3) +
  geom_line() +
  labs(x = "Year Published", y = "Partial effect") +
  theme(legend.position = "none")


#4) dimpublisher:
# p_obj=plot(model, residuals = TRUE)
p_obj4=p_obj[[4]] #prende solo dimpublisher
sm_df=as.data.frame(p_obj4[c("x", "se", "fit")])
data_df=as.data.frame(p_obj4[c("raw", "p.resid")])

## plot
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_rug(data = data_df, mapping = aes(x = raw, y = NULL), sides = "b") +
  #geom_point(data = data_df, mapping = aes(x = raw, y = p.resid, color = "salmon")) +  
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL), alpha = 0.3) +
  geom_line() +
  labs(x = "Dim Publisher" , y = "Partial effect") +
  theme(legend.position = "none")


#5) maxplayers:
# p_obj=plot(model, residuals = TRUE)
p_obj5=p_obj[[5]] #prende solo maxplayers
sm_df=as.data.frame(p_obj5[c("x", "se", "fit")])
data_df=as.data.frame(p_obj5[c("raw", "p.resid")])

## plot
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_rug(data = data_df, mapping = aes(x = raw, y = NULL), sides = "b") +
  #geom_point(data = data_df, mapping = aes(x = raw, y = p.resid, color = "salmon")) +  
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL), alpha = 0.3) +
  geom_line() +
  labs(x = "Max Players" , y = "Partial effect") +
  theme(legend.position = "none")


#6) weight:
# p_obj=plot(model, residuals = TRUE)
p_obj6=p_obj[[6]] #prende solo weight
sm_df=as.data.frame(p_obj6[c("x", "se", "fit")])
data_df=as.data.frame(p_obj6[c("raw", "p.resid")])

## plot
ggplot(sm_df, aes(x = x, y = fit)) +
  geom_rug(data = data_df, mapping = aes(x = raw, y = NULL), sides = "b") +
  #geom_point(data = data_df, mapping = aes(x = raw, y = p.resid, color = "salmon")) +  
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL), alpha = 0.3) +
  geom_line() +
  labs(x = "Weight" , y = "Partial effect") +
  theme(legend.position = "none")






