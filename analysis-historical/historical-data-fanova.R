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

# fANOVA ----------------------------------------------------------------------
## With the fdANOVA library ----
fanova <- fanova.tests(Xsp1, data$Card.Game, test = "GPF")
summary(fanova)

#groups <- data$Economic
groups <- data[,(catList.index+1):(catList.index+n_categories)]


## fANOVA - Do It Yourself ---- 
### parametric multi-way fANOVA ----
fanova.GPF <- function(data, groups) {
  T.len <- dim(data)[1]
  p <- dim(groups)[2]
  
  Fstat <- matrix(nrow=T.len, ncol=p)
  mu <- matrix(nrow=T.len)
  tau <- matrix(nrow=T.len, ncol=p)
  
  covariates <- paste(colnames(groups), collapse=" + ")
  
  for(t in 1:T.len) {
    model.formula <- as.formula(paste("data[t,]", covariates, sep=" ~ "))
    model <- aov(model.formula, data=groups)
    summ <- summary(model)
    
    mu[t] <- model$coefficients[1]
    tau[t,] <- model$coefficients[2:(p+1)]
    colnames(tau) <- colnames(groups)
    
    Fstat[t,] <- summary(model)[[1]]$`F value`[1:p]
    colnames(Fstat) <- colnames(groups)
  }
  
  output <- list(mu=mu, tau=tau, F_t=Fstat, F.glob=colMeans(Fstat))
}

model0.fanova <- fanova.GPF(Xsp0, groups)
model0.fanova$F.glob

model1.fanova <- fanova.GPF(Xsp1, groups)
model1.fanova$F.glob


### permutational multi-way fANOVA ----
fanova.GPF.perm <- function(data, groups, B) {
  p <- dim(groups)[2]
  model <- fanova.GPF(data, groups)
  
  T0 <- model$F.glob
  T_stat <- matrix(nrow=B, ncol=p) 
  
  pb <- progress_bar$new(format="  processing [:bar] :percent eta: :eta", total=B, clear=F)
  for(perm in 1:B){
    # Permutation:
    permutation <- sample(1:ncol(data))
    data.perm <- data[, permutation]
    model.perm <- fanova.GPF(data.perm, groups)
    
    # Test statistic:
    T_stat[perm,] <- model.perm$F.glob
    
    pb$tick()
  }
  
  pvalues <- rowMeans(t(T_stat) >= T0)
  names(pvalues) <- colnames(groups)
  
  output <- list(F.0=T0, F.perm=T_stat, pvalues=pvalues)
}
B <- 1000


#### On the derivative ----
### first round
model1.fanova.perm <- fanova.GPF.perm(Xsp1, groups, B)
pvals.1 <- model1.fanova.perm$pvalues
good_categories.1 <- names(which(pvals.1 <= alpha))
good_categories.1.ind <- which(colnames(groups) %in% good_categories.1)

### second round
groups.reduced.1 <- groups[,good_categories.1.ind]
model.fanova.perm <- fanova.GPF.perm(Xsp1, groups.reduced.1, B)
pvals.1.reduced <- model.fanova.perm$pvalues
good_categories.1 <- names(which(pvals.1.reduced <= alpha/length(pvals.1.reduced)))
good_categories.1.ind <- which(colnames(groups) %in% good_categories.1)

### third round
groups.reduced.1 <- groups[,good_categories.1.ind]
model.fanova.perm <- fanova.GPF.perm(Xsp1, groups.reduced.1, B)
pvals.1.reduced <- model.fanova.perm$pvalues
good_categories.1 <- names(which(pvals.1.reduced <= alpha/length(pvals.1.reduced)))
good_categories.1.ind <- which(colnames(groups) %in% good_categories.1)

### fourth round
groups.reduced.1 <- groups[,good_categories.1.ind]
model.fanova.perm <- fanova.GPF.perm(Xsp1, groups.reduced.1, B)
pvals.1.reduced <- model.fanova.perm$pvalues
good_categories.1 <- names(which(pvals.1.reduced <= alpha/length(pvals.1.reduced)))
good_categories.1.ind <- which(colnames(groups) %in% good_categories.1)

### fifth round
groups.reduced.1 <- groups[,good_categories.1.ind]
model.fanova.perm <- fanova.GPF.perm(Xsp1, groups.reduced.1, B)
pvals.1.reduced <- model.fanova.perm$pvalues
good_categories.1 <- names(which(pvals.1.reduced <= alpha/length(pvals.1.reduced)))
good_categories.1.ind <- which(colnames(groups) %in% good_categories.1)


#### On the function ----
### first round
model0.fanova.perm <- fanova.GPF.perm(Xsp0, groups, B)
pvals.0 <- model0.fanova.perm$pvalues
good_categories.0 <- names(which(pvals.0 <= alpha))
good_categories.0.ind <- which(colnames(groups) %in% good_categories.0)

### second round
groups.reduced.0 <- groups[,good_categories.0.ind]
model0.fanova.perm <- fanova.GPF.perm(Xsp0, groups.reduced.0, B)
pvals.0.reduced <- model0.fanova.perm$pvalues
good_categories.0 <- names(which(pvals.0.reduced <= alpha/length(pvals.0.reduced)))
good_categories.0.ind <- which(colnames(groups) %in% good_categories.0)

### third round
groups.reduced.0 <- groups[,good_categories.0.ind]
model0.fanova.perm <- fanova.GPF.perm(Xsp0, groups.reduced.0, B)
pvals.0.reduced <- model0.fanova.perm$pvalues
good_categories.0 <- names(which(pvals.0.reduced <= alpha/length(pvals.0.reduced)))
good_categories.0.ind <- which(colnames(groups) %in% good_categories.0)

### fourth round
groups.reduced.0 <- groups[,good_categories.0.ind]
model0.fanova.perm <- fanova.GPF.perm(Xsp0, groups.reduced.0, 1000)
pvals.0.reduced <- model0.fanova.perm$pvalues
good_categories.0 <- names(which(pvals.0.reduced <= alpha/length(pvals.0.reduced)))
good_categories.0.ind <- which(colnames(groups) %in% good_categories.0)


### multi-way fANOVA, bootstrap CI ----
B <- 100
fanova.GPF.bootstrap <- function(data, groups, B, alpha) {
  T.len <- dim(data)[1]
  N <- dim(data)[2]
  p <- dim(groups)[2]
  model <- fanova.GPF(data, groups)
  
  fitted.obs <- as.numeric(model$mu) + model$tau %*% t(groups)
  res.obs <- data - fitted.obs
  
  L.obs <- model$tau
  T.boot.L <- array(0, dim = c(B, T.len, p))
  F.boot <- matrix(nrow=B, ncol=p)
  
  pb <- progress_bar$new(format = "  processing [:bar] :percent eta: :eta", total = B, clear = FALSE)
  set.seed(seed)
  for (b in 1:B) {
    sample_rows <- sample(ncol(res.obs), replace=T)
    response.b <- fitted.obs + res.obs[,sample_rows]
    model.b <- fanova.GPF(response.b, groups)
    T.boot.L[b,,] <- model.b$tau
    F.boot[b,] <- model.b$F.glob
    pb$tick()
  }
  
  right.quantile.L <- apply(T.boot.L, c(2,3), function(column) quantile(column, 1 - alpha/2))
  left.quantile.L <- apply(T.boot.L, c(2,3), function(column) quantile(column, alpha/2))
  
  CI.RP.L <- abind(L.obs - (right.quantile.L - L.obs),
                   L.obs,
                   L.obs - (left.quantile.L - L.obs), along=3)
  CI.RP.L <- aperm(CI.RP.L, c(3,1,2))
  rownames(CI.RP.L) <- c("lwr", "lvl", "upr")
  
  output <- list(Estimate=L.obs, Estimate.boot=T.boot.L, CI.RP=CI.RP.L)
}

# functions
groups.reduced.0 <- groups[,good_categories.0.ind]
model0.fanova <- fanova.GPF(Xsp0, groups.reduced.0)
model.fanova.boot.0 <- fanova.GPF.bootstrap(Xsp0, groups.reduced.0, B, 0.05)
CI.0 <- model.fanova.boot.0$CI.RP

# derivatives
groups.reduced.1 <- groups[,good_categories.1.ind]
model1.fanova <- fanova.GPF(Xsp1, groups.reduced.1)
model.fanova.boot.1 <- fanova.GPF.bootstrap(Xsp1, groups.reduced.1, B, 0.05)
CI.1 <- model.fanova.boot.1$CI.RP


# Plots -----------------------------------------------------------------------
cat <- 1
ylim <- c(min(CI.1[,,cat], model1.fanova$mu), max(CI.1[,,cat], model1.fanova$mu))

plot(t, CI.1[2,,cat], type='l', ylim=ylim, col="#2050A8", lwd=2)
lines(t, CI.1[1,,cat], col="#2050A8", lty=2)
lines(t, CI.1[3,,cat], col="#2050A8", lty=2)
lines(t, model1.fanova$mu, col="#C64237", lwd=2)


## Plot of the tau ----
date.begin <- as.Date("2016-10-12")
date.end <- as.Date("2023-11-04")
T.range <- as.numeric(date.end - date.begin)

years.breaks <- c(round(as.numeric((as.Date("2017-1-1") - date.begin)) / T.range * length(t)),
                  round(as.numeric((as.Date("2018-1-1") - date.begin)) / T.range * length(t)),
                  round(as.numeric((as.Date("2019-1-1") - date.begin)) / T.range * length(t)),
                  round(as.numeric((as.Date("2020-1-1") - date.begin)) / T.range * length(t)),
                  round(as.numeric((as.Date("2021-1-1") - date.begin)) / T.range * length(t)),
                  round(as.numeric((as.Date("2022-1-1") - date.begin)) / T.range * length(t)),
                  round(as.numeric((as.Date("2023-1-1") - date.begin)) / T.range * length(t))
)
names(years.breaks) <- c("2017", "2018", "2019", "2020", "2021", "2022", "2023")


plot_cat_effects <- function(mu, CI, type, category_name) {
  if (type==0) {
    folder <- "plot/tau-0"
    ylabel <- "Ratings"
  }
  else if (type==1) {
    folder <- "plot/tau-deriv"
    ylabel <- "Ratings/day"
  }
  else {
    stop("Wrong type. 0: function, 1: derivative.")
  }
  
  title <- category_name
  df <- data.frame(x=t, mu=mu, estimate=CI[2,], lwr=CI[1,], upr=CI[3,])
  
  p <- ggplot(data=df, aes(x = x)) +
    geom_line(aes(y = estimate, color = "tau"), linetype = "solid", linewidth = 2) +
    geom_line(aes(y = lwr, color = "tau"), linetype = "dashed", linewidth = 1.2) +
    geom_line(aes(y = upr, color = "tau"), linetype = "dashed", linewidth = 1.2) +
    geom_line(aes(y = mu, color = "mu"), linetype = "solid", linewidth = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#3A3A3A") + 
    labs(title = title, x = "t", y = ylabel) +
    scale_x_continuous(breaks = t[years.breaks], labels = names(years.breaks)) +
    scale_color_manual(name = "Legend",
                       values = c("tau" = "#3C3FB1", "mu" = "#FBC70B"),  #c("tau" = "#73aaff", "mu" = "#f7809c"),
                       labels = c(expression(mu(t)), expression(tau(t)))) +
    theme_minimal()
  
  filename <- paste(folder, "/", tolower(title), ".png", sep="")
  ggsave(filename, plot = p, width = 6, height = 4, units = "in")
  print(p)
}

cat.ind <- 1
cat.name <- names(CI.1[1,1,])[cat.ind]
plot_cat_effects(model1.fanova$mu, CI.1[,,cat.ind], 1, cat.name)

for (cat.ind in 1:dim(CI.0)[3]) {
  cat.name <- names(CI.0[1,1,])[cat.ind]
  plot_cat_effects(model0.fanova$mu, CI.0[,,cat.ind], 0, cat.name)
}

for (cat.ind in 1:dim(CI.1)[3]) {
  cat.name <- names(CI.1[1,1,])[cat.ind]
  plot_cat_effects(model1.fanova$mu, CI.1[,,cat.ind], 1, cat.name)
}


good_categories <- intersect(good_categories.0, good_categories.1)
good_categories

# > good_categories
# [1] "Economic"           "Fantasy"            "Medieval"           "Territory.Building" "Civilization"      
# [6] "Children.s.Game"    "City.Building"      "Exploration"        "Farming"            "Science.Fiction"   
# [11] "Fighting"           "Wargame"            "Novel.based"        "Deduction"       


