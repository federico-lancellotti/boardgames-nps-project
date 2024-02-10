library(roahd)
library(dplyr)
library(tidyr)
library(fda)
library(fields)
library(progress)
library(conformalInference)

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


# Conformal prediction --------------------------------------------------------
matplot(t, Xsp1[,1:50], cex=0.5, lwd=1, lty=1, type='l')

time.grid <- seq(0, 1.1, length.out=110)
lm_train=lm.funs(intercept = T)$train.fun
lm_predict=lm.funs(intercept = T)$predict.fun


breaks <- c(quantile(t, probs=c(0.25,0.5,0.75)))
model_cut <- lm(t(Xsp1.fd.median$values) ~ bs(t, degree=3, knots=breaks))
#plot(model_cut)

design_matrix <- bs(t, degree=3, knots=breaks)
pred_grid=matrix(bs(time.grid, degree=3, knots=breaks), nrow=length(time.grid))

c_preds <- conformal.pred(design_matrix, t(Xsp1.fd.median$values), pred_grid, alpha=0.05, verbose=F, train.fun = lm_train, predict.fun=lm_predict, num.grid.pts=200)

plot(t, Xsp1.fd.median$values, xlim=range(time.grid), type='l', cex=.5, col ="lightblue", main='Spline Regression')
lines(time.grid, c_preds$pred, lwd=2, col="black", lty=1)
matlines(time.grid, cbind(c_preds$up,c_preds$lo), lwd=1, col="black", lty=2)











train_ss <- function(x, y, out=NULL){
  smooth.spline(x, y, df=opt)
}

predict_ss <- function(obj, new_x){
  predict(obj, new_x)$y
}

time.grid <- seq(0, 1.1, length.out=110)

fit <- smooth.spline(t, t(Xsp1.fd.median$values))
opt <- fit$df
c_preds <- conformal.pred(t, t(Xsp1.fd.median$values), time.grid, alpha=0.05, verbose=F, train.fun=train_ss, predict.fun=predict_ss, num.grid.pts=200)

plot(t, Xsp1.fd.median$values, cex =.5, type='l', col ="lightblue", main='Smoothing spline')
lines(time.grid, c_preds$pred, lwd=2, col ="black", lty=1)
matlines(time.grid, cbind(c_preds$up,c_preds$lo), lwd=2, col ="black", lty=2)


