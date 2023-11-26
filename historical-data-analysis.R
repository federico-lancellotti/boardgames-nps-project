library(roahd)
library(dplyr)
library(tidyr)
library(fda)


# Import data -----------------------------------------------------------------
data <- read.csv("./data/usersRated_preproc.csv")

## How many categories
catList.index <- which(colnames(data) == "Category")
start.index <- which(colnames(data) == "X2016.10.12")
n_categories <- start.index - catList.index - 1
n_categories


# Plots -----------------------------------------------------------------------
P <- ncol(data) - 4 - n_categories  # from 2016.10.12 to 2023.11.04 should be 2579
abscissa <-  seq(0, 1, length.out=P)

sample_rows <- sample(1:nrow(data), 100)
f_data <- fData(abscissa, data[sample_rows, start.index:ncol(data)])
plot(f_data)


# Smoothing (splines) ---------------------------------------------------------
## Set parameters
m <- 5           # spline order 
degree <- m-1    # spline degree 
nbasis <- 20

## Create the basis
basis <- create.bspline.basis(rangeval=c(0,1), nbasis=nbasis, norder=m)
plot(basis)

## Evaluate the basis on the grid of abscissa
basismat <- eval.basis(abscissa, basis)
basismat1 <- eval.basis(abscissa, basis, Lfdobj=1)
basismat2 <- eval.basis(abscissa, basis, Lfdobj=2)

## Fit via LS
Xsp0 <- matrix(data=numeric(), nrow=length(abscissa), ncol=nrow(data))
Xsp1 <- matrix(data=numeric(), nrow=length(abscissa), ncol=nrow(data))
Xsp2 <- matrix(data=numeric(), nrow=length(abscissa), ncol=nrow(data))

for (i in 1:nrow(data)) {
  est_coef = lsfit(basismat, t(data[i,start.index:ncol(data)]), intercept=FALSE)$coef
  Xsp0[,i] <- basismat %*% est_coef
  Xsp1[,i] <- basismat1 %*% est_coef
  Xsp2[,i] <- basismat2 %*% est_coef
}

i <- sample(1:nrow(data), 1)
par(mfrow=c(1,3))
plot(abscissa, data[i,89:ncol(data)], xlab="t", ylab="observed data", pch=20, cex=0.1)
points(abscissa, Xsp0[,i], type="l", col=rgb(0.84, 0, 0), lwd=2)
# legend("topleft", legend = c("noisy data","estimated curve"), col = c("black", rgb(0.84, 0, 0)), lwd = c(1,3,2))
plot(abscissa, Xsp1[,i], xlab="t", ylab="first derivative", type="l",col=rgb(0.84, 0, 0),lwd=2)
plot(abscissa, Xsp2[,i], xlab="t", ylab="second derivative", type="l", col=rgb(0.84, 0, 0), lwd=2)

## Generalized cross-validation
sample.n <- 500
nbasis <- 15:30
gcv <- numeric(length(nbasis))
idx <- sample(1:nrow(data), sample.n)
for (k in 1:length(nbasis)){
  basis <- create.bspline.basis(c(0,1), nbasis[k], m)
  gcv_k <- 0
  for (i in idx) {
    Xobs0 <- t(data[i,start.index:ncol(data)])
    gcv_k <- gcv_k + smooth.basis(abscissa, Xobs0, basis)$gcv
  }
  gcv[k] <- gcv_k/sample.n
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)  # nbasis=20 seems fine


# Estimate of the mean and of the covariance kernel ---------------------------
data.fd <- Data2fd(y=t(data[,start.index:ncol(data)]), argvals=abscissa, basisobj=basis)
plot.fd(data.fd, xaxs='i')
lines(mean.fd(data.fd), lwd=2)
eval <- eval.fd(abscissa, data.fd)
plot(abscissa, abscissa, (cov(t(eval))[1:length(abscissa),]))
