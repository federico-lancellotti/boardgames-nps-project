library(roahd)
library(dplyr)
library(tidyr)
library(fda)
library(fields)
library(fdANOVA)
library(progress)


# Import data -----------------------------------------------------------------
data.day <- read.csv("../data-historical/usersRated_diff.csv")
data.week <- read.csv("../data-historical/usersRated_diff_weekly.csv")

## How many categories ----
catList.index <- which(colnames(data.day) == "Category")
start.index <- which(colnames(data.day) == "X2016.10.12") + 1
n_categories <- start.index - catList.index - 1
n_categories


# Plots -----------------------------------------------------------------------
## Daily data ----
P <- ncol(data.day) - 4 - n_categories  # from 2016.10.12 to 2023.11.04 should be 2579
abscissa <-  seq(0, 1, length.out=P)

sample_rows <- sample(1:nrow(data.day), 100)
f_data <- fData(abscissa, data.day[sample_rows, start.index:ncol(data.day)])
plot(f_data)

## Weekly data ----
P <- ncol(data.week) - 4 - n_categories  # from 2016.10.12 to 2023.11.04 should be 2579
abscissa <-  seq(0, 1, length.out=P)

sample_rows <- sample(1:nrow(data.week), 100)
f_data <- fData(abscissa, data.week[sample_rows, start.index:ncol(data.week)])
plot(f_data)

## We proceed with weekly data.
data <- data.week


# Smoothing (splines) ---------------------------------------------------------
## Set parameters ----
m <- 5           # spline order 
degree <- m-1    # spline degree 
nbasis <- 8

## Create the basis ----
basis <- create.bspline.basis(rangeval=c(0,1), nbasis=nbasis, norder=m)
par(mfrow=c(1,1))
plot(basis)

basis.fdPar <- fdPar(basis, Lfdobj = 3)  # Imposta Lfdobj a 2 per ottenere la seconda derivata
basis.fdPar$lambda <- 1e-4  # Regolarizzazione per garantire la positività

## Fit via LS ----
N <- nrow(data)
N <- 100
pb <- progress::progress_bar$new(
  format="Smoothing in progress [:bar] :percent in :elapsed",
  total=N, clear=FALSE, width=60
  )

for (i in 1:N) {
  capture_output <- capture.output({
    Xsp <- smooth.pos(argvals=abscissa, y=t(data[i,start.index:ncol(data)]), WfdParobj=basis.fdPar)
  })
  
  Xsp0 <- predict(Xsp, newdata=abscissa) # the curve smoothing the data
  Xsp1 <- predict(Xsp, newdata=abscissa, deriv=1) # first derivative
  Xsp2 <- predict(Xsp, newdata=abscissa, deriv=2) # second derivative
  
  pb$tick()
}

i <- sample(1:N, 1)
par(mfrow=c(1,3))
plot(abscissa, data[i,start.index:ncol(data)], xlab="t", ylab="observed data", pch=20, cex=0.1)
points(abscissa, Xsp0[,i], type="l", col=rgb(0.84, 0, 0), lwd=2)
# legend("topleft", legend = c("noisy data","estimated curve"), col = c("black", rgb(0.84, 0, 0)), lwd = c(1,3,2))
plot(abscissa, Xsp1[,i], xlab="t", ylab="first derivative", type="l",col=rgb(0.84, 0, 0),lwd=2)
plot(abscissa, Xsp2[,i], xlab="t", ylab="second derivative", type="l", col=rgb(0.84, 0, 0), lwd=2)





