library(roahd)
library(dplyr)
library(tidyr)
library(fda)
library(fields)
library(fdANOVA)
library(progress)


# Import data -----------------------------------------------------------------
data <- read.csv("../data-historical/usersRated_preproc.csv")

## How many categories
catList.index <- which(colnames(data) == "Category")
start.index <- which(colnames(data) == "X2016.10.12")
n_categories <- start.index - catList.index - 1
n_categories

## Bad rows
rows.all_equal <- apply(data[, start.index:ncol(data)], 1, function(x) all(x == x[start.index]))
which(rows.all_equal)
data.broken <- data[rows.all_equal,]
data <- data[-which(rows.all_equal),]

rows.all_equal <- apply(data[, start.index:ncol(data)], 1, function(x) all(x == x[start.index]))
which(rows.all_equal)  # if "named integer(0)", we are good


# Plots -----------------------------------------------------------------------
N <- nrow(data)

P <- ncol(data) - 4 - n_categories  # from 2016.10.12 to 2023.11.04 should be 2579
abscissa <-  seq(0, 1, length.out=P)

sample_rows <- sample(1:nrow(data), 100)
f_data <- fData(abscissa, data[sample_rows, start.index:ncol(data)])
plot(f_data)


# Smoothing (splines) ---------------------------------------------------------
## Set parameters
m <- 5           # spline order 
degree <- m-1    # spline degree 
nbasis <- 10

## Create the basis
basis <- create.bspline.basis(rangeval=c(0,1), nbasis=nbasis, norder=m)
plot(basis)

t <- seq(0, 1, length.out=100)
y <- t(as.matrix(data[,start.index:ncol(data)]))


## Classical smoothing ----
Xsp <- smooth.basis(argvals=abscissa, y=y, fdParobj=basis)
Xsp0 <- eval.fd(t, Xsp$fd) #  the curve smoothing the data
Xsp1 <- eval.fd(t, Xsp$fd, Lfd=1) # first derivative
Xsp2 <- eval.fd(t, Xsp$fd, Lfd=2) # second derivative


## Plot
i <- sample(1:nrow(data), 1)
par(mfrow=c(1,3))
plot(abscissa, data[i,89:ncol(data)], xlab="t", ylab="observed data", pch=20, cex=0.1)
points(t, Xsp0[,i], type="l", col=rgb(0.84, 0, 0), lwd=2)
# legend("topleft", legend = c("noisy data","estimated curve"), col = c("black", rgb(0.84, 0, 0)), lwd = c(1,3,2))
plot(t, Xsp1[,i], xlab="t", ylab="first derivative", type="l",col=rgb(0.84, 0, 0),lwd=2)
plot(t, Xsp2[,i], xlab="t", ylab="second derivative", type="l", col=rgb(0.84, 0, 0), lwd=2)


## Monotone smoothing ----
idx <- sample(1:ncol(y), 1000)
idx <- 1:ncol(y)

Lfdobj <- 3          
lambda <- 1e-2
cvecf <- matrix(0, nbasis, 1) # this is used as initial value 
Wfd0 <- fd(coef = cvecf, basisobj = basis)
fdPar.obj <- fdPar(fdobj = Wfd0, Lfdobj = Lfdobj, lambda = lambda)

Xsp0 <- matrix(0, nrow=length(t), ncol=length(idx))
Xsp1 <- matrix(0, nrow=length(t), ncol=length(idx))
Xsp2 <- matrix(0, nrow=length(t), ncol=length(idx))
basismat <- eval.basis(t, basis)

err <- c()
pb <- progress_bar$new(format="  processing [:bar] :percent eta: :eta", total=length(idx), clear=F)
for (i in 1:length(idx)) {
  tryCatch({
    Xsp.mon <- smooth.monotone(argvals = abscissa, y = y[,idx[i]], WfdParobj = fdPar.obj)
  
    hgtfhatfd <- Xsp.mon$yhatfd
    Xsp0[,i] <- basismat %*% hgtfhatfd$coefs
    
    velocfdUN <- deriv.fd(expr = hgtfhatfd, Lfdobj = 1)
    Xsp1[,i] <- basismat %*% velocfdUN$coefs
    
    accelfdUN <- deriv.fd(expr = hgtfhatfd, Lfdobj = 2)
    Xsp2[,i] <- basismat %*% accelfdUN$coefs
  }, error = function(e) {
    cat("\nError at the iteration", i, ":", conditionMessage(e), "\n")
    err <<- c(err, idx[i])
  })
  
  pb$tick()
}

if (length(err) > 0) {
  cat("Iterations with errors:", err, "\n")
} else {
  cat("No errors in the loop.\n")
}

data.smoothed <- list(Xsp0=Xsp0, Xsp1=Xsp1, Xsp2=Xsp2, err_idx=err)
saveRDS(data.smoothed, file="../data-historical/smoothed-data.RDS")


# Load smoothed data ----------------------------------------------------------
data.smoothed <- readRDS("../data-historical/smoothed-data.RDS")
Xsp0 <- data.smoothed$Xsp0
Xsp1 <- data.smoothed$Xsp1
Xsp2 <- data.smoothed$Xsp2
err <- data.smoothed$err_idx

### Problematic curves ----
for (i in 1:length(err)) {
  Xsp <- smooth.basis(argvals=abscissa, y=y[,err[i]], fdParobj=basis)
  Xsp0[,err[i]] <- eval.fd(t, Xsp$fd) #  the curve smoothing the data
  Xsp1[,err[i]] <- eval.fd(t, Xsp$fd, Lfd=1) # first derivative
  Xsp2[,err[i]] <- eval.fd(t, Xsp$fd, Lfd=2) # second derivative
}


## Smoothed curves ----
Xsp0.fd <- fData(t, t(Xsp0))
Xsp0.fd.median <- median_fData(fData = Xsp0.fd, type = "MBD")

plot(Xsp0.fd)
lines(t, Xsp0.fd.median$values, lwd=2)

# fbplot(Xsp0.fd, main="Magnitude outliers")
outliergram(Xsp0.fd)

### First derivative ----
Xsp1.fd <- fData(t, t(Xsp1))
Xsp1.fd.median <- median_fData(fData = Xsp1.fd, type = "MBD")

plot(Xsp1.fd)
lines(t, Xsp1.fd.median$values, lwd=2)

# fbplot(Xsp1.fd, main="Magnitude outliers")
outliergram(Xsp1.fd)

### Second derivative ----
Xsp2.fd <- fData(t, t(Xsp2))
Xsp2.fd.median <- median_fData(fData = Xsp2.fd, type = "MBD")

plot(Xsp2.fd)
lines(t, Xsp2.fd.median$values, lwd=2)

# fbplot(Xsp2.fd, main="Magnitude outliers")
outliergram(Xsp2.fd)


### Plots
sample_rows <- sample(1:Xsp0.fd$N, 100)
sample_rows <- 672
sample_rows <- 13
par(mfrow=c(1,3))
plot(Xsp0.fd[sample_rows], lty=1, lwd=2,
     cex=2, xlab="Time", ylab="No. of ratings")
plot(Xsp1.fd[sample_rows],  lty=1, lwd=2,
     cex=2, xlab="Time", ylab="Velocity (ratings/day)")
plot(Xsp2.fd[sample_rows], lty=1, lwd=2,
     cex=2, xlab="Time", ylab="Acceleration (ratings/day/day)")



# Functional ANOVA ------------------------------------------------------------
n <- ncol(Xsp0)
# n <- 1000
plotFANOVA(x=Xsp0[,1:n], group.label=as.character(data[1:n,]$Dice), int=c(0, 1), means=TRUE)

# own.cross.prod.mat <- inprod(basis, basis)
# fanova <- fanova.tests(Xsp0[,1:n], data[1:n,]$Dice, test = "Fb",
#                        param = list(B.FP = 10, basis = "own",
#                                       own.basis = t(basismat),
#                                       own.cross.prod.mat = own.cross.prod.mat))
# summary(fanova)

fanova <- fanova.tests(Xsp0, data$Economic, test = "GPF")
summary(fanova)

## Permutational FANOVA ----
B <- 100
T0 <- fanova$GPF$statGPF
T_stat <- numeric(B) 

pb <- progress_bar$new(format="  processing [:bar] :percent eta: :eta", total=B, clear=F)
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:ncol(Xsp0))
  data_perm <- Xsp0[, permutation]
  fanova.perm <- fanova.tests(data_perm, data$Economic, test = "GPF")
  
  # Test statistic:
  T_stat[perm] <- fanova.perm$GPF$statGPF
  
  pb$tick()
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

p_val <- sum(T_stat>=T0)/B
p_val


## FANOVA for all the categories ----
cat_names <- colnames(data[, (catList.index+1):(start.index-2)])
cat_names

fanova.pvals <- numeric(length(cat_names))
names(fanova.pvals) <- cat_names
fanova.pvals

pb <- progress_bar$new(format="  processing [:bar] :percent eta: :eta", total=length(cat_names), clear=F)
for (i in 1:length(cat_names)) {
  cat.index <- catList.index + i
  fanova.cat <- fanova.tests(Xsp0, data[,cat.index], test = "GPF")
  fanova.pvals[i] <- fanova.cat$GPF$pvalueGPF
  
  pb$tick()
}

fanova.pvals
which(fanova.pvals < 0.05)


## Permutational FANOVA for all the categories ----
# (~1h)
perm.fanova.test <- function(data, labels, B) {
  fanova <- fanova.tests(data, labels, test = "GPF")
  
  T0 <- fanova$GPF$statGPF
  T_stat <- numeric(B) 
  
  for(perm in 1:B){
    # Permutation:
    permutation <- sample(1:ncol(data))
    data_perm <- data[, permutation]
    fanova.perm <- fanova.tests(data_perm, labels, test = "GPF")
    
    # Test statistic:
    T_stat[perm] <- fanova.perm$GPF$statGPF
  }
  
  p_val <- sum(T_stat>=T0)/B
}


fanova.pvals.perm <- numeric(length(cat_names))

pb <- progress_bar$new(format="  processing [:bar] :percent eta: :eta", total=length(cat_names), clear=F)
for (i in 1:length(cat_names)) {
  cat.index <- catList.index + i
  pval <- perm.fanova.test(Xsp0, data[,cat.index], 100)
  fanova.pvals[i] <- pval
  
  pb$tick()
}

fanova.pvals
signif_cat <- which(fanova.pvals < 0.01)
signif_cat <- signif_cat[-length(signif_cat)]

# Should we use the median, instead of the mean?


## FANOVA for all the categories on the derivative ----
fanova.pvals.veloc <- numeric(length(cat_names))
names(fanova.pvals.veloc) <- cat_names
fanova.pvals.veloc

pb <- progress_bar$new(format="  processing [:bar] :percent eta: :eta", total=length(cat_names), clear=F)
for (i in 1:length(cat_names)) {
  cat.index <- catList.index + i
  fanova.cat <- fanova.tests(Xsp1, data[,cat.index], test = "GPF")
  fanova.pvals.veloc[i] <- fanova.cat$GPF$pvalueGPF
  
  pb$tick()
}

fanova.pvals.veloc
which(fanova.pvals.veloc < 0.05)
length(which(fanova.pvals.veloc < 0.05))

sum(which(fanova.pvals.veloc < 0.05) %in% which(fanova.pvals < 0.05))

signif_cat.deriv <- which(fanova.pvals.veloc < 0.01)
signif_cat.deriv <- signif_cat.deriv[-length(signif_cat.deriv)]


## Plot of the medians ----
date.begin <- as.Date("2016-10-12")
date.end <- as.Date("2023-11-04")
T <- as.numeric(date.end - date.begin)

years.breaks <- c(round(as.numeric((as.Date("2017-1-1") - date.begin)) / T * length(t)),
                  round(as.numeric((as.Date("2018-1-1") - date.begin)) / T * length(t)),
                  round(as.numeric((as.Date("2019-1-1") - date.begin)) / T * length(t)),
                  round(as.numeric((as.Date("2020-1-1") - date.begin)) / T * length(t)),
                  round(as.numeric((as.Date("2021-1-1") - date.begin)) / T * length(t)),
                  round(as.numeric((as.Date("2022-1-1") - date.begin)) / T * length(t)),
                  round(as.numeric((as.Date("2023-1-1") - date.begin)) / T * length(t))
                  )
names(years.breaks) <- c("2017", "2018", "2019", "2020", "2021", "2022", "2023")


plot_medians <- function(col_ind, type) {
  if (type==0) {
    f_data <- Xsp0.fd
    folder <- "plot/medians"
    ylabel <- "Ratings"
  }
  else if (type==1) {
    f_data <- Xsp1.fd
    folder <- "plot/medians-deriv"
    ylabel <- "Ratings/day"
  }
  else {
    stop("Wrong type. 0: function, 1: derivative.")
  }
  
  cat1_ind <- which(data[,col_ind] == 1)
  median.1 <- median_fData(fData = f_data[cat1_ind], type = "MBD")
  median.0 <- median_fData(fData = f_data[-cat1_ind], type = "MBD")
  
  median.df <- data.frame(x=t, y0=t(median.0$values), y1=t(median.1$values))
  title <- names(data)[col_ind]
  
  p <- ggplot(data=median.df, aes(x = x)) +
    geom_line(aes(y = y0, color = "0"), linetype = "solid", linewidth = 2) +
    geom_line(aes(y = y1, color = "1"), linetype = "solid", linewidth = 2) +
    labs(title = title, x = "t", y = ylabel) +
    scale_x_continuous(breaks = t[years.breaks], labels = names(years.breaks)) +
    scale_color_manual(name = "Legend",
                       values = c("0" = "#73aaff", "1" = "#f7809c"),
                       labels = c("0", "1")) +
    theme_minimal()
  
  filename <- paste(folder, "/", tolower(title), ".png", sep="")
  ggsave(filename, plot = p, width = 6, height = 4, units = "in")
  print(p)
}

for (cat in signif_cat) {
  idx <- catList.index + cat
  plot_medians(idx, 0)
  #Sys.sleep(5)
}

## Plot of the medians of the derivative ----

for (cat in signif_cat.deriv) {
  idx <- catList.index + cat
  plot_medians(idx, 1)
  #Sys.sleep(5)
}


# fRegress -------------------------------------------------------------------
y.fd <- Data2fd(t, Xsp1)
x1.fd <- as.numeric(data$Economic)
x2.fd <- as.numeric(data$Negotiation)

fmodel <- fRegress(y.fd ~ x1.fd + x2.fd)

plot(fmodel$betaestlist$const$fd)
plot(fmodel$betaestlist$x1.fd$fd)
plot(fmodel$betaestlist$x2.fd$fd)


# Multi-way ANOVA with Modified Band Depth ------------------------------------
modified_band_depth <- MBD(Data = Xsp1.fd)
model.anova <- aov(modified_band_depth ~ Economic + Negotiation +
                     Card.Game + Fantasy + Medieval +
                     Territory.Building + Civilization +
                     Children.s.Game + City.Building + Exploration + Travel +
                     Farming + Mythology + Bluffing + Science.Fiction +
                     Collectible.Components + Dice + Fighting +
                     Miniatures + Racing +
                     Wargame + Space.Exploration + Renaissance +
                     Humor + Electronic + Horror + 
                     Deduction + 
                     Movies...TV...Radio.theme + Memory + Puzzle +
                     Trivia + Industry...Manufacturing +
                     Animals +
                     Sports +
                     Number + Spies.Secret.Agents + Medical + 
                     Post.Napoleonic + Environmental
                   , data=data)
summary(model.anova)
length(model.anova$coefficients) - 1

signif_cat <- names(model.anova$coefficients)[-1]


anova.interactions <- function(category) {
  category.ind <- grep(category, signif_cat)
  new_signif_cat <- signif_cat[-category.ind]
  
  interactions <- paste("modified_band_depth", category, sep=" ~ ")
  
  for (other_cat in new_signif_cat) {
    new_interaction <- paste(category, other_cat, sep=":")
    interactions <- paste(interactions, other_cat, sep=" + ")
    interactions <- paste(interactions, new_interaction, sep=" + ")
  }
  
  model <- aov(as.formula(interactions), data=data)
}
anova.interactions("Economic")

for (cat in signif_cat) {
  print(summary(anova.interactions(cat)))
}
