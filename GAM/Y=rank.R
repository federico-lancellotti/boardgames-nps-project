setwd("C:/Users/Silvia/Desktop/23-24/NONPAR/project")
# avg=read.csv("data/Average.csv", head=T) #avg rating per day
# users_rated=read.csv("data/usersRated.csv", head=T) #number of users that rated a game per day
# rank=read.csv("data/Rank.csv", head=T) #rank position per day
# details=read.csv("details.csv", head=T) 
# ratings=read.csv("ratings.csv", head=T)
LMdataset=as.data.frame(read.csv("C:/Users/Silvia/Desktop/23-24/NONPAR/project/data/LMdataset.csv",head=TRUE))

#how many unique years there are?
# unique_y <- unique(LMdataset$Year)
# num_unique_y <- length(unique_y)
#we can do different linear models for different years and see what changes during time



library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)

mydata=LMdataset[,c(-5,-10,-11,-13,-14,-15,-16,-17)]
mydata=mydata[,c(-5,-6,-11,-12,-13)]


#variable selection through parametric lm---------------------------------

#modello 1 :rank~.
model_lm=lm(Rank ~., data=mydata)
summary(model_lm)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -17576  -2974    413   2725  31824 
# 
# Coefficients:
#                             Estimate  Std. Error  t value   Pr(>|t|)    
# (Intercept)                  4.411e+04  2.399e+02  183.864  < 2e-16 ***
#   ID                         8.151e-03  3.100e-04   26.293  < 2e-16 ***
#   Avg_ratings               -5.179e+03  3.641e+01 -142.234  < 2e-16 ***
#   Users_rated                8.851e-01  4.273e-02   20.716  < 2e-16 ***
#   Max_players               -9.714e+01  3.270e+01   -2.971 0.002973 ** 
#   Playing_time               9.635e-02  4.949e-02    1.947 0.051590 .  
#   Min_age                   -1.195e+02  8.052e+00  -14.844  < 2e-16 ***
#   Owned                     -7.882e-01  2.923e-02  -26.966  < 2e-16 ***
#       Economic                  -4.476e+01  1.174e+02   -0.381 0.703039    
#   Negotiation                6.037e+02  1.629e+02    3.706 0.000211 ***
#   Political                 -3.777e+02  1.819e+02   -2.077 0.037796 *  
#       Card.Game                  8.177e+00  6.512e+01    0.126 0.900076    
#   Fantasy                   -2.313e+02  8.994e+01   -2.572 0.010131 *  
#   Abstract.Strategy          5.499e+02  1.102e+02    4.989 6.12e-07 ***
#   Medieval                  -4.787e+02  1.267e+02   -3.779 0.000158 ***
#   Ancient                   -8.817e+02  1.524e+02   -5.785 7.36e-09 ***
#   Territory.Building        -8.061e+02  1.774e+02   -4.544 5.54e-06 ***
#   Civilization              -6.248e+02  2.187e+02   -2.856 0.004288 ** 
#   Nautical                  -8.209e+02  1.627e+02   -5.045 4.57e-07 ***
#   Exploration               -2.256e+02  1.444e+02   -1.562 0.118203    
#       Travel                    -1.859e+02  2.572e+02   -0.723 0.469910    
#   Farming                   -1.081e+03  2.529e+02   -4.274 1.93e-05 ***
#       Mythology                  7.528e+01  2.088e+02    0.360 0.718490    
#   Bluffing                  -5.316e+02  1.234e+02   -4.309 1.65e-05 ***
#   Science.Fiction           -3.231e+02  1.090e+02   -2.965 0.003034 ** 
#   Collectible.Components     5.113e+02  2.102e+02    2.432 0.015020 *  
#   Dice                      -1.636e+02  9.647e+01   -1.696 0.089832 .  
#   Fighting                   2.447e+02  1.079e+02    2.268 0.023337 *  
#   Print...Play               1.081e+03  1.595e+02    6.778 1.25e-11 ***
#      30 Maze                      -2.694e+01  2.820e+02   -0.096 0.923902    
#   Miniatures                 5.632e+02  1.342e+02    4.196 2.72e-05 ***
#       32Racing                     2.173e+02  1.616e+02    1.345 0.178748    
#       33American.West             -2.121e+02  2.520e+02   -0.842 0.399988    
#   City.Building             -1.463e+03  1.751e+02   -8.357  < 2e-16 ***
#   Wargame                    2.444e+03  9.549e+01   25.595  < 2e-16 ***
#   Adventure                  6.528e+02  1.310e+02    4.983 6.32e-07 ***
#       37Space.Exploration         -5.436e+01  2.404e+02   -0.226 0.821099    
#   Renaissance               -1.277e+03  2.415e+02   -5.285 1.27e-07 ***
#   Modern.Warfare             4.722e+02  2.083e+02    2.267 0.023391 *  
#   Humor                      4.732e+02  1.206e+02    3.924 8.74e-05 ***
#   Electronic                 6.739e+02  2.753e+02    2.448 0.014372 *  
#       42Horror                     4.968e+01  1.596e+02    0.311 0.755648    
#      43 Novel.based               -2.520e+02  1.775e+02   -1.420 0.155694    
#   Deduction                 -4.479e+02  1.357e+02   -3.301 0.000964 ***
#   Word.Game                  6.170e+02  1.846e+02    3.342 0.000833 ***
#   Aviation...Flight         -8.187e+02  2.370e+02   -3.454 0.000553 ***
#   Movies...TV...Radio.theme  3.055e+02  1.265e+02    2.416 0.015717 *  
#   Party.Game                 3.524e+02  1.070e+02    3.294 0.000990 ***
#   Memory                     7.772e+02  1.734e+02    4.482 7.43e-06 ***
#   Puzzle                    -7.559e+02  1.563e+02   -4.838 1.32e-06 ***
#   Real.time                 -9.910e+02  1.488e+02   -6.661 2.79e-11 ***
#   Trivia                     1.099e+03  1.759e+02    6.246 4.28e-10 ***
#   Industry...Manufacturing  -8.146e+02  2.344e+02   -3.476 0.000511 ***
#      54 Age.of.Reason             -3.916e+02  3.032e+02   -1.292 0.196502    
#   Trains                    -3.718e+02  2.290e+02   -1.624 0.104449    
#   Animals                   -7.828e+02  1.145e+02   -6.835 8.44e-12 ***
#   X.Childrens.Game.          6.088e+02  1.142e+02    5.331 9.89e-08 ***
#      58 Pirates                    1.276e+02  2.192e+02    0.582 0.560403    
#      59 Murder.Mystery             2.797e+02  2.283e+02    1.225 0.220672    
#   Transportation            -5.931e+02  2.161e+02   -2.745 0.006062 ** 
#   Prehistoric               -9.418e+02  3.065e+02   -3.072 0.002127 ** 
#   Sports                     1.089e+03  1.799e+02    6.054 1.44e-09 ***
#       63Action...Dexterity        -1.969e+02  1.289e+02   -1.528 0.126555    
#   Game.System                1.468e+03  7.147e+02    2.054 0.040011 *  
#       65Spies.Secret.Agents        2.072e+01  2.725e+02    0.076 0.939400    
#   Educational                6.036e+02  1.817e+02    3.322 0.000896 ***
#      66 Medical                    2.505e+02  4.136e+02    0.606 0.544717    
#      68 Mafia                      2.449e+02  3.167e+02    0.773 0.439431    
#       69Zombies                   -1.382e+02  2.808e+02   -0.492 0.622549    
#       70Comic.Book...Strip         1.536e+02  2.115e+02    0.726 0.467600    
#   Napoleonic                 6.164e+02  2.263e+02    2.724 0.006453 ** 
#   Post.Napoleonic            5.495e+02  2.563e+02    2.144 0.032016 *  
#      73 Math                       3.712e+02  3.253e+02    1.141 0.253765    
#   Book                       1.252e+03  2.983e+02    4.195 2.74e-05 ***
#   Music                      1.050e+03  4.180e+02    2.513 0.011968 *  
#      76 Environmental              1.666e+02  2.837e+02    0.587 0.557107    
#   Arabian                   -6.961e+02  3.788e+02   -1.838 0.066140 .  
#      78 Number                    -1.347e+02  2.836e+02   -0.475 0.634779    
#      79 Religious                 -2.460e+02  3.461e+02   -0.711 0.477349    
#       80Pike.and.Shot             -8.249e+01  4.656e+02   -0.177 0.859389    
#   Video.Game.Theme           3.845e+02  2.092e+02    1.838 0.066018 .  
#   Mature...Adult             1.686e+03  3.193e+02    5.281 1.30e-07 ***
#     83  Expansion.for.Base.game    1.006e+03  7.834e+02    1.284 0.199007    
#   Mean_players               1.907e+02  6.535e+01    2.919 0.003519 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3824 on 21547 degrees of freedom
# Multiple R-squared:  0.6343,	Adjusted R-squared:  0.6329 
# F-statistic: 450.3 on 83 and 21547 DF,  p-value: < 2.2e-16

mydata=mydata[,c(-9,-12,-21,-23,-30,-32,-33,-37,-42,-43,-54,-58,-59,-63,-65,-66,-68,-69,-70,-73,-76,-78,-79,-80,-83)]
mydata=mydata[,-59]

#manova (fail)-------------------------------------------------------------------------
#permutational repeated measures manova

# num=mydata[,2:8]
# num.names=colnames((num))
# cat=mydata[,10:58]
# cat.names=colnames((cat))
# formula=as.formula(paste("as.matrix(num)~",paste(cat.names, collapse = "+ "),sep=""))
# manova_result <- manova(formula,data = mydata)
# 
# # 
# set.seed(412)
# it = 10
# B=10
# n=21631
# T_stat <- numeric(B)
# T0 <- -summary.manova(manova_result ,test="Wilks")$stats[1,2]
# #
#  permMANOVA=function(num.data, cat.data,dim,it){
#    T_stat <- numeric(dim)
#    num.names=colnames((num.data))
#    cat.names=colnames((cat.data))
#    for(perm in 1:it){
#      permutation <- sample(1:dim)
#      cat.names.perm <- cat.names[permutation]
#      formula.perm=as.formula(paste("as.matrix(num)~",paste(cat.names.perm, collapse = "+ "),sep=""))
#     fit.perm <- anova(formula.perm,data = cbind(num.data,cat.data))
#     T_stat[perm] <- -summary.manova(fit.perm,test="Wilks")$stats[1,2]
#   }
#   return(T_stat)
# }
# perm_results=permMANOVA(num,cat,n,B) # non gira :') overflow
# # 
# # 
# 

# #let's try with parallel code
# library(parallel)
# 
# 
# permutation <- function(num, cat, n) {
#     num.names=colnames((num))
#     cat.names=colnames((cat))
#     permutation <- sample(1:n)
#     cat.names.perm <- cat.names[permutation]
#     formula.perm <- as.formula(paste("as.matrix(num) ~ ", paste(cat.names.perm, collapse = " + "), sep = ""))
#     fit.perm <- manova(formula.perm, data = cbind(num, cat))
#     T_stat<- -summary.manova(fit.perm, test = "Wilks")$stats[1, 2]
#   return(T_stat)
# }

# cl <- makeCluster(detectCores())
# objects_to_export <- c( "num", "cat", "n","permutation" )
# clusterExport(cl, objects_to_export)
# 
# #Use parLapply to run permMANOVA_iteration in parallel
#   T_stat <- parLapply(cl, 1:B,function(i) permutation(num,cat,n))
#   
#   # Stop the cluster
#   stopCluster(cl)

# p-value
# p_val <- sum(T_stat>=T0)/B
# p_val  



#secondo tentativo di parallel code: 
# 
# # Create a cluster
# cl <- makeCluster(detectCores())
# # Export necessary objects to the cluster
# clusterExport(cl, c("permMANOVA", "num", "cat", "n", "B"))
# # Define a function for parallel evaluation
# # permMANOVA_parallel <- function(iterations) {
# #    result <- permMANOVA(num, cat, n, B)
# #    return(result)
# #  }
# 
# # 
# #perm_results <- parLapply(cl,permMANOVA,num.data=num, cat.data=cat,dim=n,it=B)
# 
# 
# 
# perm_results <- parLapply(cl, 1:B, function(iteration) {
# 
#   result <- permMANOVA(num.data = num, cat.data = cat, dim = n, it = iteration)
#   return(result)
# })
# 
# 
# # Stop the cluster
# stopCluster(cl)
# 
# # Combine the results if needed
# combined_results <- do.call(rbind, perm_results)
# 
# 
# hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
# abline(v=T0,col=3,lwd=2)
# 
# plot(ecdf(T_stat),xlim=c(-2,1))
# abline(v=T0,col=3,lwd=4)
# 
# 
# p-value
# p_val <- sum(T_stat>=T0)/B
# p_val
# 


#functions----------------------------------------------------------------------------------
perm_sel=function(model){
  pv=pv_extr(model)
  if(model$type=="lm"){
    covar.names=gsub("ns\\(([^,]+),.*", "\\1", names(pv[pv>0.1]))
    #covar.names=gsub("ns\\(([^,]+),.*", "\\1", names(pv))
    R2adj.perm=names(covar.names)
    for(i in covar.names){
      mydata.perm=mydata
      mydata.perm[[i]]=sample(mydata.perm[[i]])
      model_perm <-lm(formula, data = mydata.perm)
      R2adj.perm[i]=summary(model_perm)$adj.r.squared
    }
  }
  else{
    covar.names=gsub("s\\((.*?)\\)", "\\1", names(pv[pv>0.1]))
    #covar.names=gsub("s\\((.*?)\\)", "\\1", names(pv))
    R2adj.perm=names(covar.names)
    for(i in covar.names){
      mydata.perm=mydata
      mydata.perm[[i]]=sample(mydata.perm[[i]])
      model_perm <-gam(formula, data = mydata.perm)
      R2adj.perm[i]=summary(model_perm)$r.sq
    }
  }
  return(R2adj.perm)
}


pv_extr=function(model){
  if(model$type=="lm"){
    return(summary(model)$coefficients[, "Pr(>|t|)"])
  }
  else{
    return(summary(model)$s.table[,4]) #non funziona bene
  }
}

R_extr=function(model){
  if(model$type=="lm"){
    return(summary(model)$adj.r.squared)
  }
  else{
    return(summary(model)$r.sq)
  }
}


#gam------------------------------------------------------------
#1)Random effects-----------------------------------------------
names="Rank~"
for(i in colnames(mydata[,c(-1,-2)])){
  names=paste(names, "+ ","s(",i,",bs='re' )")
}
formula=as.formula(names)
model_re=gam(formula,data = mydata)
model_re$type="gam"
summary(model_re)

# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  42538.1      211.7     201   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#                                   edf     Ref.df    F       p-value    
#   s(Avg_ratings)               9.980e-01      1 2.430e+09  < 2e-16 ***
#   s(Users_rated)               1.000e+00      1 4.075e+09  < 2e-16 ***
#   s(Max_players)               4.890e-07      1 0.000e+00 0.470761    
#   s(Playing_time)              4.120e-01      1 4.192e+02 0.188053    
#   s(Min_age)                   9.792e-01      1 1.076e+06  < 2e-16 ***
#   s(Owned)                     1.000e+00      1 1.089e+10  < 2e-16 ***
#   s(Negotiation)               9.376e-01      1 5.682e+04 0.002702 ** 
#   s(Political)                 8.600e-01      1 2.417e+04 0.008145 ** 
#   s(Fantasy)                   6.698e-01      1 9.583e+03 0.085930 .  
#   s(Abstract.Strategy)         8.152e-01      1 7.496e+03 0.021010 *  
#   s(Medieval)                  9.164e-01      1 5.467e+05 1.56e-05 ***
#   s(Ancient)                   9.555e-01      1 8.882e+05  < 2e-16 ***
#   s(Territory.Building)        8.931e-01      1 8.215e+05 5.18e-05 ***
#   s(Civilization)              8.446e-01      1 6.855e+05 0.001485 ** 
#   s(Nautical)                  9.673e-01      1 9.176e+04  < 2e-16 ***
#   s(Exploration)               7.995e-01      1 4.311e+03 0.014976 *  
#   s(Farming)                   9.303e-01      1 9.832e+05 2.22e-05 ***
#   s(Bluffing)                  9.358e-01      1 1.274e+05 3.17e-06 ***
#   s(Science.Fiction)           8.921e-01      1 2.585e+03 0.003022 ** 
#   s(Collectible.Components)    2.509e-07      1 0.000e+00 0.661062    
#   s(Dice)                      2.124e-07      1 0.000e+00 0.983066    
#   s(Fighting)                  4.721e-07      1 0.000e+00 0.470946    
#   s(Print...Play)              9.879e-01      1 1.805e+05  < 2e-16 ***
#   s(Miniatures)                9.625e-01      1 1.417e+05 6.16e-07 ***
#   s(City.Building)             9.657e-01      1 8.362e+06  < 2e-16 ***
#   s(Wargame)                   9.856e-01      1 2.629e+07  < 2e-16 ***
#   s(Adventure)                 9.983e-01      1 1.491e+05 2.75e-05 ***
#   s(Renaissance)               9.603e-01      1 1.119e+06  < 2e-16 ***
#   s(Modern.Warfare)            8.252e-01      1 2.117e+04 0.019588 *  
#   s(Humor)                     9.239e-01      1 1.546e+05 3.06e-05 ***
#   s(Electronic)                4.640e-01      1 8.770e+01 0.143977    
#   s(Deduction)                 6.366e-01      1 1.972e+03 0.092342 .  
#   s(Word.Game)                 6.321e-01      1 6.310e+03 0.070008 .  
#   s(Aviation...Flight)         9.344e-01      1 2.106e+04 0.000183 ***
#   s(Movies...TV...Radio.theme) 8.394e-01      1 8.933e+04 0.003092 ** 
#   s(Party.Game)                9.009e-01      1 1.468e+05 5.81e-07 ***
#   s(Memory)                    9.137e-01      1 2.029e+04 0.000309 ***
#   s(Puzzle)                    9.055e-01      1 3.495e+03 0.000674 ***
#   s(Real.time)                 9.739e-01      1 2.594e+04  < 2e-16 ***
#   s(Trivia)                    9.291e-01      1 1.106e+05  < 2e-16 ***
#   s(Industry...Manufacturing)  8.952e-01      1 2.730e+05 0.000208 ***
#   s(Trains)                    8.861e-01      1 8.327e+04 0.000476 ***
#   s(Animals)                   9.730e-01      1 1.035e+05  < 2e-16 ***
#   s(X.Childrens.Game.)         9.236e-01      1 6.358e+04 1.14e-05 ***
#   s(Transportation)            8.491e-01      1 1.375e+05 0.000241 ***
#   s(Prehistoric)               8.797e-01      1 2.112e+04 0.001938 ** 
#   s(Sports)                    9.124e-01      1 1.293e+04 0.000411 ***
#   s(Game.System)               2.840e-01      1 4.687e+01 0.227449    
#   s(Medical)                   2.044e-01      1 3.007e+01 0.267170    
#   s(Napoleonic)                7.644e-01      1 8.230e+03 0.027124 *  
#   s(Post.Napoleonic)           6.699e-01      1 6.454e+01 0.080870 .  
#   s(Book)                      9.270e-01      1 1.867e+04 4.14e-05 ***
#   s(Music)                     7.745e-01      1 8.140e+02 0.021779 *  
#   s(Arabian)                   7.604e-01      1 9.116e+03 0.044457 *  
#   s(Video.Game.Theme)          9.026e-01      1 3.379e+04 0.002835 ** 
#   s(Mature...Adult)            9.626e-01      1 1.309e+05  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.621   Deviance explained = 62.2%
# GCV = 1.5132e+07  Scale est. = 1.51e+07  n = 21631

R2adj=R_extr(model_re)
R2adj.perm=perm_sel(model_re)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#if the difference in terms of R^2adj is not so big we can delete those variables
#all true
#let's delete them:
to_delete_ind=which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE]))
names="Rank~"
for(i in colnames(mydata[,c(-1,-2,-to_delete_ind)])){
  names=paste(names, "+ ","s(",i,",bs='re' )")
}
formula=as.formula(names)
model_re1=gam(formula,data = mydata)
model_re1$type="gam"
summary(model_re1)

# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  42535.2      211.5   201.1   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df         F  p-value    
#   s(Avg_ratings)               0.9979      1 1.403e+07  < 2e-16 ***
#   s(Users_rated)               1.0000      1 1.947e+09  < 2e-16 ***
#   s(Min_age)                   0.9799      1 5.602e+05  < 2e-16 ***
#   s(Owned)                     0.9999      1 1.087e+10  < 2e-16 ***
#   s(Negotiation)               0.9428      1 3.086e+03 0.002473 ** 
#   s(Political)                 0.8590      1 8.656e+02 0.008105 ** 
#   s(Fantasy)                   0.6539      1 2.232e+03 0.084319 .  
#   s(Abstract.Strategy)         0.8229      1 3.225e+02 0.019576 *  
#   s(Medieval)                  0.9171      1 2.194e+04 1.55e-05 ***
#   s(Ancient)                   0.9561      1 1.310e+04  < 2e-16 ***
#   s(Territory.Building)        0.8960      1 3.848e+04 4.99e-05 ***
#   s(Civilization)              0.8331      1 1.674e+04 0.001498 ** 
#   s(Nautical)                  0.9667      1 1.220e+03  < 2e-16 ***
#   s(Exploration)               0.7668      1 9.024e+03 0.015864 *  
#   s(Farming)                   0.9305      1 2.497e+04 2.25e-05 ***
#   s(Bluffing)                  0.9390      1 9.661e+03 2.77e-06 ***
#   s(Science.Fiction)           0.8954      1 2.578e+03 0.002845 ** 
#   s(Print...Play)              0.9890      1 1.226e+04  < 2e-16 ***
#   s(Miniatures)                0.9620      1 7.781e+03 6.73e-07 ***
#   s(City.Building)             0.9658      1 3.543e+05  < 2e-16 ***
#   s(Wargame)                   0.9878      1 1.481e+06  < 2e-16 ***
#   s(Adventure)                 0.9999      1 1.735e+04 2.85e-05 ***
#   s(Renaissance)               0.9607      1 3.116e+04  < 2e-16 ***
#   s(Modern.Warfare)            0.8272      1 1.008e+03 0.019927 *  
#   s(Humor)                     0.9202      1 3.754e+02 3.14e-05 ***
#   s(Deduction)                 0.6313      1 6.762e+02 0.095964 .  
#   s(Word.Game)                 0.6401      1 4.139e+01 0.070010 .  
#   s(Aviation...Flight)         0.9346      1 8.717e+02 0.000179 ***
#   s(Movies...TV...Radio.theme) 0.8519      1 4.684e+02 0.002794 ** 
#   s(Party.Game)                0.9099      1 1.501e+03 4.35e-07 ***
#   s(Memory)                    0.9166      1 2.171e+03 0.000294 ***
#   s(Puzzle)                    0.9030      1 6.821e+02 0.000695 ***
#   s(Real.time)                 0.9722      1 3.901e+02  < 2e-16 ***
#   s(Trivia)                    0.9283      1 7.858e+03  < 2e-16 ***
#   s(Industry...Manufacturing)  0.8950      1 8.083e+03 0.000217 ***
#   s(Trains)                    0.8747      1 1.334e+03 0.000525 ***
#   s(Animals)                   0.9746      1 5.024e+03  < 2e-16 ***
#   s(X.Childrens.Game.)         0.9278      1 1.235e+04 9.30e-06 ***
#   s(Transportation)            0.8613      1 7.682e+02 0.000228 ***
#   s(Prehistoric)               0.8811      1 5.122e+02 0.001884 ** 
#   s(Sports)                    0.9119      1 1.734e+03 0.000399 ***
#   s(Napoleonic)                0.7667      1 8.894e+02 0.027440 *  
#   s(Post.Napoleonic)           0.6654      1 3.372e+01 0.081926 .  
#   s(Book)                      0.9268      1 3.816e+03 4.26e-05 ***
#   s(Music)                     0.7869      1 9.794e+01 0.020579 *  
#   s(Arabian)                   0.7620      1 7.610e+01 0.043870 *  
#   s(Video.Game.Theme)          0.9039      1 1.856e+02 0.002760 ** 
#   s(Mature...Adult)            0.9612      1 3.722e+02  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.621   Deviance explained = 62.2%
# GCV = 1.5133e+07  Scale est. = 1.5102e+07  n = 21631






#2)natural splines--------------------------------------------------------------

#2.1) degree 3 for all covariates------------------------------------------------
names="Rank~"
for(i in colnames(mydata[,c(-1,-2)])){
  names=paste(names, "+ ","ns(",i,",df=3)")
}
formula=as.formula(names)


model_ns <-lm(formula, data = mydata)
model_ns$type="lm"
summary(model_ns)

#UGLY!!!
#probably is over fitting data
#i don't even show you the summary, trust me. Just know that:

## Residual standard error: 2282 on 21559 degrees of freedom
# Multiple R-squared:  0.8698,	Adjusted R-squared:  0.8693 
# F-statistic:  2028 on 71 and 21559 DF,  p-value: < 2.2e-16

#i don't even try to do perm test since the degree of the splines of categ. variables is too high
#let's reduce the latter

#2.2) degree 3 for num and 1 for cat----------------------------------------------
names="Rank~"
for(i in colnames(mydata[,3:8])){
  names=paste(names, "+ ","ns(",i,",df=3)")
}

for(i in colnames(mydata[,9:ncol(mydata)])){
  names=paste(names, "+ ","ns(",i,",df=1)")
}

formula=as.formula(names)
model_ns1 <-lm(formula, data = mydata)
model_ns1$type="lm"
summary(model_ns1)

# hist(model_gam_ns1$residuals)
# qqnorm(model_gam_ns1$residuals)
# #pretty normal :')


# Call:
#   lm(formula = formula, data = mydata)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -16572.9  -1454.5     22.8   1251.0  16641.7 
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                            23567.02     386.64  60.953  < 2e-16 ***
#   ns(Avg_ratings, df = 3)1              -18929.32     190.99 -99.114  < 2e-16 ***
#   ns(Avg_ratings, df = 3)2               -4500.18     782.67  -5.750 9.05e-09 ***
#   ns(Avg_ratings, df = 3)3               -8658.69     266.55 -32.485  < 2e-16 ***
#   ns(Users_rated, df = 3)1                1545.25    1529.16   1.011 0.312256    
#   ns(Users_rated, df = 3)2                4710.51    2181.92   2.159 0.030870 *  
#   ns(Users_rated, df = 3)3               23538.70    4467.57   5.269 1.39e-07 ***
#   ns(Max_players, df = 3)1                  34.38     675.68   0.051 0.959421    
#   ns(Max_players, df = 3)2                -687.97     718.93  -0.957 0.338604    
#   ns(Max_players, df = 3)3                -486.34    1314.92  -0.370 0.711487    
#   ns(Playing_time, df = 3)1              -1087.31    1283.56  -0.847 0.396950    
#   ns(Playing_time, df = 3)2               1123.88    1195.45   0.940 0.347163    
#   ns(Playing_time, df = 3)3               2326.13    2249.07   1.034 0.301025    
#   ns(Min_age, df = 3)1                    -435.62     115.40  -3.775 0.000161 ***
#   ns(Min_age, df = 3)2                    1095.84     304.19   3.602 0.000316 ***
#   ns(Min_age, df = 3)3                    2444.16     461.83   5.292 1.22e-07 ***
#   ns(Owned, df = 3)1                    -25089.96    1576.39 -15.916  < 2e-16 ***
#   ns(Owned, df = 3)2                    -21747.30    2379.58  -9.139  < 2e-16 ***
#   ns(Owned, df = 3)3                    -15078.70    4875.84  -3.093 0.001987 ** 
#   ns(Negotiation, df = 1)                  364.26     117.99   3.087 0.002023 ** 
#   ns(Political, df = 1)                   -219.68     135.06  -1.627 0.103835    
#   ns(Fantasy, df = 1)                      117.92      65.77   1.793 0.073009 .  
#   ns(Abstract.Strategy, df = 1)             16.71      81.07   0.206 0.836673    
#   ns(Medieval, df = 1)                    -245.91      94.01  -2.616 0.008907 ** 
#   ns(Ancient, df = 1)                     -380.42     111.67  -3.407 0.000659 ***
#   ns(Territory.Building, df = 1)          -136.97     131.64  -1.040 0.298126    
#   ns(Civilization, df = 1)                -655.55     161.98  -4.047 5.20e-05 ***
#   ns(Nautical, df = 1)                    -162.20     117.18  -1.384 0.166293    
#   ns(Exploration, df = 1)                  -72.42     106.57  -0.680 0.496793    
#   ns(Farming, df = 1)                     -339.88     187.55  -1.812 0.069969 .  
#   ns(Bluffing, df = 1)                    -219.34      90.65  -2.420 0.015546 *  
#   ns(Science.Fiction, df = 1)               73.00      77.51   0.942 0.346294    
#   ns(Collectible.Components, df = 1)       283.26     155.92   1.817 0.069272 .  
#   ns(Dice, df = 1)                         151.56      70.65   2.145 0.031955 *  
#   ns(Fighting, df = 1)                      47.05      79.79   0.590 0.555409    
#   ns(Print...Play, df = 1)                 492.40     120.38   4.090 4.32e-05 ***
#   ns(Miniatures, df = 1)                    45.13      99.90   0.452 0.651455    
#   ns(City.Building, df = 1)               -583.12     129.57  -4.500 6.82e-06 ***
#   ns(Wargame, df = 1)                      696.48      72.81   9.566  < 2e-16 ***
#   ns(Adventure, df = 1)                    263.08      96.37   2.730 0.006342 ** 
#   ns(Renaissance, df = 1)                 -767.11     178.69  -4.293 1.77e-05 ***
#   ns(Modern.Warfare, df = 1)               489.87     154.64   3.168 0.001538 ** 
#   ns(Humor, df = 1)                        353.20      89.24   3.958 7.59e-05 ***
#   ns(Electronic, df = 1)                   504.37     204.27   2.469 0.013553 *  
#   ns(Deduction, df = 1)                     94.45      92.82   1.018 0.308900    
#   ns(Word.Game, df = 1)                     84.78     136.39   0.622 0.534209    
#   ns(Aviation...Flight, df = 1)           -145.53     176.26  -0.826 0.409004    
#   ns(Movies...TV...Radio.theme, df = 1)    276.87      93.63   2.957 0.003109 ** 
#   ns(Party.Game, df = 1)                   354.47      78.68   4.505 6.67e-06 ***
#   ns(Memory, df = 1)                        34.50     128.97   0.267 0.789094    
#   ns(Puzzle, df = 1)                      -189.03     114.86  -1.646 0.099839 .  
#   ns(Real.time, df = 1)                   -359.83     110.68  -3.251 0.001152 ** 
#   ns(Trivia, df = 1)                        55.24     128.91   0.429 0.668264    
#   ns(Industry...Manufacturing, df = 1)    -432.17     170.04  -2.542 0.011043 *  
#   ns(Trains, df = 1)                      -340.37     168.04  -2.026 0.042824 *  
#   ns(Animals, df = 1)                      -90.33      84.65  -1.067 0.285956    
#   ns(X.Childrens.Game., df = 1)             35.91      84.55   0.425 0.671072    
#   ns(Transportation, df = 1)              -459.70     159.05  -2.890 0.003852 ** 
#   ns(Prehistoric, df = 1)                 -338.23     227.35  -1.488 0.136846    
#   ns(Sports, df = 1)                        18.12     131.73   0.138 0.890602    
#   ns(Game.System, df = 1)                  542.45     531.07   1.021 0.307068    
#   ns(Medical, df = 1)                     -328.49     307.32  -1.069 0.285135    
#   ns(Napoleonic, df = 1)                   165.07     168.35   0.980 0.326860    
#   ns(Post.Napoleonic, df = 1)               85.11     190.59   0.447 0.655183    
#   ns(Book, df = 1)                         -73.60     222.33  -0.331 0.740620    
#   ns(Music, df = 1)                        710.71     311.02   2.285 0.022318 *  
#   ns(Arabian, df = 1)                     -387.80     281.75  -1.376 0.168720    
#   ns(Video.Game.Theme, df = 1)             659.65     155.46   4.243 2.21e-05 ***
#   ns(Mature...Adult, df = 1)               200.12     260.39   0.769 0.442182    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2282 on 21562 degrees of freedom
# Multiple R-squared:  0.8697,	Adjusted R-squared:  0.8693 
# F-statistic:  2117 on 68 and 21562 DF,  p-value: < 2.2e-16

#gam::plot.Gam(model_ns1, se=TRUE)


R2adj=R_extr(model_ns1)
R2adj.perm=perm_sel(model_ns1)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#all true again
#let's delete them:
to_delete_ind=which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE]))
names="Rank~"
for(i in colnames(mydata[,c(3:8)[!c(3:8)%in%to_delete_ind[which(to_delete_ind<=8)]]])){
  names=paste(names, "+ ","ns(",i,",df=3)")
}

for(i in colnames(mydata[,c(-seq(1:8),-to_delete_ind[which(to_delete_ind>8)])])){
  names=paste(names, "+ ","ns(",i,",df=1)")
}

formula=as.formula(names)
model_ns1.1<-lm(formula, data = mydata)
model_ns1.1$type="lm"
summary(model_ns1.1)

# Call:
#   lm(formula = formula, data = mydata)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -20234.9  -1721.0   -147.1   1632.8  15439.0 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                            24887.65     413.83  60.140  < 2e-16 ***
#   ns(Avg_ratings, df = 3)1              -19293.01     209.30 -92.177  < 2e-16 ***
#   ns(Avg_ratings, df = 3)2               -4283.86     862.52  -4.967 6.86e-07 ***
#   ns(Avg_ratings, df = 3)3               -8945.46     290.48 -30.796  < 2e-16 ***
#   ns(Min_age, df = 3)1                     100.12     113.75   0.880 0.378777    
#   ns(Min_age, df = 3)2                     379.56     310.05   1.224 0.220886    
#   ns(Min_age, df = 3)3                    1953.97     457.72   4.269 1.97e-05 ***
#   ns(Owned, df = 3)1                    -25383.53     375.53 -67.593  < 2e-16 ***
#   ns(Owned, df = 3)2                    -17754.49     492.41 -36.056  < 2e-16 ***
#   ns(Owned, df = 3)3                     11483.99    1085.24  10.582  < 2e-16 ***
#   ns(Negotiation, df = 1)                  274.60     127.02   2.162 0.030640 *  
#   ns(Fantasy, df = 1)                      189.84      69.82   2.719 0.006554 ** 
#   ns(Medieval, df = 1)                    -315.98     102.89  -3.071 0.002136 ** 
#   ns(Ancient, df = 1)                     -533.69     122.41  -4.360 1.31e-05 ***
#   ns(Civilization, df = 1)                -878.27     176.69  -4.971 6.73e-07 ***
#   ns(Farming, df = 1)                     -379.48     205.84  -1.844 0.065260 .  
#   ns(Bluffing, df = 1)                    -341.39      95.92  -3.559 0.000373 ***
#   ns(Collectible.Components, df = 1)       -58.01     169.19  -0.343 0.731705    
#   ns(Dice, df = 1)                         158.01      77.60   2.036 0.041750 *  
#   ns(Print...Play, df = 1)                 428.56     130.70   3.279 0.001044 ** 
#   ns(City.Building, df = 1)               -742.48     142.21  -5.221 1.80e-07 ***
#   ns(Wargame, df = 1)                     1983.60      67.02  29.597  < 2e-16 ***
#   ns(Adventure, df = 1)                    270.29      99.08   2.728 0.006379 ** 
#   ns(Renaissance, df = 1)                 -871.28     196.40  -4.436 9.20e-06 ***
#   ns(Modern.Warfare, df = 1)               509.33     168.05   3.031 0.002442 ** 
#   ns(Humor, df = 1)                        288.56      97.30   2.966 0.003023 ** 
#   ns(Electronic, df = 1)                   431.69     224.94   1.919 0.054983 .  
#   ns(Movies...TV...Radio.theme, df = 1)    666.14     101.07   6.591 4.48e-11 ***
#   ns(Party.Game, df = 1)                   537.53      80.42   6.684 2.39e-11 ***
#   ns(Puzzle, df = 1)                      -104.92     126.00  -0.833 0.405000    
#   ns(Real.time, df = 1)                   -410.84     121.11  -3.392 0.000694 ***
#   ns(Industry...Manufacturing, df = 1)    -685.00     187.22  -3.659 0.000254 ***
#   ns(Trains, df = 1)                      -591.07     184.21  -3.209 0.001335 ** 
#   ns(Transportation, df = 1)              -610.45     174.38  -3.501 0.000465 ***
#   ns(Music, df = 1)                        765.05     343.09   2.230 0.025764 *  
#   ns(Video.Game.Theme, df = 1)             861.49     171.12   5.034 4.83e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2522 on 21595 degrees of freedom
# Multiple R-squared:  0.8406,	Adjusted R-squared:  0.8403 
# F-statistic:  3253 on 35 and 21595 DF,  p-value: < 2.2e-16

#let's do perm sel one more time
R2adj=R_extr(model_ns1.1)
R2adj.perm=perm_sel(model_ns1.1)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#all true again
#let's delete them:
to_delete_ind=cbind(to_delete_ind,which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE])))
names="Rank~"
for(i in colnames(mydata[,c(3:8)[!c(3:8)%in%to_delete_ind[which(to_delete_ind<=8)]]])){
  names=paste(names, "+ ","ns(",i,",df=3)")
}

for(i in colnames(mydata[,c(-seq(1:8),-to_delete_ind[which(to_delete_ind>8)])])){
  names=paste(names, "+ ","ns(",i,",df=1)")
}

formula=as.formula(names)
model_ns1.2<-lm(formula, data = mydata)
summary(model_ns1.2)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -20205.9  -1728.0   -142.3   1645.4  15386.9 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                            24709.94     411.20  60.092  < 2e-16 ***
#   ns(Avg_ratings, df = 3)1              -19320.13     209.33 -92.293  < 2e-16 ***
#   ns(Avg_ratings, df = 3)2               -4499.22     863.59  -5.210 1.91e-07 ***
#   ns(Avg_ratings, df = 3)3               -8748.71     290.22 -30.145  < 2e-16 ***
#   ns(Owned, df = 3)1                    -25440.19     375.89 -67.680  < 2e-16 ***
#   ns(Owned, df = 3)2                    -17845.01     492.99 -36.197  < 2e-16 ***
#   ns(Owned, df = 3)3                     11340.92    1087.22  10.431  < 2e-16 ***
#   ns(Negotiation, df = 1)                  307.71     127.11   2.421 0.015492 *  
#   ns(Fantasy, df = 1)                      216.96      69.35   3.129 0.001759 ** 
#   ns(Medieval, df = 1)                    -306.21     102.99  -2.973 0.002951 ** 
#   ns(Ancient, df = 1)                     -522.50     122.58  -4.263 2.03e-05 ***
#   ns(Civilization, df = 1)                -804.64     176.66  -4.555 5.27e-06 ***
#   ns(Farming, df = 1)                     -395.90     206.22  -1.920 0.054898 .  
#   ns(Bluffing, df = 1)                    -357.61      95.98  -3.726 0.000195 ***
#   ns(Dice, df = 1)                         135.23      77.58   1.743 0.081342 .  
#   ns(Print...Play, df = 1)                 428.74     130.79   3.278 0.001047 ** 
#   ns(City.Building, df = 1)               -744.17     142.37  -5.227 1.74e-07 ***
#   ns(Wargame, df = 1)                     2095.51      63.60  32.947  < 2e-16 ***
#   ns(Adventure, df = 1)                    298.12      99.03   3.010 0.002613 ** 
#   ns(Renaissance, df = 1)                 -829.06     196.56  -4.218 2.48e-05 ***
#   ns(Modern.Warfare, df = 1)               537.84     168.34   3.195 0.001400 ** 
#   ns(Humor, df = 1)                        367.33      97.01   3.787 0.000153 ***
#   ns(Electronic, df = 1)                   425.02     225.23   1.887 0.059163 .  
#   ns(Movies...TV...Radio.theme, df = 1)    703.31     100.83   6.975 3.14e-12 ***
#   ns(Party.Game, df = 1)                   633.53      79.55   7.964 1.75e-15 ***
#   ns(Real.time, df = 1)                   -496.85     119.20  -4.168 3.08e-05 ***
#   ns(Industry...Manufacturing, df = 1)    -620.49     187.10  -3.316 0.000914 ***
#   ns(Trains, df = 1)                      -522.71     184.35  -2.835 0.004580 ** 
#   ns(Transportation, df = 1)              -592.08     174.65  -3.390 0.000700 ***
#   ns(Music, df = 1)                        836.27     343.64   2.434 0.014959 *  
#   ns(Video.Game.Theme, df = 1)             876.81     171.23   5.121 3.07e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2527 on 21600 degrees of freedom
# Multiple R-squared:  0.8399,	Adjusted R-squared:  0.8397 
# F-statistic:  3777 on 30 and 21600 DF,  p-value: < 2.2e-16


#2.3) degree 2 for num and 1 for cat----------------------------------------------
names="Rank~"
for(i in colnames(mydata[,3:9])){
  names=paste(names, "+ ","ns(",i,",df=2)")
}

for(i in colnames(mydata[,10:ncol(mydata)])){
  names=paste(names, "+ ","ns(",i,",df=1)")
}

formula=as.formula(names)
model_ns2 <-lm(formula, data = mydata)
model_ns2$type="lm"
summary(model_ns2)

# hist(model_gam_ns2$residuals)
# qqnorm(model_gam_ns2$residuals)
# #pretty normal :')

# Residuals:
#   Min     1Q Median     3Q    Max 
# -38818  -2819    333   2709  23573 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            51184.93     861.19   59.435  < 2e-16 ***
#   ns(Avg_ratings, df = 2)1              -70087.29     754.84  -92.851  < 2e-16 ***
#   ns(Avg_ratings, df = 2)2              -26190.44     196.71 -133.144  < 2e-16 ***
#   ns(Users_rated, df = 2)1               55889.80    3776.99   14.797  < 2e-16 ***
#   ns(Users_rated, df = 2)2               25791.33    7224.39    3.570 0.000358 ***
#   ns(Max_players, df = 2)1                -920.00    1453.66   -0.633 0.526815    
#   ns(Max_players, df = 2)2               -1834.92    2120.16   -0.865 0.386794    
#   ns(Playing_time, df = 2)1              -3021.56    2539.56   -1.190 0.234139    
#   ns(Playing_time, df = 2)2               2169.87    3682.67    0.589 0.555726    
#   ns(Min_age, df = 2)1                   -2395.32     208.26  -11.501  < 2e-16 ***
#   ns(Min_age, df = 2)2                    1183.29     393.05    3.011 0.002611 ** 
#   ns(Owned, df = 2)1                    -80155.44    4059.97  -19.743  < 2e-16 ***
#   ns(Owned, df = 2)2                     16107.01    7862.95    2.048 0.040526 *  
#   ns(Negotiation, df = 2)1               -3692.44    1388.65   -2.659 0.007843 ** 
#   ns(Negotiation, df = 2)2                     NA         NA       NA       NA    
#   ns(Political, df = 1)                   -639.91     211.96   -3.019 0.002539 ** 
#   ns(Fantasy, df = 1)                     -162.46     103.12   -1.575 0.115185    
#   ns(Abstract.Strategy, df = 1)            472.00     124.73    3.784 0.000155 ***
#   ns(Medieval, df = 1)                    -458.59     147.63   -3.106 0.001897 ** 
#   ns(Ancient, df = 1)                    -1086.42     175.34   -6.196 5.89e-10 ***
#   ns(Territory.Building, df = 1)          -793.68     206.71   -3.840 0.000124 ***
#   ns(Civilization, df = 1)                -956.94     254.53   -3.760 0.000171 ***
#   ns(Nautical, df = 1)                    -784.67     184.09   -4.262 2.03e-05 ***
#   ns(Exploration, df = 1)                 -245.83     167.38   -1.469 0.141934    
#   ns(Farming, df = 1)                     -915.27     294.79   -3.105 0.001906 ** 
#   ns(Bluffing, df = 1)                    -331.22     142.33   -2.327 0.019967 *  
#   ns(Science.Fiction, df = 1)             -272.08     121.49   -2.240 0.025129 *  
#   ns(Collectible.Components, df = 1)        65.08     243.48    0.267 0.789256    
#   ns(Dice, df = 1)                          32.14     110.88    0.290 0.771898    
#   ns(Fighting, df = 1)                     182.80     125.20    1.460 0.144273    
#   ns(Print...Play, df = 1)                1165.90     186.15    6.263 3.85e-10 ***
#   ns(Miniatures, df = 1)                   265.38     156.49    1.696 0.089935 .  
#   ns(City.Building, df = 1)              -1527.32     203.38   -7.510 6.15e-14 ***
#   ns(Wargame, df = 1)                     1502.92     101.89   14.750  < 2e-16 ***
#   ns(Adventure, df = 1)                    705.75     151.28    4.665 3.10e-06 ***
#   ns(Renaissance, df = 1)                -1757.59     280.62   -6.263 3.84e-10 ***
#   ns(Modern.Warfare, df = 1)               562.99     242.98    2.317 0.020515 *  
#   ns(Humor, df = 1)                        663.31     140.09    4.735 2.21e-06 ***
#   ns(Electronic, df = 1)                   530.17     320.95    1.652 0.098574 .  
#   ns(Deduction, df = 1)                   -129.19     145.87   -0.886 0.375818    
#   ns(Word.Game, df = 1)                    502.94     214.41    2.346 0.018998 *  
#   ns(Aviation...Flight, df = 1)          -1117.48     276.81   -4.037 5.43e-05 ***
#   ns(Movies...TV...Radio.theme, df = 1)    300.59     146.82    2.047 0.040639 *  
#   ns(Party.Game, df = 1)                   595.68     123.41    4.827 1.40e-06 ***
#   ns(Memory, df = 1)                       715.58     202.43    3.535 0.000409 ***
#   ns(Puzzle, df = 1)                      -624.71     180.49   -3.461 0.000539 ***
#   ns(Real.time, df = 1)                   -644.43     172.97   -3.726 0.000195 ***
#   ns(Trivia, df = 1)                       784.31     201.91    3.884 0.000103 ***
#   ns(Industry...Manufacturing, df = 1)    -997.05     266.89   -3.736 0.000188 ***
#   ns(Trains, df = 1)                     -1232.40     263.64   -4.675 2.96e-06 ***
#   ns(Animals, df = 1)                     -508.59     132.79   -3.830 0.000129 ***
#   ns(X.Childrens.Game., df = 1)            427.74     129.43    3.305 0.000952 ***
#   ns(Transportation, df = 1)              -843.24     249.93   -3.374 0.000743 ***
#   ns(Prehistoric, df = 1)                 -940.68     357.29   -2.633 0.008474 ** 
#   ns(Sports, df = 1)                       381.28     205.82    1.853 0.063962 .  
#   ns(Game.System, df = 1)                  873.89     834.76    1.047 0.295170    
#   ns(Medical, df = 1)                     -362.64     483.12   -0.751 0.452894    
#   ns(Napoleonic, df = 1)                   294.99     264.66    1.115 0.265023    
#   ns(Post.Napoleonic, df = 1)              677.25     299.35    2.262 0.023684 *  
#   ns(Book, df = 1)                         773.29     348.73    2.217 0.026604 *  
#   ns(Music, df = 1)                        893.97     488.85    1.829 0.067452 .  
#   ns(Arabian, df = 1)                     -657.60     442.96   -1.485 0.137671    
#   ns(Video.Game.Theme, df = 1)             681.85     244.35    2.790 0.005268 ** 
#   ns(Mature...Adult, df = 1)              1017.44     391.41    2.599 0.009344 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3588 on 21568 degrees of freedom
# Multiple R-squared:  0.6778,	Adjusted R-squared:  0.6769 
# F-statistic: 731.8 on 62 and 21568 DF,  p-value: < 2.2e-16

#gam::plot.Gam(model_ns2, se=TRUE)

R2adj=R_extr(model_ns2)
R2adj.perm=perm_sel(model_ns2)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#all true again
#let's delete them:
to_delete_ind=which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE]))
names="Rank~"
for(i in colnames(mydata[,c(3:8)[!c(3:8)%in%to_delete_ind[which(to_delete_ind<=8)]]])){
  names=paste(names, "+ ","ns(",i,",df=2)")
}

for(i in colnames(mydata[,c(-seq(1:8),-to_delete_ind[which(to_delete_ind>8)])])){
  names=paste(names, "+ ","ns(",i,",df=1)")
}

formula=as.formula(names)
model_ns2.1<-lm(formula, data = mydata)
model_ns2.1$type="lm"
summary(model_ns2.1)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -38813  -2815    337   2714  23609 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            49108.91     419.68  117.016  < 2e-16 ***
#   ns(Avg_ratings, df = 2)1              -70060.05     753.70  -92.954  < 2e-16 ***
#   ns(Avg_ratings, df = 2)2              -26242.27     193.88 -135.355  < 2e-16 ***
#   ns(Users_rated, df = 2)1               56286.61    3770.14   14.930  < 2e-16 ***
#   ns(Users_rated, df = 2)2               25862.10    7221.07    3.581 0.000342 ***
#   ns(Min_age, df = 2)1                   -2442.04     207.24  -11.784  < 2e-16 ***
#   ns(Min_age, df = 2)2                    1122.74     391.62    2.867 0.004149 ** 
#   ns(Owned, df = 2)1                    -80582.64    4051.58  -19.889  < 2e-16 ***
#   ns(Owned, df = 2)2                     16058.04    7857.62    2.044 0.041003 *  
#   ns(Negotiation, df = 1)                  488.51     184.64    2.646 0.008158 ** 
#   ns(Political, df = 1)                   -633.00     211.68   -2.990 0.002790 ** 
#   ns(Abstract.Strategy, df = 1)            498.39     123.53    4.035 5.49e-05 ***
#   ns(Medieval, df = 1)                    -463.51     147.10   -3.151 0.001629 ** 
#   ns(Ancient, df = 1)                    -1070.10     174.83   -6.121 9.47e-10 ***
#   ns(Territory.Building, df = 1)          -801.52     206.53   -3.881 0.000104 ***
#   ns(Civilization, df = 1)                -977.66     253.87   -3.851 0.000118 ***
#   ns(Nautical, df = 1)                    -774.62     183.57   -4.220 2.46e-05 ***
#   ns(Farming, df = 1)                     -902.72     294.52   -3.065 0.002179 ** 
#   ns(Bluffing, df = 1)                    -355.37     136.88   -2.596 0.009431 ** 
#   ns(Science.Fiction, df = 1)             -236.58     119.82   -1.974 0.048339 *  
#   ns(Print...Play, df = 1)                1165.24     185.73    6.274 3.59e-10 ***
#   ns(Miniatures, df = 1)                   291.80     152.16    1.918 0.055158 .  
#   ns(City.Building, df = 1)              -1528.82     203.10   -7.528 5.37e-14 ***
#   ns(Wargame, df = 1)                     1510.97      95.36   15.844  < 2e-16 ***
#   ns(Adventure, df = 1)                    603.40     138.71    4.350 1.37e-05 ***
#   ns(Renaissance, df = 1)                -1752.38     280.31   -6.252 4.14e-10 ***
#   ns(Modern.Warfare, df = 1)               518.65     241.00    2.152 0.031401 *  
#   ns(Humor, df = 1)                        670.08     139.66    4.798 1.61e-06 ***
#   ns(Electronic, df = 1)                   508.47     320.69    1.586 0.112859    
#   ns(Word.Game, df = 1)                    521.48     214.10    2.436 0.014872 *  
#   ns(Aviation...Flight, df = 1)          -1109.74     276.06   -4.020 5.84e-05 ***
#   ns(Movies...TV...Radio.theme, df = 1)    312.62     146.34    2.136 0.032673 *  
#   ns(Party.Game, df = 1)                   598.88     120.45    4.972 6.67e-07 ***
#   ns(Memory, df = 1)                       683.84     201.16    3.399 0.000677 ***
#   ns(Puzzle, df = 1)                      -642.80     179.68   -3.577 0.000348 ***
#   ns(Real.time, df = 1)                   -638.72     172.80   -3.696 0.000219 ***
#   ns(Trivia, df = 1)                       796.16     200.82    3.965 7.38e-05 ***
#   ns(Industry...Manufacturing, df = 1)    -987.60     266.49   -3.706 0.000211 ***
#   ns(Trains, df = 1)                     -1211.54     262.86   -4.609 4.07e-06 ***
#   ns(Animals, df = 1)                     -495.47     132.38   -3.743 0.000182 ***
#   ns(X.Childrens.Game., df = 1)            428.26     129.24    3.314 0.000923 ***
#   ns(Transportation, df = 1)              -853.87     249.67   -3.420 0.000627 ***
#   ns(Prehistoric, df = 1)                 -939.60     357.00   -2.632 0.008497 ** 
#   ns(Sports, df = 1)                       414.40     205.27    2.019 0.043521 *  
#   ns(Post.Napoleonic, df = 1)              678.35     299.03    2.268 0.023311 *  
#   ns(Book, df = 1)                         774.75     348.20    2.225 0.026092 *  
#   ns(Music, df = 1)                        911.35     488.73    1.865 0.062233 .  
#   ns(Video.Game.Theme, df = 1)             702.67     243.27    2.888 0.003875 ** 
#   ns(Mature...Adult, df = 1)              1050.24     390.89    2.687 0.007220 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3588 on 21582 degrees of freedom
# Multiple R-squared:  0.6776,	Adjusted R-squared:  0.6768 
# F-statistic: 944.8 on 48 and 21582 DF,  p-value: < 2.2e-16

#let's do perm sel one more time
R2adj=R_extr(model_ns2.1)
R2adj.perm=perm_sel(model_ns2.1)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#all true again
#let's delete them:
to_delete_ind=cbind(to_delete_ind,which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE])))
names="Rank~"
for(i in colnames(mydata[,c(3:8)[!c(3:8)%in%to_delete_ind[which(to_delete_ind<=8)]]])){
  names=paste(names, "+ ","ns(",i,",df=2)")
}

for(i in colnames(mydata[,c(-seq(1:8),-to_delete_ind[which(to_delete_ind>8)])])){
  names=paste(names, "+ ","ns(",i,",df=1)")
}

formula=as.formula(names)
model_ns2.2<-lm(formula, data = mydata)
summary(model_ns2.2)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -38810  -2815    337   2717  23597 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                            49121.94     419.61  117.065  < 2e-16 ***
#   ns(Avg_ratings, df = 2)1              -70078.06     753.65  -92.985  < 2e-16 ***
#   ns(Avg_ratings, df = 2)2              -26252.76     193.77 -135.483  < 2e-16 ***
#   ns(Users_rated, df = 2)1               56311.02    3770.24   14.936  < 2e-16 ***
#   ns(Users_rated, df = 2)2               25891.44    7221.30    3.585 0.000337 ***
#   ns(Min_age, df = 2)1                   -2445.54     207.24  -11.801  < 2e-16 ***
#   ns(Min_age, df = 2)2                    1125.42     391.63    2.874 0.004061 ** 
#   ns(Owned, df = 2)1                    -80605.63    4051.69  -19.894  < 2e-16 ***
#   ns(Owned, df = 2)2                     16018.39    7857.85    2.039 0.041510 *  
#   ns(Negotiation, df = 1)                  494.78     184.61    2.680 0.007364 ** 
#   ns(Political, df = 1)                   -635.27     211.69   -3.001 0.002694 ** 
#   ns(Abstract.Strategy, df = 1)            498.39     123.53    4.035 5.49e-05 ***
#   ns(Medieval, df = 1)                    -465.66     147.10   -3.166 0.001549 ** 
#   ns(Ancient, df = 1)                    -1071.74     174.83   -6.130 8.94e-10 ***
#   ns(Territory.Building, df = 1)          -802.86     206.54   -3.887 0.000102 ***
#   ns(Civilization, df = 1)                -978.31     253.88   -3.853 0.000117 ***
#   ns(Nautical, df = 1)                    -772.76     183.57   -4.210 2.57e-05 ***
#   ns(Farming, df = 1)                     -903.43     294.53   -3.067 0.002162 ** 
#   ns(Bluffing, df = 1)                    -359.35     136.86   -2.626 0.008653 ** 
#   ns(Science.Fiction, df = 1)             -237.97     119.82   -1.986 0.047039 *  
#   ns(Print...Play, df = 1)                1163.76     185.74    6.266 3.78e-10 ***
#   ns(Miniatures, df = 1)                   292.26     152.17    1.921 0.054787 .  
#   ns(City.Building, df = 1)              -1527.83     203.10   -7.522 5.59e-14 ***
#   ns(Wargame, df = 1)                     1510.09      95.37   15.835  < 2e-16 ***
#   ns(Adventure, df = 1)                    607.19     138.69    4.378 1.20e-05 ***
#   ns(Renaissance, df = 1)                -1753.93     280.32   -6.257 4.00e-10 ***
#   ns(Modern.Warfare, df = 1)               521.41     241.00    2.164 0.030513 *  
#   ns(Humor, df = 1)                        667.26     139.65    4.778 1.78e-06 ***
#   ns(Word.Game, df = 1)                    521.37     214.11    2.435 0.014896 *  
#   ns(Aviation...Flight, df = 1)          -1106.66     276.06   -4.009 6.12e-05 ***
#   ns(Movies...TV...Radio.theme, df = 1)    321.50     146.24    2.198 0.027931 *  
#   ns(Party.Game, df = 1)                   603.76     120.41    5.014 5.37e-07 ***
#   ns(Memory, df = 1)                       690.05     201.13    3.431 0.000603 ***
#   ns(Puzzle, df = 1)                      -642.18     179.69   -3.574 0.000353 ***
#   ns(Real.time, df = 1)                   -626.22     172.63   -3.628 0.000287 ***
#   ns(Trivia, df = 1)                       810.76     200.62    4.041 5.33e-05 ***
#   ns(Industry...Manufacturing, df = 1)    -989.08     266.49   -3.711 0.000207 ***
#   ns(Trains, df = 1)                     -1212.55     262.87   -4.613 4.00e-06 ***
#   ns(Animals, df = 1)                     -496.35     132.38   -3.749 0.000178 ***
#   ns(X.Childrens.Game., df = 1)            439.17     129.07    3.403 0.000669 ***
#   ns(Transportation, df = 1)              -856.35     249.68   -3.430 0.000605 ***
#   ns(Prehistoric, df = 1)                 -944.36     357.00   -2.645 0.008169 ** 
#   ns(Sports, df = 1)                       414.78     205.28    2.021 0.043336 *  
#   ns(Post.Napoleonic, df = 1)              677.78     299.05    2.266 0.023431 *  
#   ns(Book, df = 1)                         773.36     348.21    2.221 0.026365 *  
#   ns(Music, df = 1)                        937.27     488.47    1.919 0.055024 .  
#   ns(Video.Game.Theme, df = 1)             704.47     243.27    2.896 0.003785 ** 
#   ns(Mature...Adult, df = 1)              1043.45     390.88    2.669 0.007603 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3588 on 21583 degrees of freedom
# Multiple R-squared:  0.6775,	Adjusted R-squared:  0.6768 
# F-statistic: 964.8 on 47 and 21583 DF,  p-value: < 2.2e-16


#3)semiparametric models--------------------------------------------------------
#3.1) ns for numeric-----------
#(degree=2, maybe the previous nonpar models with degree 3 overfit data being R^2 adj so high)

names="Rank~"
for(i in colnames(mydata[,3:8])){
  names=paste(names, "+ ","ns(",i,",df=2)")
}

for(i in colnames(mydata[,9:ncol(mydata)])){
  names=paste(names, "+ ",i)
}

formula=as.formula(names)


model_sp1<-lm(formula, data = mydata)
model_sp1$type="lm"
summary(model_sp1)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -38818  -2819    333   2709  23573 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                49147.40     421.85  116.504  < 2e-16 ***
#   ns(Avg_ratings, df = 2)1  -70087.29     754.84  -92.851  < 2e-16 ***
#   ns(Avg_ratings, df = 2)2  -26190.44     196.71 -133.144  < 2e-16 ***
#   ns(Users_rated, df = 2)1   55889.80    3776.99   14.797  < 2e-16 ***
#   ns(Users_rated, df = 2)2   25791.33    7224.39    3.570 0.000358 ***
#   ns(Max_players, df = 2)1    -920.00    1453.66   -0.633 0.526815    
#   ns(Max_players, df = 2)2   -1834.92    2120.16   -0.865 0.386794    
#   ns(Playing_time, df = 2)1  -3021.56    2539.56   -1.190 0.234139    
#   ns(Playing_time, df = 2)2   2169.87    3682.67    0.589 0.555726    
#   ns(Min_age, df = 2)1       -2395.32     208.26  -11.501  < 2e-16 ***
#   ns(Min_age, df = 2)2        1183.29     393.05    3.011 0.002611 ** 
#   ns(Owned, df = 2)1        -80155.44    4059.97  -19.743  < 2e-16 ***
#   ns(Owned, df = 2)2         16107.01    7862.95    2.048 0.040526 *  
#   Negotiation                  394.35     148.31    2.659 0.007843 ** 
#   Political                   -513.07     169.94   -3.019 0.002539 ** 
#   Fantasy                     -130.25      82.68   -1.575 0.115185    
#   Abstract.Strategy            378.44     100.00    3.784 0.000155 ***
#   Medieval                    -367.69     118.37   -3.106 0.001897 ** 
#   Ancient                     -871.07     140.58   -6.196 5.89e-10 ***
#   Territory.Building          -636.36     165.74   -3.840 0.000124 ***
#   Civilization                -767.26     204.08   -3.760 0.000171 ***
#   Nautical                    -629.14     147.60   -4.262 2.03e-05 ***
#   Exploration                 -197.11     134.21   -1.469 0.141934    
#   Farming                     -733.85     236.36   -3.105 0.001906 ** 
#   Bluffing                    -265.57     114.12   -2.327 0.019967 *  
#   Science.Fiction             -218.15      97.41   -2.240 0.025129 *  
#   Collectible.Components        52.18     195.22    0.267 0.789256    
#   Dice                          25.77      88.90    0.290 0.771898    
#   Fighting                     146.57     100.38    1.460 0.144273    
#   Print...Play                 934.80     149.26    6.263 3.85e-10 ***
#   Miniatures                   212.78     125.47    1.696 0.089935 .  
#   City.Building              -1224.58     163.06   -7.510 6.15e-14 ***
#   Wargame                     1205.02      81.70   14.750  < 2e-16 ***
#   Adventure                    565.86     121.30    4.665 3.10e-06 ***
#   Renaissance                -1409.21     224.99   -6.263 3.84e-10 ***
#   Modern.Warfare               451.39     194.82    2.317 0.020515 *  
#   Humor                        531.83     112.32    4.735 2.21e-06 ***
#   Electronic                   425.08     257.33    1.652 0.098574 .  
#   Deduction                   -103.58     116.96   -0.886 0.375818    
#   Word.Game                    403.25     171.91    2.346 0.018998 *  
#   Aviation...Flight           -895.98     221.94   -4.037 5.43e-05 ***
#   Movies...TV...Radio.theme    241.01     117.72    2.047 0.040639 *  
#   Party.Game                   477.60      98.95    4.827 1.40e-06 ***
#   Memory                       573.74     162.30    3.535 0.000409 ***
#   Puzzle                      -500.88     144.71   -3.461 0.000539 ***
#   Real.time                   -516.69     138.68   -3.726 0.000195 ***
#   Trivia                       628.85     161.89    3.884 0.000103 ***
#   Industry...Manufacturing    -799.42     213.99   -3.736 0.000188 ***
#   Trains                      -988.12     211.38   -4.675 2.96e-06 ***
#   Animals                     -407.78     106.47   -3.830 0.000129 ***
#   X.Childrens.Game.            342.96     103.77    3.305 0.000952 ***
#   Transportation              -676.10     200.39   -3.374 0.000743 ***
#   Prehistoric                 -754.22     286.47   -2.633 0.008474 ** 
#   Sports                       305.71     165.02    1.853 0.063962 .  
#   Game.System                  700.67     669.30    1.047 0.295170    
#   Medical                     -290.76     387.36   -0.751 0.452894    
#   Napoleonic                   236.52     212.20    1.115 0.265023    
#   Post.Napoleonic              543.01     240.02    2.262 0.023684 *  
#   Book                         620.01     279.61    2.217 0.026604 *  
#   Music                        716.77     391.95    1.829 0.067452 .  
#   Arabian                     -527.26     355.16   -1.485 0.137671    
#   Video.Game.Theme             546.70     195.92    2.790 0.005268 ** 
#   Mature...Adult               815.77     313.82    2.599 0.009344 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3588 on 21568 degrees of freedom
# Multiple R-squared:  0.6778,	Adjusted R-squared:  0.6769 
# F-statistic: 731.8 on 62 and 21568 DF,  p-value: < 2.2e-16



#with natural splines of deg 3:
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -16572.9  -1454.5     22.8   1251.0  16641.7 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                23567.02     386.64  60.953  < 2e-16 ***
#   ns(Avg_ratings, df = 3)1  -18929.32     190.99 -99.114  < 2e-16 ***
#   ns(Avg_ratings, df = 3)2   -4500.18     782.67  -5.750 9.05e-09 ***
#   ns(Avg_ratings, df = 3)3   -8658.69     266.55 -32.485  < 2e-16 ***
#   ns(Users_rated, df = 3)1    1545.25    1529.16   1.011 0.312256    
#   ns(Users_rated, df = 3)2    4710.51    2181.92   2.159 0.030870 *  
#   ns(Users_rated, df = 3)3   23538.70    4467.57   5.269 1.39e-07 ***
#   ns(Max_players, df = 3)1      34.38     675.68   0.051 0.959421    
#   ns(Max_players, df = 3)2    -687.97     718.93  -0.957 0.338604    
#   ns(Max_players, df = 3)3    -486.34    1314.92  -0.370 0.711487    
#   ns(Playing_time, df = 3)1  -1087.31    1283.56  -0.847 0.396950    
#   ns(Playing_time, df = 3)2   1123.88    1195.45   0.940 0.347163    
#   ns(Playing_time, df = 3)3   2326.13    2249.07   1.034 0.301025    
#   ns(Min_age, df = 3)1        -435.62     115.40  -3.775 0.000161 ***
#   ns(Min_age, df = 3)2        1095.84     304.19   3.602 0.000316 ***
#   ns(Min_age, df = 3)3        2444.16     461.83   5.292 1.22e-07 ***
#   ns(Owned, df = 3)1        -25089.96    1576.39 -15.916  < 2e-16 ***
#   ns(Owned, df = 3)2        -21747.30    2379.58  -9.139  < 2e-16 ***
#   ns(Owned, df = 3)3        -15078.70    4875.84  -3.093 0.001987 ** 
#   Negotiation                  292.06      94.60   3.087 0.002023 ** 
#   Political                   -176.14     108.29  -1.627 0.103835    
#   Fantasy                       94.55      52.73   1.793 0.073009 .  
#   Abstract.Strategy             13.40      65.00   0.206 0.836673    
#   Medieval                    -197.17      75.37  -2.616 0.008907 ** 
#   Ancient                     -305.01      89.53  -3.407 0.000659 ***
#   Territory.Building          -109.82     105.55  -1.040 0.298126    
#   Civilization                -525.61     129.87  -4.047 5.20e-05 ***
#   Nautical                    -130.05      93.95  -1.384 0.166293    
#   Exploration                  -58.06      85.44  -0.680 0.496793    
#   Farming                     -272.51     150.37  -1.812 0.069969 .  
#   Bluffing                    -175.87      72.68  -2.420 0.015546 *  
#   Science.Fiction               58.53      62.15   0.942 0.346294    
#   Collectible.Components       227.11     125.01   1.817 0.069272 .  
#   Dice                         121.52      56.65   2.145 0.031955 *  
#   Fighting                      37.72      63.97   0.590 0.555409    
#   Print...Play                 394.80      96.52   4.090 4.32e-05 ***
#   Miniatures                    36.18      80.10   0.452 0.651455    
#   City.Building               -467.54     103.89  -4.500 6.82e-06 ***
#   Wargame                      558.42      58.38   9.566  < 2e-16 ***
#   Adventure                    210.93      77.27   2.730 0.006342 ** 
#   Renaissance                 -615.05     143.27  -4.293 1.77e-05 ***
#   Modern.Warfare               392.77     123.98   3.168 0.001538 ** 
#   Humor                        283.19      71.55   3.958 7.59e-05 ***
#   Electronic                   404.39     163.78   2.469 0.013553 *  
#   Deduction                     75.73      74.43   1.018 0.308900    
#   Word.Game                     67.98     109.36   0.622 0.534209    
#   Aviation...Flight           -116.68     141.32  -0.826 0.409004    
#   Movies...TV...Radio.theme    221.99      75.07   2.957 0.003109 ** 
#   Party.Game                   284.21      63.09   4.505 6.67e-06 ***
#   Memory                        27.66     103.40   0.267 0.789094    
#   Puzzle                      -151.56      92.10  -1.646 0.099839 .  
#   Real.time                   -288.51      88.74  -3.251 0.001152 ** 
#   Trivia                        44.29     103.36   0.429 0.668264    
#   Industry...Manufacturing    -346.50     136.33  -2.542 0.011043 *  
#   Trains                      -272.90     134.73  -2.026 0.042824 *  
#   Animals                      -72.43      67.88  -1.067 0.285956    
#   X.Childrens.Game.             28.79      67.79   0.425 0.671072    
#   Transportation              -368.58     127.52  -2.890 0.003852 ** 
#   Prehistoric                 -271.19     182.29  -1.488 0.136846    
#   Sports                        14.53     105.62   0.138 0.890602    
#   Game.System                  434.92     425.80   1.021 0.307068    
#   Medical                     -263.38     246.40  -1.069 0.285135    
#   Napoleonic                   132.35     134.98   0.980 0.326860    
#   Post.Napoleonic               68.24     152.81   0.447 0.655183    
#   Book                         -59.01     178.26  -0.331 0.740620    
#   Music                        569.83     249.37   2.285 0.022318 *  
#   Arabian                     -310.93     225.91  -1.376 0.168720    
#   Video.Game.Theme             528.90     124.65   4.243 2.21e-05 ***
#   Mature...Adult               160.45     208.78   0.769 0.442182    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2282 on 21562 degrees of freedom
# Multiple R-squared:  0.8697,	Adjusted R-squared:  0.8693 
# F-statistic:  2117 on 68 and 21562 DF,  p-value: < 2.2e-16


R2adj=R_extr(model_sp1)
R2adj.perm=perm_sel(model_sp1)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#all true again
#let's delete them:
to_delete_ind=which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE]))
names="Rank~"
for(i in colnames(mydata[,c(3:8)[!c(3:8)%in%to_delete_ind[which(to_delete_ind<=8)]]])){
  names=paste(names, "+ ","ns(",i,",df=2)")
}

for(i in colnames(mydata[,c(-seq(1:8),-to_delete_ind[which(to_delete_ind>8)])])){
  names=paste(names, "+ ",i)
}

formula=as.formula(names)
model_sp1.1<-lm(formula, data = mydata)
model_sp1.1$type="lm"
summary(model_sp1.1)
# Residuals:
#   Min     1Q Median     3Q    Max 
# -38813  -2815    337   2714  23609 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                49108.91     419.68  117.016  < 2e-16 ***
#   ns(Avg_ratings, df = 2)1  -70060.05     753.70  -92.954  < 2e-16 ***
#   ns(Avg_ratings, df = 2)2  -26242.27     193.88 -135.355  < 2e-16 ***
#   ns(Users_rated, df = 2)1   56286.61    3770.14   14.930  < 2e-16 ***
#   ns(Users_rated, df = 2)2   25862.10    7221.07    3.581 0.000342 ***
#   ns(Min_age, df = 2)1       -2442.04     207.24  -11.784  < 2e-16 ***
#   ns(Min_age, df = 2)2        1122.74     391.62    2.867 0.004149 ** 
#   ns(Owned, df = 2)1        -80582.64    4051.58  -19.889  < 2e-16 ***
#   ns(Owned, df = 2)2         16058.04    7857.62    2.044 0.041003 *  
#   Negotiation                  391.68     148.04    2.646 0.008158 ** 
#   Political                   -507.53     169.73   -2.990 0.002790 ** 
#   Abstract.Strategy            399.60      99.04    4.035 5.49e-05 ***
#   Medieval                    -371.64     117.94   -3.151 0.001629 ** 
#   Ancient                     -857.99     140.18   -6.121 9.47e-10 ***
#   Territory.Building          -642.65     165.59   -3.881 0.000104 ***
#   Civilization                -783.87     203.55   -3.851 0.000118 ***
#   Nautical                    -621.08     147.18   -4.220 2.46e-05 ***
#   Farming                     -723.78     236.14   -3.065 0.002179 ** 
#   Bluffing                    -284.93     109.75   -2.596 0.009431 ** 
#   Science.Fiction             -189.69      96.07   -1.974 0.048339 *  
#   Print...Play                 934.27     148.92    6.274 3.59e-10 ***
#   Miniatures                   233.96     122.00    1.918 0.055158 .  
# City.Building              -1225.78     162.84   -7.528 5.37e-14 ***
#   Wargame                     1211.47      76.46   15.844  < 2e-16 ***
#   Adventure                    483.80     111.21    4.350 1.37e-05 ***
#   Renaissance                -1405.03     224.75   -6.252 4.14e-10 ***
#   Modern.Warfare               415.85     193.23    2.152 0.031401 *  
#   Humor                        537.26     111.98    4.798 1.61e-06 ***
#   Electronic                   407.68     257.12    1.586 0.112859    
# Word.Game                    418.11     171.66    2.436 0.014872 *  
#   Aviation...Flight           -889.77     221.34   -4.020 5.84e-05 ***
#   Movies...TV...Radio.theme    250.66     117.34    2.136 0.032673 *  
#   Party.Game                   480.18      96.57    4.972 6.67e-07 ***
#   Memory                       548.29     161.29    3.399 0.000677 ***
#   Puzzle                      -515.38     144.07   -3.577 0.000348 ***
#   Real.time                   -512.11     138.55   -3.696 0.000219 ***
#   Trivia                       638.35     161.02    3.965 7.38e-05 ***
#   Industry...Manufacturing    -791.84     213.67   -3.706 0.000211 ***
#   Trains                      -971.39     210.76   -4.609 4.07e-06 ***
#   Animals                     -397.26     106.14   -3.743 0.000182 ***
#   X.Childrens.Game.            343.37     103.63    3.314 0.000923 ***
#   Transportation              -684.62     200.18   -3.420 0.000627 ***
#   Prehistoric                 -753.35     286.24   -2.632 0.008497 ** 
#   Sports                       332.26     164.58    2.019 0.043521 *  
#   Post.Napoleonic              543.89     239.76    2.268 0.023311 *  
#   Book                         621.18     279.18    2.225 0.026092 *  
#   Music                        730.70     391.85    1.865 0.062233 .  
#   Video.Game.Theme             563.39     195.05    2.888 0.003875 ** 
#   Mature...Adult               842.07     313.41    2.687 0.007220 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3588 on 21582 degrees of freedom
# Multiple R-squared:  0.6776,	Adjusted R-squared:  0.6768 
# F-statistic: 944.8 on 48 and 21582 DF,  p-value: < 2.2e-16



#3.2) re for numeric-------------------

names="Rank~"
for(i in colnames(mydata[,3:8])){
  names=paste(names, "+ ","s(",i,",bs='re' )")
}

for(i in colnames(mydata[,9:ncol(mydata)])){
  names=paste(names, "+ ",i)
}

formula=as.formula(names)


model_sp2<-gam(formula,data = mydata)
model_sp2$type="gam"
summary(model_sp2)

# 
# Parametric coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               42522.643    214.601 198.148  < 2e-16 ***
#   Negotiation                 504.642    160.441   3.145 0.001661 ** 
#   Political                  -485.776    183.891  -2.642 0.008256 ** 
#   Fantasy                    -161.477     89.361  -1.807 0.070773 .  
#   Abstract.Strategy           251.393    107.904   2.330 0.019827 *  
#   Medieval                   -532.745    128.143  -4.157 3.23e-05 ***
#   Ancient                    -935.362    152.195  -6.146 8.09e-10 ***
#   Territory.Building         -691.023    179.496  -3.850 0.000119 ***
#   Civilization               -647.477    220.779  -2.933 0.003364 ** 
#   Nautical                   -921.770    159.749  -5.770 8.03e-09 ***
#   Exploration                -333.168    145.278  -2.293 0.021840 *  
#   Farming                   -1058.704    255.880  -4.138 3.52e-05 ***
#   Bluffing                   -550.817    123.409  -4.463 8.11e-06 ***
#   Science.Fiction            -317.943    105.391  -3.017 0.002558 ** 
#   Collectible.Components       78.537    211.262   0.372 0.710082    
#   Dice                         -3.051     96.216  -0.032 0.974701    
#   Fighting                     91.570    108.668   0.843 0.399427    
#   Print...Play               1092.981    161.545   6.766 1.36e-11 ***
#   Miniatures                  638.463    134.876   4.734 2.22e-06 ***
#   City.Building             -1508.219    176.526  -8.544  < 2e-16 ***
#   Wargame                    1643.779     85.822  19.153  < 2e-16 ***
#   Adventure                   569.174    131.293   4.335 1.46e-05 ***
#   Renaissance               -1405.442    243.458  -5.773 7.90e-09 ***
#   Modern.Warfare              497.985    210.939   2.361 0.018245 *  
#   Humor                       485.693    121.544   3.996 6.46e-05 ***
#   Electronic                  381.815    278.685   1.370 0.170682    
#   Deduction                  -209.232    126.641  -1.652 0.098516 .  
#   Word.Game                   316.796    186.106   1.702 0.088727 .  
#   Aviation...Flight          -899.808    240.152  -3.747 0.000180 ***
#   Movies...TV...Radio.theme   355.480    127.209   2.794 0.005203 ** 
#   Party.Game                  505.238    105.161   4.804 1.56e-06 ***
#   Memory                      629.214    175.761   3.580 0.000344 ***
#   Puzzle                     -524.331    156.665  -3.347 0.000819 ***
#   Real.time                  -861.542    149.906  -5.747 9.20e-09 ***
#   Trivia                      927.331    174.882   5.303 1.15e-07 ***
#   Industry...Manufacturing   -824.227    231.662  -3.558 0.000375 ***
#   Trains                     -740.309    228.753  -3.236 0.001213 ** 
#   Animals                    -623.740    115.139  -5.417 6.12e-08 ***
#   X.Childrens.Game.           482.299    112.202   4.298 1.73e-05 ***
#   Transportation             -731.837    216.949  -3.373 0.000744 ***
#   Prehistoric                -943.712    310.199  -3.042 0.002351 ** 
#   Sports                      626.227    178.550   3.507 0.000454 ***
#   Game.System                 843.784    724.855   1.164 0.244408    
#   Medical                     463.601    419.188   1.106 0.268760    
#   Napoleonic                  492.646    229.671   2.145 0.031963 *  
#   Post.Napoleonic             450.605    259.867   1.734 0.082936 .  
#   Book                       1227.646    302.410   4.060 4.93e-05 ***
#   Music                       939.044    424.444   2.212 0.026949 *  
#   Arabian                    -772.075    384.586  -2.008 0.044704 *  
#   Video.Game.Theme            623.308    212.121   2.938 0.003302 ** 
#   Mature...Adult             1946.937    323.603   6.016 1.81e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#                       edf     Ref.df     F   p-value    
#   s(Avg_ratings)  1.000e+00      1 3.868e+04  <2e-16 ***
#   s(Users_rated)  9.967e-01      1 1.028e+04  <2e-16 ***
#   s(Max_players)  3.885e-09      1 0.000e+00   0.423    
#   s(Playing_time) 4.123e-01      1 7.020e-01   0.192    
#   s(Min_age)      9.951e-01      1 2.063e+02  <2e-16 ***
#   s(Owned)        1.000e+00      1 6.412e+05  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.621   Deviance explained = 62.2%
# GCV = 1.5141e+07  Scale est. = 1.5102e+07  n = 21631


R2adj=R_extr(model_sp2)
R2adj.perm=perm_sel(model_sp2)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#if the difference in terms of R^2adj is not so big we can delete those variables
#all true
#let's delete them:
to_delete_ind=which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE]))
names="Rank~"
for(i in colnames(mydata[,c(3:8)[!c(3:8)%in%to_delete_ind[which(to_delete_ind<=8)]]])){
  names=paste(names, "+ ","s(",i,",bs='re' )")
}

for(i in colnames(mydata[,c(-seq(1:8),-to_delete_ind[which(to_delete_ind>8)])])){
  names=paste(names, "+ ",i)
}


formula=as.formula(names)
model_sp2.1=gam(formula,data = mydata)
model_sp2.1$type="gam"
summary(model_sp2.1)

# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               42520.33     213.43 199.219  < 2e-16 ***
#   Negotiation                 514.40     160.34   3.208 0.001338 ** 
#   Political                  -469.49     183.73  -2.555 0.010615 *  
#   Abstract.Strategy           279.61     107.05   2.612 0.009006 ** 
#   Medieval                   -553.79     127.73  -4.336 1.46e-05 ***
#   Ancient                    -930.93     151.81  -6.132 8.81e-10 ***
#   Territory.Building         -700.14     179.39  -3.903 9.53e-05 ***
#   Civilization               -670.91     220.27  -3.046 0.002323 ** 
#   Nautical                   -918.28     159.33  -5.763 8.37e-09 ***
#   Farming                   -1043.79     255.74  -4.081 4.49e-05 ***
#   Bluffing                   -603.25     118.70  -5.082 3.76e-07 ***
#   Science.Fiction            -301.82     104.04  -2.901 0.003722 ** 
#   Print...Play               1078.04     161.28   6.684 2.38e-11 ***
#   Miniatures                  621.36     131.35   4.730 2.25e-06 ***
#   City.Building             -1502.23     176.34  -8.519  < 2e-16 ***
#   Wargame                    1720.86      81.71  21.060  < 2e-16 ***
#   Adventure                   426.09     120.33   3.541 0.000399 ***
#   Renaissance               -1401.58     243.26  -5.762 8.44e-09 ***
#   Modern.Warfare              434.11     209.32   2.074 0.038100 *  
#   Humor                       499.12     121.22   4.118 3.84e-05 ***
#   Electronic                  358.49     278.55   1.287 0.198113    
#   Word.Game                   334.07     185.90   1.797 0.072341 .  
#   Aviation...Flight          -913.34     239.75  -3.809 0.000140 ***
#   Movies...TV...Radio.theme   364.06     126.84   2.870 0.004105 ** 
#   Party.Game                  506.97     104.33   4.859 1.19e-06 ***
#   Memory                      594.10     174.72   3.400 0.000674 ***
#   Puzzle                     -546.83     156.03  -3.505 0.000458 ***
#   Real.time                  -853.97     149.80  -5.701 1.21e-08 ***
#   Trivia                      956.38     174.19   5.491 4.05e-08 ***
#   Industry...Manufacturing   -798.84     231.39  -3.452 0.000557 ***
#   Trains                     -705.05     228.18  -3.090 0.002005 ** 
#   Animals                    -609.80     114.81  -5.312 1.10e-07 ***
#   X.Childrens.Game.           485.00     112.09   4.327 1.52e-05 ***
#   Transportation             -732.46     216.79  -3.379 0.000730 ***
#   Prehistoric                -949.81     310.05  -3.063 0.002191 ** 
#   Sports                      659.29     178.13   3.701 0.000215 ***
#   Post.Napoleonic             450.55     259.68   1.735 0.082752 .  
#   Book                       1221.00     302.25   4.040 5.37e-05 ***
#   Music                       952.69     424.47   2.244 0.024814 *  
#   Video.Game.Theme            633.12     211.22   2.997 0.002726 ** 
#   Mature...Adult             1972.35     323.49   6.097 1.10e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df        F p-value    
#   s(Avg_ratings) 1.0000      1  53824.3  <2e-16 ***
#   s(Users_rated) 0.9967      1 420656.6  <2e-16 ***
#   s(Min_age)     0.9948      1    247.6  <2e-16 ***
#   s(Owned)       1.0000      1 736393.6  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.621   Deviance explained = 62.1%
# GCV = 1.5143e+07  Scale est. = 1.5112e+07  n = 21631


#4)comparing models--------------------------------------
#do we really need a nonparametri contribution for the categories or we can use semiparametric models?
#let's compare models in 2.3 (model_ns2.2) and 3.1 (model_sp1.1)
#                 and in 1 (model_re1)  and 3.2  (model_sp2.1)

hist(model_ns2.2$residuals)
qqnorm(model_ns2.2$residuals)
hist(model_sp1.1$residuals)
qqnorm(model_sp1.1$residuals)
# normal-ish ==> i can use parametric test(can I?):
anova(model_sp1.1,model_ns2.2, test = "F")
#pvalue=0.1129==>i modelli in realtà sono simili( up to alpha=0.1) e potrei usare quello meno complesso


hist(model_re1$residuals) #normal-ish
qqnorm(model_re1$residuals) #eehmm
hist(model_sp2.1$residuals)
qqnorm(model_sp2.1$residuals)
# normal-ish ==> i can use parametric test(can I?):
anova(model_sp2.1,model_re1, test = "F")
#pvalue=0.1704==> i modelli in realtà sono simili e potrei usare quello meno complesso



#test non parametrici
#coming soon

#5) plots------------------------------------
# dev.new()
# gam::plot.Gam(model_sp1.1,se=TRUE)

#altri
#coming soon













