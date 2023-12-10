setwd("C:/Users/Silvia/Desktop/23-24/NONPAR/project")
# avg=read.csv("data/Average.csv", head=T) #avg rating per day
# users_rated=read.csv("data/usersRated.csv", head=T) #number of users that rated a game per day
# rank=read.csv("data/Rank.csv", head=T) #rank position per day
# details=read.csv("details.csv", head=T) 
# ratings=read.csv("ratings.csv", head=T)
LMdataset=as.data.frame(read.csv("C:/Users/Silvia/Desktop/23-24/NONPAR/project/data/LMdataset.csv",head=TRUE))
LMdata=as.data.frame(read.csv("C:/Users/Silvia/Desktop/23-24/NONPAR/project/data/LMdata.csv",head=TRUE))
#the one with weights


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

mydata=LMdata[,c(-3,-5,-8,-11,-12,-14,-15,-16,-17,-18,-19,-20,-21,-22,-98)]
colSums(is.na(mydata))
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
    return(c(summary(model)$p.table[,"Pr(>|t|)"],summary(model)$s.table[,4]) )
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



#variable selection through parametric lm---------------------------------

#modello 1 :rank~.
model_lm=lm(Rank ~., data=mydata)
summary(model_lm)


# Residuals:
#   Min     1Q Median     3Q    Max 
# -16634  -4042    243   3946  36671 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                2.192e+04  3.643e+02  60.165  < 2e-16 ***
#   ID                        -1.257e-02  3.647e-04 -34.478  < 2e-16 ***
#   Users_rated               -4.361e-01  9.667e-03 -45.113  < 2e-16 ***
#   Year                      -1.047e+00  1.691e-01  -6.194 5.97e-10 ***
#   weights                   -3.048e+03  6.359e+01 -47.936  < 2e-16 ***
#   Max_players               -2.620e+00  2.356e+00  -1.112 0.266068    
#   Playing_time               1.135e-01  6.689e-02   1.697 0.089629 .  
#   Min_age                   -9.808e+01  1.107e+01  -8.857  < 2e-16 ***
#   Economic                   6.376e+02  1.585e+02   4.022 5.80e-05 ***
#   Negotiation                1.323e+03  2.182e+02   6.065 1.34e-09 ***
#   Political                 -2.468e+02  2.446e+02  -1.009 0.313095    
#   Card.Game                 -3.211e+02  8.844e+01  -3.630 0.000284 ***
#   Fantasy                   -3.951e+02  1.214e+02  -3.255 0.001137 ** 
#   Abstract.Strategy         -2.422e+01  1.479e+02  -0.164 0.869925    
#   Medieval                  -4.695e+02  1.702e+02  -2.758 0.005813 ** 
#   Ancient                   -9.993e+02  2.042e+02  -4.894 9.96e-07 ***
#   Territory.Building        -4.264e+02  2.375e+02  -1.796 0.072553 .  
#   Civilization              -1.694e+02  2.938e+02  -0.577 0.564117    
#   Nautical                  -1.011e+03  2.184e+02  -4.631 3.66e-06 ***
#   Exploration               -4.231e+02  1.936e+02  -2.185 0.028880 *  
#   Travel                    -1.220e+01  3.450e+02  -0.035 0.971796    
#   Farming                   -1.235e+03  3.389e+02  -3.645 0.000268 ***
#   Mythology                  1.569e+02  2.807e+02   0.559 0.576151    
#   Bluffing                  -9.580e+02  1.654e+02  -5.792 7.07e-09 ***
#   Science.Fiction           -8.859e+01  1.466e+02  -0.604 0.545582    
#   Collectible.Components     7.965e+02  2.865e+02   2.780 0.005437 ** 
#   Dice                      -4.406e+01  1.298e+02  -0.339 0.734312    
#   Fighting                  -1.946e+02  1.455e+02  -1.338 0.180885    
#   Print...Play              -1.650e+02  2.136e+02  -0.772 0.439851    
#   Maze                      -5.500e+02  3.784e+02  -1.454 0.146085    
#   Miniatures                -7.236e+02  1.807e+02  -4.005 6.23e-05 ***
#   Racing                    -1.864e+01  2.166e+02  -0.086 0.931421    
#   American.West             -2.837e+02  3.373e+02  -0.841 0.400273    
#   City.Building             -1.501e+03  2.353e+02  -6.379 1.82e-10 ***
#   Wargame                    1.028e+03  1.321e+02   7.783 7.42e-15 ***
#   Adventure                  6.501e+02  1.750e+02   3.715 0.000204 ***
#   Space.Exploration          2.633e+02  3.220e+02   0.818 0.413532    
#   Renaissance               -1.291e+03  3.235e+02  -3.991 6.61e-05 ***
#   Modern.Warfare             1.065e+03  2.792e+02   3.815 0.000137 ***
#   Humor                      6.746e+02  1.622e+02   4.159 3.21e-05 ***
#   Electronic                 9.989e+02  3.692e+02   2.705 0.006827 ** 
#   Horror                    -1.691e+02  2.147e+02  -0.788 0.430695    
#   Novel.based                2.897e+02  2.382e+02   1.216 0.223961    
#   Deduction                 -5.580e+02  1.821e+02  -3.065 0.002183 ** 
#   Word.Game                 -4.678e+02  2.485e+02  -1.882 0.059796 .  
#   Aviation...Flight         -1.397e+03  3.171e+02  -4.406 1.06e-05 ***
#   Movies...TV...Radio.theme  1.514e+03  1.693e+02   8.942  < 2e-16 ***
#   Party.Game                -7.815e+01  1.432e+02  -0.546 0.585282    
#   Memory                     6.945e+02  2.331e+02   2.980 0.002888 ** 
#   Puzzle                    -8.211e+02  2.084e+02  -3.940 8.17e-05 ***
#   Real.time                 -1.109e+03  2.001e+02  -5.543 3.00e-08 ***
#   Trivia                     1.931e+03  2.374e+02   8.133 4.40e-16 ***
#   Industry...Manufacturing  -5.907e+02  3.147e+02  -1.877 0.060505 .  
#   Age.of.Reason             -9.828e+02  4.059e+02  -2.422 0.015464 *  
#   Trains                    -1.244e+03  3.079e+02  -4.039 5.39e-05 ***
#   Animals                   -9.419e+02  1.539e+02  -6.121 9.43e-10 ***
#   X.Childrens.Game.          1.067e+03  1.542e+02   6.921 4.61e-12 ***
#   Pirates                    1.805e+02  2.942e+02   0.614 0.539541    
#   Murder.Mystery            -4.803e+02  3.074e+02  -1.563 0.118139    
#   Transportation            -4.319e+02  2.895e+02  -1.492 0.135737    
#   Prehistoric               -6.014e+02  4.104e+02  -1.466 0.142789    
#   Sports                    -6.556e+02  2.406e+02  -2.725 0.006433 ** 
#   Action...Dexterity        -8.161e+02  1.744e+02  -4.680 2.89e-06 ***
#   Game.System               -8.789e+02  9.560e+02  -0.919 0.357946    
#   Spies.Secret.Agents        6.903e+01  3.662e+02   0.188 0.850501    
#   Educational                2.696e+02  2.446e+02   1.102 0.270337    
#   Medical                    6.578e+02  5.532e+02   1.189 0.234430    
#   Mafia                     -2.695e+02  4.249e+02  -0.634 0.525862    
#   Zombies                    1.646e+02  3.763e+02   0.437 0.661785    
#   Comic.Book...Strip         7.569e+01  2.851e+02   0.265 0.790646    
#   Napoleonic                -3.366e+02  3.035e+02  -1.109 0.267444    
#   Post.Napoleonic            1.001e+02  3.430e+02   0.292 0.770427    
#   Math                       9.805e+02  4.368e+02   2.245 0.024803 *  
#   Book                      -4.723e+02  4.003e+02  -1.180 0.238014    
#   Music                      5.572e+02  5.593e+02   0.996 0.319150    
#   Environmental              2.549e+02  3.817e+02   0.668 0.504387    
#   Arabian                   -1.536e+02  5.069e+02  -0.303 0.761928    
#   Number                    -2.003e+02  3.796e+02  -0.528 0.597693    
#   Religious                  3.692e+02  4.634e+02   0.797 0.425634    
#   Pike.and.Shot              1.841e+02  6.233e+02   0.295 0.767641    
#   Video.Game.Theme           9.605e+02  2.803e+02   3.427 0.000613 ***
#   Mature...Adult             2.853e+03  4.272e+02   6.677 2.49e-11 ***
#   Expansion.for.Base.game   -2.464e+02  1.048e+03  -0.235 0.814187    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5117 on 21378 degrees of freedom
# Multiple R-squared:  0.348,	Adjusted R-squared:  0.3455 
# F-statistic: 139.1 on 82 and 21378 DF,  p-value: < 2.2e-16

#plot(model_lm)
model_lm$type="lm"
pv=pv_extr(model_lm)
to_delete=gsub("s\\((.*?)\\)", "\\1", names(pv[pv>0.1]))
to_delete_ind=which(names(mydata) %in% to_delete)


mydata=mydata[,-to_delete_ind]

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
# (Intercept)  21748.9      362.1   60.06   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#                                   edf       Ref.df     F   p-value    
#   s(Users_rated)               9.961e-01      1  11585.66  < 2e-16 ***
#   s(Year)                      9.836e-01      1    125.62  < 2e-16 ***
#   s(weights)                   9.958e-01      1 369236.41  < 2e-16 ***
#   s(Playing_time)              6.354e-01      1     20.97 0.088800 .  
#   s(Min_age)                   9.840e-01      1   6009.35  < 2e-16 ***
#   s(Economic)                  9.929e-01      1    865.70  < 2e-16 ***
#   s(Negotiation)               9.785e-01      1    604.93  < 2e-16 ***
#   s(Card.Game)                 9.783e-01      1   1494.43  < 2e-16 ***
#   s(Fantasy)                   9.645e-01      1    379.96  < 2e-16 ***
#   s(Medieval)                  8.069e-01      1     25.48 0.011910 *  
#   s(Ancient)                   9.474e-01      1    127.38 3.73e-06 ***
#   s(Territory.Building)        8.449e-01      1     39.48 0.003739 ** 
#   s(Nautical)                  9.281e-01      1     59.96 0.000141 ***
#   s(Exploration)               7.880e-01      1     39.92 0.026861 *  
#   s(Farming)                   9.349e-01      1     74.53 1.92e-05 ***
#   s(Bluffing)                  9.543e-01      1    294.45 3.41e-07 ***
#   s(Collectible.Components)    9.716e-01      1    138.88  < 2e-16 ***
#   s(Miniatures)                9.845e-01      1   1350.68  < 2e-16 ***
#   s(City.Building)             9.661e-01      1    533.81  < 2e-16 ***
#   s(Wargame)                   9.910e-01      1  34952.35  < 2e-16 ***
#   s(Adventure)                 9.652e-01      1    133.91 0.000448 ***
#   s(Renaissance)               9.139e-01      1     57.24 0.000147 ***
#   s(Modern.Warfare)            9.168e-01      1    285.68 0.000103 ***
#   s(Humor)                     9.477e-01      1    287.86 3.59e-06 ***
#   s(Electronic)                9.293e-01      1     63.70 4.65e-05 ***
#   s(Deduction)                 9.705e-01      1    360.18  < 2e-16 ***
#   s(Word.Game)                 2.915e-09      1      0.00 0.973433    
#   s(Aviation...Flight)         9.533e-01      1    100.68 2.90e-05 ***
#   s(Movies...TV...Radio.theme) 9.737e-01      1    583.42  < 2e-16 ***
#   s(Memory)                    9.445e-01      1    185.28 5.17e-06 ***
#   s(Puzzle)                    9.716e-01      1    209.78  < 2e-16 ***
#   s(Real.time)                 9.767e-01      1    531.14  < 2e-16 ***
#   s(Trivia)                    9.825e-01      1   1277.85  < 2e-16 ***
#   s(Industry...Manufacturing)  8.632e-01      1    104.83 0.005627 ** 
#   s(Age.of.Reason)             8.266e-01      1     31.71 0.017524 *  
#   s(Trains)                    9.501e-01      1    226.23 5.45e-05 ***
#   s(Animals)                   9.865e-01      1    970.88  < 2e-16 ***
#   s(X.Childrens.Game.)         9.788e-01      1   4865.63  < 2e-16 ***
#   s(Sports)                    2.869e-09      1      0.00 0.749439    
#   s(Action...Dexterity)        9.713e-01      1    563.44 4.81e-06 ***
#   s(Math)                      8.588e-01      1     10.91 0.008435 ** 
#   s(Video.Game.Theme)          7.210e-01      1      4.32 0.059211 .  
#   s(Mature...Adult)            9.664e-01      1    185.42  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.309   Deviance explained =   31%
# GCV = 2.7699e+07  Scale est. = 2.7648e+07  n = 21461



R2adj=R_extr(model_re)
R2adj.perm=perm_sel(model_re)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres) #if the difference in terms of R^2adj is not so big we can delete those variables
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
# (Intercept)  21748.8      362.1   60.06   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df         F  p-value    
#   s(Users_rated)               0.9961      1  11583.70  < 2e-16 ***
#   s(Year)                      0.9836      1    125.61  < 2e-16 ***
#   s(weights)                   0.9958      1 369220.46  < 2e-16 ***
#   s(Playing_time)              0.6354      1     20.97 0.088800 .  
#   s(Min_age)                   0.9840      1   6008.77  < 2e-16 ***
#   s(Economic)                  0.9929      1    865.65  < 2e-16 ***
#   s(Negotiation)               0.9785      1    604.67  < 2e-16 ***
#   s(Card.Game)                 0.9783      1   1494.30  < 2e-16 ***
#   s(Fantasy)                   0.9645      1    379.93  < 2e-16 ***
#   s(Medieval)                  0.8069      1     25.47 0.011910 *  
#   s(Ancient)                   0.9474      1    127.37 3.73e-06 ***
#   s(Territory.Building)        0.8449      1     39.47 0.003739 ** 
#   s(Nautical)                  0.9281      1     59.96 0.000141 ***
#   s(Exploration)               0.7880      1     39.92 0.026861 *  
#   s(Farming)                   0.9349      1     74.52 1.92e-05 ***
#   s(Bluffing)                  0.9543      1    294.44 3.41e-07 ***
#   s(Collectible.Components)    0.9716      1    138.87  < 2e-16 ***
#   s(Miniatures)                0.9845      1   1350.56  < 2e-16 ***
#   s(City.Building)             0.9661      1    533.71  < 2e-16 ***
#   s(Wargame)                   0.9910      1  34948.94  < 2e-16 ***
#   s(Adventure)                 0.9652      1    133.91 0.000448 ***
#   s(Renaissance)               0.9139      1     57.23 0.000147 ***
#   s(Modern.Warfare)            0.9168      1    285.67 0.000103 ***
#   s(Humor)                     0.9477      1    287.84 3.59e-06 ***
#   s(Electronic)                0.9293      1     63.69 4.65e-05 ***
#   s(Deduction)                 0.9705      1    360.16  < 2e-16 ***
#   s(Aviation...Flight)         0.9533      1    100.68 2.90e-05 ***
#   s(Movies...TV...Radio.theme) 0.9737      1    583.40  < 2e-16 ***
#   s(Memory)                    0.9445      1    185.27 5.17e-06 ***
#   s(Puzzle)                    0.9716      1    209.77  < 2e-16 ***
#   s(Real.time)                 0.9767      1    531.09  < 2e-16 ***
#   s(Trivia)                    0.9825      1   1277.74  < 2e-16 ***
#   s(Industry...Manufacturing)  0.8632      1    104.79 0.005627 ** 
#   s(Age.of.Reason)             0.8266      1     31.71 0.017524 *  
#   s(Trains)                    0.9501      1    226.16 5.45e-05 ***
#   s(Animals)                   0.9865      1    970.80  < 2e-16 ***
#   s(X.Childrens.Game.)         0.9788      1   4865.08  < 2e-16 ***
#   s(Action...Dexterity)        0.9713      1    563.38 4.81e-06 ***
#   s(Math)                      0.8588      1     10.90 0.008435 ** 
#   s(Video.Game.Theme)          0.7210      1      4.32 0.059212 .  
#   s(Mature...Adult)            0.9664      1    185.42  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.309   Deviance explained =   31%
# GCV = 2.7699e+07  Scale est. = 2.7648e+07  n = 21461


#dev.new()
#plot(model_re1)#?

#1.1) model assessment----------------------------------
gam.check(model_re1)
# 
# Method: GCV   Optimizer: magic
# Smoothing parameter selection converged after 21 iterations.
# The RMS GCV score gradient at convergence was 1.361296 .
# The Hessian was positive definite.
# Model rank =  42 / 42 
# 
# Basis dimension (k) checking results. Low p-value (k-index<1) may
# indicate that k is too low, especially if edf is close to k'.
# 
#                                 k'   edf k-index p-value    
#   s(Users_rated)               1.000 0.996    0.53  <2e-16 *** BAD
#   s(Year)                      1.000 0.984    0.93  <2e-16 *** BAD
#   s(weights)                   1.000 0.996    0.81  <2e-16 *** BAD



concurvity(model_re1, full = FALSE)

# $worst
#                                   para s(Users_rated)     s(Year)  s(weights)
# para                         1.000000000   5.336732e-02 0.988992161 0.860317142
# s(Users_rated)               0.053367318   1.000000e+00 0.053172348 0.060794484
# s(Year)                      0.988992161   5.317235e-02 1.000000000 0.853847956
# s(weights)                   0.860317142   6.079448e-02 0.853847956 1.000000000
# s(Playing_time)              0.027812914   1.173565e-03 0.027798928 0.050774638
# s(Min_age)                   0.874860726   5.800155e-02 0.871586018 0.825689489                  0.070688352    0.082784996     9.716504e-01   0.076267680

#==>interazioni:
#   s(Year)      para
#   s(weights)   para
#   s(Year)      s(weights)
#   s(Min_age)   para     
#   s(Min_age)   s(Year) 
#   s(Min_age)   s(weights) 

#let's try to deal with interactions and if the model is still lacking basis I'll change smoothing method

names="Rank~"
for(i in colnames(mydata[,c(-1,-2,-to_delete_ind)])){
  names=paste(names,"+ ", "s(",i,",bs='re' )")
}
names=paste(names,"-1+s(I(weights*Min_age))+s(I(Year*Min_age))+s(I(Year*weights))")
formula=as.formula(names)
model_re1.1=gam(formula,data = mydata)
model_re1.1$type="gam"
summary(model_re1.1)
# 
# Approximate significance of smooth terms:
#   edf Ref.df         F  p-value    
#   s(Users_rated)               9.977e-01  1.000 1.385e+04  < 2e-16 ***
#   s(Year)                      5.725e-01  1.000 5.432e+05  < 2e-16 ***
#   s(weights)                   3.914e-09  1.000 0.000e+00 1.09e-05 ***
#   s(Playing_time)              7.145e-04  1.000 0.000e+00 0.577557    
#   s(Min_age)                   7.711e-01  1.000 4.149e+06  < 2e-16 ***
#   s(Economic)                  9.627e-01  1.000 4.054e+02  < 2e-16 ***
#   s(Negotiation)               9.714e-01  1.000 3.714e+02  < 2e-16 ***
#   s(Card.Game)                 1.000e+00  1.000 1.899e+03  < 2e-16 ***
#   s(Fantasy)                   9.538e-01  1.000 2.411e+02 0.000348 ***
#   s(Medieval)                  5.858e-01  1.000 6.264e+00 0.119274    
#   s(Ancient)                   9.366e-01  1.000 5.643e+01 0.000314 ***
#   s(Territory.Building)        7.562e-01  1.000 1.233e+01 0.033821 *  
#   s(Nautical)                  9.336e-01  1.000 4.398e+01 0.000713 ***
#   s(Exploration)               6.360e-01  1.000 1.240e+01 0.105928    
#   s(Farming)                   9.438e-01  1.000 5.177e+01 2.32e-05 ***
#   s(Bluffing)                  9.632e-01  1.000 3.375e+02 2.24e-07 ***
#   s(Collectible.Components)    9.728e-01  1.000 1.444e+02  < 2e-16 ***
#   s(Miniatures)                9.991e-01  1.000 4.700e+02  < 2e-16 ***
#   s(City.Building)             9.693e-01  1.000 2.045e+02  < 2e-16 ***
#   s(Wargame)                   9.643e-01  1.000 6.569e+03  < 2e-16 ***
#   s(Adventure)                 9.663e-01  1.000 1.459e+02 9.24e-05 ***
#   s(Renaissance)               9.235e-01  1.000 3.264e+01 0.000198 ***
#   s(Modern.Warfare)            8.787e-01  1.000 3.901e+01 0.000400 ***
#   s(Humor)                     8.808e-01  1.000 6.802e+01 0.001015 ** 
#   s(Electronic)                9.149e-01  1.000 3.488e+01 8.49e-05 ***
#   s(Deduction)                 9.738e-01  1.000 3.313e+02  < 2e-16 ***
#   s(Aviation...Flight)         9.557e-01  1.000 3.503e+01 0.000154 ***
#   s(Movies...TV...Radio.theme) 9.680e-01  1.000 7.940e+02  < 2e-16 ***
#   s(Memory)                    9.098e-01  1.000 5.295e+01 0.000280 ***
#   s(Puzzle)                    9.688e-01  1.000 1.838e+02  < 2e-16 ***
#   s(Real.time)                 9.855e-01  1.000 3.718e+02  < 2e-16 ***
#   s(Trivia)                    9.722e-01  1.000 4.103e+02  < 2e-16 ***
#   s(Industry...Manufacturing)  9.024e-01  1.000 3.119e+01 0.002862 ** 
#   s(Age.of.Reason)             8.458e-01  1.000 8.777e+00 0.018173 *  
#   s(Trains)                    9.757e-01  1.000 8.354e+01 1.79e-06 ***
#   s(Animals)                   9.916e-01  1.000 4.663e+02  < 2e-16 ***
#   s(X.Childrens.Game.)         8.691e-01  1.000 1.882e+02 0.000519 ***
#   s(Action...Dexterity)        9.990e-01  1.000 3.737e+02  < 2e-16 ***
#   s(Math)                      8.566e-01  1.000 1.034e+01 0.007847 ** 
#   s(Video.Game.Theme)          7.847e-01  1.000 9.626e+00 0.026233 *  
#   s(Mature...Adult)            4.297e-08  1.000 0.000e+00 0.461275    
#   s(I(weights * Min_age))      8.232e+00  8.823 7.063e+00  < 2e-16 ***
#   s(I(Year * Min_age))         8.759e+00  8.977 5.398e+01  < 2e-16 ***
#   s(I(Year * weights))         8.644e+00  8.959 4.927e+01  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.325   Deviance explained =   83%
# GCV = 2.7083e+07  Scale est. = 2.7007e+07  n = 21461


gam.check(model_re1.1)
#                                   k'    edf     k-index p-value    
# s(Users_rated)               1.00e+00 9.96e-01    0.55  <2e-16 ***
# s(Year)                      1.00e+00 7.23e-01    0.94  <2e-16 ***
# s(weights)                   1.00e+00 9.60e-01    0.82  <2e-16 ***
# s(Playing_time)              1.00e+00 1.87e-02    0.97   0.025 * 
concurvity(model_re1.1, full = FALSE)$worst

#                               s(Users_rated)      s(Year)  s(weights) s(Playing_time)
# s(Users_rated)                 1.000000e+00 0.0531723478 0.060794484    1.173565e-03
# s(Year)                        5.317235e-02 1.0000000000 0.853847956    2.779893e-02
# s(weights)                     6.079448e-02 0.8538479562 1.000000000    5.077464e-02
# s(Playing_time)                1.173565e-03 0.0277989276 0.050774638    1.000000e+00
# s(Min_age)                     5.800155e-02 0.8715860176 0.825689489    2.925919e-02

#still very correlated:
# s(weights) vs  s(Year)
# s(weights) vs  s(Min_age)
# s(Min_age) vs  s(Year)



scatterplot(mydata$weights,mydata$Min_age)
scatterplot(mydata$Min_age[mydata$Year>-1000],mydata$Year[mydata$Year>-1000])
scatterplot(mydata$weights[mydata$Year>1800],mydata$Year[mydata$Year>1800])
#boh
#dev.new()
#plot(model_re1.1,select = 3, all.terms = TRUE)


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

## Residual standard error: 3869 on 21407 degrees of freedom
# Multiple R-squared:  0.6268,	Adjusted R-squared:  0.6259 
# F-statistic: 678.3 on 53 and 21407 DF,  p-value: < 2.2e-16

#i don't even try to do perm test since the degree of the splines of categ. variables is too high
#let's reduce the latter

#2.2) degree 3 for num and 1 for cat----------------------------------------------
names="Rank~"
for(i in colnames(mydata[,3:7])){
  names=paste(names, "+ ","ns(",i,",df=3)")
}

for(i in colnames(mydata[,8:ncol(mydata)])){
  names=paste(names, "+ ","ns(",i,",df=1)")
}

formula=as.formula(names)
model_ns1 <-lm(formula, data = mydata)
model_ns1$type="lm"
summary(model_ns1)

# hist(model_gam_ns1$residuals)
# qqnorm(model_gam_ns1$residuals)
# #pretty normal :')
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -25168.2  -2598.8   -585.7   1801.8  25996.9 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                            16118.90    1868.62   8.626  < 2e-16 ***
#   ns(Users_rated, df = 3)1              -34648.07     558.29 -62.061  < 2e-16 ***
#   ns(Users_rated, df = 3)2              -22773.16     677.50 -33.613  < 2e-16 ***
#   ns(Users_rated, df = 3)3               10981.23    1515.81   7.244 4.49e-13 ***
#   ns(Year, df = 3)1                       2009.27    1198.80   1.676 0.093740 .  
#   ns(Year, df = 3)2                       5961.55    4065.95   1.466 0.142605    
#   ns(Year, df = 3)3                      -6515.27     478.27 -13.623  < 2e-16 ***
#   ns(weights, df = 3)1                   -5567.80     177.81 -31.313  < 2e-16 ***
#   ns(weights, df = 3)2                   -6793.72     246.13 -27.602  < 2e-16 ***
#   ns(weights, df = 3)3                   -4380.91     292.51 -14.977  < 2e-16 ***
#   ns(Playing_time, df = 3)1              -4122.08    2269.68  -1.816 0.069361 .  
#   ns(Playing_time, df = 3)2               1458.42    2062.68   0.707 0.479544    
#   ns(Playing_time, df = 3)3               6093.81    3815.51   1.597 0.110255    
#   ns(Min_age, df = 3)1                    -404.69     203.31  -1.991 0.046544 *  
#   ns(Min_age, df = 3)2                    3396.02     522.95   6.494 8.54e-11 ***
#   ns(Min_age, df = 3)3                    4465.90     791.15   5.645 1.67e-08 ***
#   ns(Economic, df = 1)                     859.19     147.60   5.821 5.93e-09 ***
#   ns(Negotiation, df = 1)                 1248.50     201.11   6.208 5.47e-10 ***
#   ns(Card.Game, df = 1)                   -270.59      80.38  -3.366 0.000763 ***
#   ns(Fantasy, df = 1)                       20.75     109.64   0.189 0.849909    
#   ns(Medieval, df = 1)                    -197.98     159.20  -1.244 0.213653    
#   ns(Ancient, df = 1)                     -451.73     185.69  -2.433 0.014995 *  
#   ns(Territory.Building, df = 1)           167.12     222.29   0.752 0.452169    
#   ns(Nautical, df = 1)                    -246.34     198.15  -1.243 0.213807    
#   ns(Exploration, df = 1)                  109.14     180.50   0.605 0.545415    
#   ns(Farming, df = 1)                     -746.25     318.58  -2.342 0.019169 *  
#   ns(Bluffing, df = 1)                    -802.27     154.43  -5.195 2.07e-07 ***
#   ns(Collectible.Components, df = 1)      1126.29     268.26   4.199 2.70e-05 ***
#   ns(Miniatures, df = 1)                  -817.32     161.08  -5.074 3.93e-07 ***
#   ns(City.Building, df = 1)               -730.05     220.63  -3.309 0.000938 ***
#   ns(Wargame, df = 1)                     -659.71     118.91  -5.548 2.93e-08 ***
#   ns(Adventure, df = 1)                    900.01     163.04   5.520 3.42e-08 ***
#   ns(Renaissance, df = 1)                 -872.00     302.26  -2.885 0.003919 ** 
#   ns(Modern.Warfare, df = 1)              1153.58     259.05   4.453 8.50e-06 ***
#   ns(Humor, df = 1)                        606.60     150.08   4.042 5.32e-05 ***
#   ns(Electronic, df = 1)                  1474.94     347.53   4.244 2.20e-05 ***
#   ns(Deduction, df = 1)                   -216.84     157.74  -1.375 0.169248    
#   ns(Word.Game, df = 1)                   -706.92     229.98  -3.074 0.002117 ** 
#   ns(Aviation...Flight, df = 1)           -176.41     297.38  -0.593 0.553051    
#   ns(Movies...TV...Radio.theme, df = 1)   1775.48     156.82  11.321  < 2e-16 ***
#   ns(Memory, df = 1)                       287.27     219.61   1.308 0.190866    
#   ns(Puzzle, df = 1)                      -521.24     195.59  -2.665 0.007705 ** 
#   ns(Real.time, df = 1)                   -604.63     188.80  -3.202 0.001365 ** 
#   ns(Trivia, df = 1)                      1227.26     214.95   5.710 1.15e-08 ***
#   ns(Industry...Manufacturing, df = 1)    -336.75     295.82  -1.138 0.254984    
#   ns(Age.of.Reason, df = 1)               -732.11     381.91  -1.917 0.055256 .  
#   ns(Trains, df = 1)                     -1960.98     271.39  -7.226 5.15e-13 ***
#   ns(Animals, df = 1)                     -221.34     143.58  -1.542 0.123191    
#   ns(X.Childrens.Game., df = 1)            751.97     148.66   5.058 4.26e-07 ***
#   ns(Sports, df = 1)                     -1210.16     222.26  -5.445 5.24e-08 ***
#   ns(Action...Dexterity, df = 1)          -701.15     164.44  -4.264 2.02e-05 ***
#   ns(Math, df = 1)                         374.79     397.40   0.943 0.345641    
#   ns(Video.Game.Theme, df = 1)            1424.15     263.23   5.410 6.36e-08 ***
#   ns(Mature...Adult, df = 1)              1121.48     441.73   2.539 0.011130 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3869 on 21407 degrees of freedom
# Multiple R-squared:  0.6268,	Adjusted R-squared:  0.6259 
# F-statistic: 678.3 on 53 and 21407 DF,  p-value: < 2.2e-16


#gam::plot.Gam(model_ns1, se=TRUE)


R2adj=R_extr(model_ns1)
R2adj.perm=perm_sel(model_ns1)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#all true again
#let's delete them:
to_delete_ind=which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE]))
names="Rank~"
for(i in colnames(mydata[,c(3:7)[!c(3:7)%in%to_delete_ind[which(to_delete_ind<=7)]]])){
  names=paste(names, "+ ","ns(",i,",df=3)")
}

for(i in colnames(mydata[,c(-seq(1:7),-to_delete_ind[which(to_delete_ind>7)])])){
  names=paste(names, "+ ","ns(",i,",df=1)")
}

formula=as.formula(names)
model_ns1.1<-lm(formula, data = mydata)
model_ns1.1$type="lm"
summary(model_ns1.1)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -24516  -2793   -752   1942  25593 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                            19170.18     146.29 131.046  < 2e-16 ***
#   ns(Users_rated, df = 3)1              -32835.34     589.15 -55.733  < 2e-16 ***
#   ns(Users_rated, df = 3)2              -21640.27     717.03 -30.180  < 2e-16 ***
#   ns(Users_rated, df = 3)3               11046.64    1606.95   6.874 6.40e-12 ***
#   ns(weights, df = 3)1                   -5976.68     184.36 -32.419  < 2e-16 ***
#   ns(weights, df = 3)2                   -7223.88     250.32 -28.859  < 2e-16 ***
#   ns(weights, df = 3)3                   -5036.49     294.09 -17.126  < 2e-16 ***
#   ns(Min_age, df = 3)1                    -579.72     212.49  -2.728 0.006372 ** 
#   ns(Min_age, df = 3)2                    1311.20     552.25   2.374 0.017591 *  
#   ns(Min_age, df = 3)3                     943.76     835.06   1.130 0.258413    
#   ns(Economic, df = 1)                    1234.09     151.77   8.131 4.48e-16 ***
#   ns(Negotiation, df = 1)                 1732.81     212.56   8.152 3.77e-16 ***
#   ns(Card.Game, df = 1)                   -527.57      84.44  -6.248 4.24e-10 ***
#   ns(Ancient, df = 1)                     -307.47     196.23  -1.567 0.117163    
#   ns(Farming, df = 1)                    -1238.66     335.99  -3.687 0.000228 ***
#   ns(Bluffing, df = 1)                    -814.67     157.25  -5.181 2.23e-07 ***
#   ns(Collectible.Components, df = 1)      2313.84     282.53   8.190 2.76e-16 ***
#   ns(Miniatures, df = 1)                 -1385.25     168.82  -8.206 2.42e-16 ***
#   ns(City.Building, df = 1)              -1048.54     232.77  -4.505 6.68e-06 ***
#   ns(Wargame, df = 1)                      535.06     119.46   4.479 7.53e-06 ***
#   ns(Adventure, df = 1)                    717.85     158.95   4.516 6.33e-06 ***
#   ns(Renaissance, df = 1)                 -730.16     319.68  -2.284 0.022377 *  
#   ns(Modern.Warfare, df = 1)              1158.58     274.15   4.226 2.39e-05 ***
#   ns(Humor, df = 1)                        824.57     158.82   5.192 2.10e-07 ***
#   ns(Electronic, df = 1)                  2051.38     368.03   5.574 2.52e-08 ***
#   ns(Word.Game, df = 1)                   -162.67     243.22  -0.669 0.503611    
#   ns(Movies...TV...Radio.theme, df = 1)   1938.72     165.83  11.691  < 2e-16 ***
#   ns(Puzzle, df = 1)                     -1388.12     205.98  -6.739 1.64e-11 ***
#   ns(Real.time, df = 1)                  -1001.92     199.53  -5.021 5.17e-07 ***
#   ns(Trivia, df = 1)                      2244.97     226.38   9.917  < 2e-16 ***
#   ns(Age.of.Reason, df = 1)               -717.89     404.64  -1.774 0.076054 .  
#   ns(Trains, df = 1)                     -1581.91     286.37  -5.524 3.35e-08 ***
#   ns(X.Childrens.Game., df = 1)           1265.18     153.30   8.253  < 2e-16 ***
#   ns(Sports, df = 1)                      -514.38     234.73  -2.191 0.028435 *  
#   ns(Action...Dexterity, df = 1)          -697.05     173.81  -4.010 6.08e-05 ***
#   ns(Video.Game.Theme, df = 1)             871.20     278.51   3.128 0.001762 ** 
#   ns(Mature...Adult, df = 1)              2120.27     467.96   4.531 5.90e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4104 on 21424 degrees of freedom
# Multiple R-squared:  0.5797,	Adjusted R-squared:  0.579 
# F-statistic: 820.7 on 36 and 21424 DF,  p-value: < 2.2e-16


#let's do perm sel one more time
R2adj=R_extr(model_ns1.1)
R2adj.perm=perm_sel(model_ns1.1)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)

to_delete_ind=c(to_delete_ind,which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE])))

names="Rank~"
for(i in colnames(mydata[,c(3:7)[!c(3:7)%in%to_delete_ind[which(to_delete_ind<=7)]]])){
  names=paste(names, "+ ","ns(",i,",df=3)")
}

for(i in colnames(mydata[,c(-seq(1:7),-to_delete_ind[which(to_delete_ind>7)])])){
  names=paste(names, "+ ","ns(",i,",df=1)")
}

formula=as.formula(names)
model_ns1.2<-lm(formula, data = mydata)
summary(model_ns1.2)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -24491.7  -2802.8   -749.7   1953.0  25713.0 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                            19551.73      96.18 203.275  < 2e-16 ***
#   ns(Users_rated, df = 3)1              -32841.91     588.95 -55.763  < 2e-16 ***
#   ns(Users_rated, df = 3)2              -21583.99     716.99 -30.104  < 2e-16 ***
#   ns(Users_rated, df = 3)3               11118.00    1607.50   6.916 4.77e-12 ***
#   ns(weights, df = 3)1                   -6224.89     173.52 -35.873  < 2e-16 ***
#   ns(weights, df = 3)2                   -7450.68     243.37 -30.614  < 2e-16 ***
#   ns(weights, df = 3)3                   -5201.63     291.17 -17.865  < 2e-16 ***
#   ns(Economic, df = 1)                    1239.50     151.74   8.169 3.29e-16 ***
#   ns(Negotiation, df = 1)                 1723.47     212.57   8.108 5.42e-16 ***
#   ns(Card.Game, df = 1)                   -533.63      84.22  -6.336 2.40e-10 ***
#   ns(Farming, df = 1)                    -1216.85     336.09  -3.621 0.000295 ***
#   ns(Bluffing, df = 1)                    -820.46     157.23  -5.218 1.83e-07 ***
#   ns(Collectible.Components, df = 1)      2317.01     282.41   8.204 2.45e-16 ***
#   ns(Miniatures, df = 1)                 -1396.91     168.75  -8.278  < 2e-16 ***
#   ns(City.Building, df = 1)              -1048.13     232.43  -4.509 6.54e-06 ***
#   ns(Wargame, df = 1)                      450.51     117.60   3.831 0.000128 ***
#   ns(Adventure, df = 1)                    702.34     158.79   4.423 9.77e-06 ***
#   ns(Renaissance, df = 1)                 -755.67     319.73  -2.363 0.018113 *  
#   ns(Modern.Warfare, df = 1)              1171.79     273.99   4.277 1.90e-05 ***
#   ns(Humor, df = 1)                        757.07     157.78   4.798 1.61e-06 ***
#   ns(Electronic, df = 1)                  2054.09     368.12   5.580 2.44e-08 ***
#   ns(Movies...TV...Radio.theme, df = 1)   1943.02     165.52  11.739  < 2e-16 ***
#   ns(Puzzle, df = 1)                     -1354.06     205.94  -6.575 4.98e-11 ***
#   ns(Real.time, df = 1)                   -984.73     199.14  -4.945 7.68e-07 ***
#   ns(Trivia, df = 1)                      2124.71     224.12   9.480  < 2e-16 ***
#   ns(Age.of.Reason, df = 1)               -715.35     404.55  -1.768 0.077033 .  
#   ns(Trains, df = 1)                     -1584.99     286.27  -5.537 3.12e-08 ***
#   ns(X.Childrens.Game., df = 1)           1357.76     148.54   9.141  < 2e-16 ***
#   ns(Sports, df = 1)                      -519.10     234.53  -2.213 0.026883 *  
#   ns(Action...Dexterity, df = 1)          -671.35     173.45  -3.871 0.000109 ***
#   ns(Video.Game.Theme, df = 1)             877.01     278.51   3.149 0.001641 ** 
#   ns(Mature...Adult, df = 1)              2065.20     418.45   4.935 8.06e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4106 on 21429 degrees of freedom
# Multiple R-squared:  0.5792,	Adjusted R-squared:  0.5786 
# F-statistic: 951.3 on 31 and 21429 DF,  p-value: < 2.2e-16


#2.3) degree 2 for num and 1 for cat----------------------------------------------
names="Rank~"
for(i in colnames(mydata[,3:7])){
  names=paste(names, "+ ","ns(",i,",df=2)")
}

for(i in colnames(mydata[,8:ncol(mydata)])){
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
# -40517  -3964    -85   3800  28535 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                             4319.5     2388.0   1.809 0.070496 .  
#   ns(Users_rated, df = 2)1              -31823.5      889.7 -35.769  < 2e-16 ***
#   ns(Users_rated, df = 2)2               42102.6     2044.8  20.591  < 2e-16 ***
#   ns(Year, df = 2)1                      30781.0     4871.1   6.319 2.68e-10 ***
#   ns(Year, df = 2)2                      -6777.0      502.6 -13.483  < 2e-16 ***
#   ns(weights, df = 2)1                  -12964.1      253.7 -51.100  < 2e-16 ***
#   ns(weights, df = 2)2                   -5155.5      335.6 -15.362  < 2e-16 ***
#   ns(Playing_time, df = 2)1             -10359.6     3643.9  -2.843 0.004473 ** 
#   ns(Playing_time, df = 2)2               9554.5     5106.0   1.871 0.061324 .  
#   ns(Min_age, df = 2)1                   -2274.6      294.6  -7.721 1.20e-14 ***
#   ns(Min_age, df = 2)2                    -670.4      550.8  -1.217 0.223587    
#   ns(Economic, df = 1)                    1138.1      189.1   6.018 1.80e-09 ***
#   ns(Negotiation, df = 1)                 1897.4      258.1   7.352 2.03e-13 ***
#   ns(Card.Game, df = 1)                   -687.6      102.8  -6.690 2.28e-11 ***
#   ns(Fantasy, df = 1)                     -462.9      140.6  -3.292 0.000995 ***
#   ns(Medieval, df = 1)                    -188.4      204.6  -0.921 0.356998    
#   ns(Ancient, df = 1)                     -814.3      238.6  -3.413 0.000645 ***
#   ns(Territory.Building, df = 1)          -671.2      285.6  -2.351 0.018748 *  
#   ns(Nautical, df = 1)                    -643.7      254.6  -2.528 0.011473 *  
#   ns(Exploration, df = 1)                 -181.2      231.9  -0.781 0.434724    
#   ns(Farming, df = 1)                    -1409.2      409.5  -3.441 0.000580 ***
#   ns(Bluffing, df = 1)                    -893.1      198.4  -4.502 6.76e-06 ***
#   ns(Collectible.Components, df = 1)      2422.1      343.2   7.057 1.76e-12 ***
#   ns(Miniatures, df = 1)                 -1599.8      206.4  -7.751 9.49e-15 ***
#   ns(City.Building, df = 1)              -1610.0      283.4  -5.681 1.36e-08 ***
#   ns(Wargame, df = 1)                     1921.7      146.3  13.135  < 2e-16 ***
#   ns(Adventure, df = 1)                    984.7      209.4   4.702 2.59e-06 ***
#   ns(Renaissance, df = 1)                -1300.2      388.4  -3.348 0.000816 ***
#   ns(Modern.Warfare, df = 1)              1100.1      333.1   3.303 0.000958 ***
#   ns(Humor, df = 1)                        803.4      192.4   4.176 2.98e-05 ***
#   ns(Electronic, df = 1)                  1806.6      446.5   4.046 5.22e-05 ***
#   ns(Deduction, df = 1)                  -1051.4      202.2  -5.200 2.02e-07 ***
#   ns(Word.Game, df = 1)                   -371.2      295.4  -1.257 0.208929    
#   ns(Aviation...Flight, df = 1)          -1611.4      381.9  -4.219 2.46e-05 ***
#   ns(Movies...TV...Radio.theme, df = 1)   2024.2      201.4  10.051  < 2e-16 ***
#   ns(Memory, df = 1)                       978.9      281.9   3.472 0.000517 ***
#   ns(Puzzle, df = 1)                     -1380.6      250.4  -5.512 3.58e-08 ***
#   ns(Real.time, df = 1)                  -1739.1      241.8  -7.194 6.51e-13 ***
#   ns(Trivia, df = 1)                      2676.4      274.0   9.766  < 2e-16 ***
#   ns(Industry...Manufacturing, df = 1)   -1160.0      380.1  -3.052 0.002279 ** 
#   ns(Age.of.Reason, df = 1)              -1136.2      491.0  -2.314 0.020668 *  
#   ns(Trains, df = 1)                     -2069.2      348.6  -5.935 2.98e-09 ***
#   ns(Animals, df = 1)                    -1264.4      184.0  -6.871 6.53e-12 ***
#   ns(X.Childrens.Game., df = 1)           1189.6      186.9   6.364 2.00e-10 ***
#   ns(Sports, df = 1)                      -146.4      285.1  -0.514 0.607549    
#   ns(Action...Dexterity, df = 1)         -1369.7      210.5  -6.508 7.76e-11 ***
#   ns(Math, df = 1)                        1182.4      510.6   2.316 0.020571 *  
#   ns(Video.Game.Theme, df = 1)             757.1      338.2   2.239 0.025164 *  
#   ns(Mature...Adult, df = 1)              2241.5      543.6   4.123 3.75e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4974 on 21412 degrees of freedom
# Multiple R-squared:  0.3829,	Adjusted R-squared:  0.3815 
# F-statistic: 276.8 on 48 and 21412 DF,  p-value: < 2.2e-16
#gam::plot.Gam(model_ns2, se=TRUE)

R2adj=R_extr(model_ns2)
R2adj.perm=perm_sel(model_ns2)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#all true again
#let's delete them:
to_delete_ind=which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE]))
names="Rank~"
for(i in colnames(mydata[,c(3:7)[!c(3:7)%in%to_delete_ind[which(to_delete_ind<=7)]]])){
  names=paste(names, "+ ","ns(",i,",df=2)")
}

for(i in colnames(mydata[,c(-seq(1:7),-to_delete_ind[which(to_delete_ind>7)])])){
  names=paste(names, "+ ","ns(",i,",df=1)")
}

formula=as.formula(names)
model_ns2.1<-lm(formula, data = mydata)
model_ns2.1$type="lm"
summary(model_ns2.1)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -40813  -3959    -34   3819  28674 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                             3350.80    2389.68   1.402  0.160871    
#   ns(Users_rated, df = 2)1              -32054.76     890.86 -35.982  < 2e-16 ***
#   ns(Users_rated, df = 2)2               42562.44    2047.75  20.785  < 2e-16 ***
#   ns(Year, df = 2)1                      31306.31    4878.42   6.417 1.42e-10 ***
#   ns(Year, df = 2)2                      -7159.08     501.52 -14.275  < 2e-16 ***
#   ns(weights, df = 2)1                  -13371.78     242.06 -55.242  < 2e-16 ***
#   ns(weights, df = 2)2                   -5292.90     331.71 -15.956  < 2e-16 ***
#   ns(Playing_time, df = 2)1             -10133.79    3649.52  -2.777 0.005495 ** 
#   ns(Playing_time, df = 2)2               9312.24    5115.15   1.821 0.068694 .  
#   ns(Economic, df = 1)                    1123.87     189.36   5.935 2.98e-09 ***
#   ns(Negotiation, df = 1)                 1866.54     258.46   7.222 5.31e-13 ***
#   ns(Card.Game, df = 1)                   -706.82     102.78  -6.877 6.28e-12 ***
#   ns(Fantasy, df = 1)                     -463.91     140.53  -3.301 0.000965 ***
#   ns(Medieval, df = 1)                    -190.19     204.93  -0.928 0.353380    
#   ns(Territory.Building, df = 1)          -741.69     285.83  -2.595 0.009471 ** 
#   ns(Nautical, df = 1)                    -650.79     254.97  -2.552 0.010704 *  
#   ns(Exploration, df = 1)                 -202.04     232.33  -0.870 0.384503    
#   ns(Farming, df = 1)                    -1411.67     410.19  -3.442 0.000580 ***
#   ns(Bluffing, df = 1)                    -914.43     198.68  -4.603 4.20e-06 ***
#   ns(Collectible.Components, df = 1)      2542.58     343.59   7.400 1.41e-13 ***
#   ns(Miniatures, df = 1)                 -1613.28     206.58  -7.809 6.01e-15 ***
#   ns(City.Building, df = 1)              -1672.29     283.46  -5.900 3.70e-09 ***
#   ns(Wargame, df = 1)                     1987.91     145.90  13.625  < 2e-16 ***
#   ns(Adventure, df = 1)                    974.57     209.76   4.646 3.40e-06 ***
#   ns(Renaissance, df = 1)                -1313.87     389.05  -3.377 0.000734 ***
#   ns(Modern.Warfare, df = 1)              1144.80     333.31   3.435 0.000594 ***
#   ns(Humor, df = 1)                        724.78     191.73   3.780 0.000157 ***
#   ns(Electronic, df = 1)                  1846.38     447.24   4.128 3.67e-05 ***
#   ns(Deduction, df = 1)                  -1103.32     202.30  -5.454 4.99e-08 ***
#   ns(Aviation...Flight, df = 1)          -1583.34     382.37  -4.141 3.47e-05 ***
#   ns(Movies...TV...Radio.theme, df = 1)   2013.56     201.39   9.998  < 2e-16 ***
#   ns(Memory, df = 1)                      1043.51     282.31   3.696 0.000219 ***
#   ns(Puzzle, df = 1)                     -1335.68     250.76  -5.327 1.01e-07 ***
#   ns(Real.time, df = 1)                  -1777.43     241.63  -7.356 1.97e-13 ***
#   ns(Trivia, df = 1)                      2530.79     272.79   9.277  < 2e-16 ***
#   ns(Industry...Manufacturing, df = 1)   -1171.31     380.55  -3.078 0.002087 ** 
#   ns(Age.of.Reason, df = 1)              -1016.59     491.51  -2.068 0.038624 *  
#   ns(Trains, df = 1)                     -1957.09     348.94  -5.609 2.06e-08 ***
#   ns(Animals, df = 1)                    -1194.19     183.73  -6.500 8.23e-11 ***
#   ns(X.Childrens.Game., df = 1)           1468.40     184.15   7.974 1.61e-15 ***
#   ns(Sports, df = 1)                       -39.38     285.31  -0.138 0.890220    
#   ns(Action...Dexterity, df = 1)         -1286.74     210.43  -6.115 9.84e-10 ***
#   ns(Math, df = 1)                        1233.41     511.36   2.412 0.015873 *  
#   ns(Video.Game.Theme, df = 1)             754.25     338.64   2.227 0.025938 *  
#   ns(Mature...Adult, df = 1)              1863.70     507.59   3.672 0.000242 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4983 on 21416 degrees of freedom
# Multiple R-squared:  0.3805, Adjusted R-squared:  0.3792 	 
# F-statistic: 298.9 on 44 and 21416 DF,  p-value: < 2.2e-16
ypred <- predict(model_ns2.1, newdata=xnew)
temp=sample(21461,10000)
dev.new()
plot(ypred[temp],mydata$Rank[temp],main="Response vs. Fitted Values",xlab="Fitted Values",ylab="Response")
lines(seq(0,21461,0.5),seq(0,21461,0.5),col="red")

dev.new()
plot(ypred[ypred>0][temp],mydata$Rank[ypred>0][temp],main="Response vs. Fitted Values",xlab="Fitted Values",ylab="Response")
lines(seq(0,21461,0.5),seq(0,21461,0.5),col="red")
#piccola parentesi___________
out=ypred[ypred>30000]
mydata[names(out),]
#_______________________________

R2adj=R_extr(model_ns2.1)
R2adj.perm=perm_sel(model_ns2.1)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#all true again
#let's delete them:
to_delete_ind=c(to_delete_ind,which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE])))
names="Rank~-1"
for(i in colnames(mydata[,c(3:7)[!c(3:7)%in%to_delete_ind[which(to_delete_ind<=7)]]])){
  names=paste(names, "+ ","ns(",i,",df=2)")
}

for(i in colnames(mydata[,c(-seq(1:7),-to_delete_ind[which(to_delete_ind>7)])])){
  names=paste(names, "+ ","ns(",i,",df=1)")
}

formula=as.formula(names)
model_ns2.2<-lm(formula, data = mydata)
model_ns2.2$type="lm"
summary(model_ns2.2)
# 

# Residuals:
#   Min     1Q Median     3Q    Max 
# -40799  -3974    -38   3817  28683 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   ns(Users_rated, df = 2)1              -32069.4      890.5 -36.014  < 2e-16 ***
#   ns(Users_rated, df = 2)2               42569.0     2047.3  20.793  < 2e-16 ***
#   ns(Year, df = 2)1                      38108.2      509.5  74.796  < 2e-16 ***
#   ns(Year, df = 2)2                      -7286.7      494.3 -14.740  < 2e-16 ***
#   ns(weights, df = 2)1                  -13390.7      241.0 -55.569  < 2e-16 ***
#   ns(weights, df = 2)2                   -5295.5      331.6 -15.969  < 2e-16 ***
#   ns(Playing_time, df = 2)1             -10105.2     3648.9  -2.769 0.005621 ** 
#   ns(Playing_time, df = 2)2               9299.6     5115.0   1.818 0.069064 .  
#   ns(Economic, df = 1)                    1124.9      189.2   5.944 2.82e-09 ***
#   ns(Negotiation, df = 1)                 1858.7      258.3   7.195 6.44e-13 ***
#   ns(Card.Game, df = 1)                   -699.5      102.6  -6.817 9.54e-12 ***
#   ns(Fantasy, df = 1)                     -474.8      140.0  -3.393 0.000694 ***
#   ns(Territory.Building, df = 1)          -760.3      285.2  -2.665 0.007694 ** 
#   ns(Nautical, df = 1)                    -658.6      254.5  -2.587 0.009678 ** 
#   ns(Farming, df = 1)                    -1405.0      410.1  -3.426 0.000613 ***
#   ns(Bluffing, df = 1)                    -918.0      198.6  -4.622 3.82e-06 ***
#   ns(Collectible.Components, df = 1)      2547.4      343.4   7.419 1.23e-13 ***
#   ns(Miniatures, df = 1)                 -1613.7      206.4  -7.818 5.63e-15 ***
#   ns(City.Building, df = 1)              -1681.8      282.9  -5.945 2.81e-09 ***
#   ns(Wargame, df = 1)                     1993.4      144.7  13.780  < 2e-16 ***
#   ns(Adventure, df = 1)                    916.7      198.1   4.628 3.72e-06 ***
#   ns(Renaissance, df = 1)                -1335.9      388.4  -3.439 0.000585 ***
#   ns(Modern.Warfare, df = 1)              1153.6      333.2   3.462 0.000536 ***
#   ns(Humor, df = 1)                        729.5      191.7   3.806 0.000142 ***
#   ns(Electronic, df = 1)                  1840.1      447.1   4.115 3.88e-05 ***
#   ns(Deduction, df = 1)                  -1095.0      202.1  -5.418 6.10e-08 ***
#   ns(Aviation...Flight, df = 1)          -1576.0      382.2  -4.123 3.75e-05 ***
#   ns(Movies...TV...Radio.theme, df = 1)   2023.9      201.1  10.062  < 2e-16 ***
#   ns(Memory, df = 1)                      1039.3      282.3   3.682 0.000232 ***
#   ns(Puzzle, df = 1)                     -1331.4      250.6  -5.312 1.09e-07 ***
#   ns(Real.time, df = 1)                  -1774.1      241.6  -7.345 2.14e-13 ***
#   ns(Trivia, df = 1)                      2537.9      272.6   9.308  < 2e-16 ***
#   ns(Industry...Manufacturing, df = 1)   -1160.6      380.5  -3.050 0.002288 ** 
#   ns(Age.of.Reason, df = 1)              -1003.6      491.4  -2.042 0.041134 *  
#   ns(Trains, df = 1)                     -1937.3      348.5  -5.559 2.75e-08 ***
#   ns(Animals, df = 1)                    -1187.1      183.6  -6.465 1.04e-10 ***
#   ns(X.Childrens.Game., df = 1)           1471.5      184.0   7.997 1.34e-15 ***
#   ns(Action...Dexterity, df = 1)         -1279.0      210.2  -6.084 1.19e-09 ***
#   ns(Math, df = 1)                        1236.6      511.3   2.418 0.015595 *  
#   ns(Video.Game.Theme, df = 1)             761.4      338.6   2.249 0.024531 *  
#   ns(Mature...Adult, df = 1)              1867.3      507.5   3.679 0.000234 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4983 on 21420 degrees of freedom
# Multiple R-squared:  0.8431,	Adjusted R-squared:  0.8428 
# F-statistic:  2807 on 41 and 21420 DF,  p-value: < 2.2e-16






# gam::plot.Gam(model_ns2.2, se=TRUE)


#2.3.1)model assessment---------------------------------------------------------------------
vif(model_ns2.2)
# ns(Year, df = 2)                      8.107246  2        1.687401
# ns(weights, df = 2)                   8.803173  2        1.722502 
cor(mydata)
#year,weights =0.041448803


#prediction
xnew=mydata[,3:45]
ypred <- predict(model_ns2.2, newdata=xnew)
temp=sample(21461,10000)
dev.new()
plot(ypred[temp],mydata$Rank[temp],main="Response vs. Fitted Values",xlab="Fitted Values",ylab="Response")
lines(seq(0,21461,0.5),seq(0,21461,0.5),col="red")

dev.new()
plot(ypred[ypred>0],mydata$Rank[ypred>0],main="Response vs. Fitted Values",xlab="Fitted Values",ylab="Response")
lines(seq(0,21461,0.5),seq(0,21461,0.5),col="red")


#3)semiparametric models--------------------------------------------------------
#3.1) ns for numeric-----------
#(degree=2, maybe the previous nonpar models with degree 3 overfit data being R^2 adj so high)

names="Rank~"
for(i in colnames(mydata[,3:7])){
  names=paste(names, "+ ","ns(",i,",df=2)")
}

for(i in colnames(mydata[,8:ncol(mydata)])){
  names=paste(names, "+ ",i)
}

formula=as.formula(names)


model_sp1<-lm(formula, data = mydata)
model_sp1$type="lm"
summary(model_sp1)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -40517  -3964    -85   3800  28535 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                 4319.46    2388.02   1.809 0.070496 .  
#   ns(Users_rated, df = 2)1  -31823.51     889.69 -35.769  < 2e-16 ***
#   ns(Users_rated, df = 2)2   42102.61    2044.75  20.591  < 2e-16 ***
#   ns(Year, df = 2)1          30780.99    4871.11   6.319 2.68e-10 ***
#   ns(Year, df = 2)2          -6777.01     502.65 -13.483  < 2e-16 ***
#   ns(weights, df = 2)1      -12964.14     253.70 -51.100  < 2e-16 ***
#   ns(weights, df = 2)2       -5155.49     335.60 -15.362  < 2e-16 ***
#   ns(Playing_time, df = 2)1 -10359.58    3643.90  -2.843 0.004473 ** 
#   ns(Playing_time, df = 2)2   9554.52    5105.96   1.871 0.061324 .  
#   ns(Min_age, df = 2)1       -2274.64     294.60  -7.721 1.20e-14 ***
#   ns(Min_age, df = 2)2        -670.36     550.79  -1.217 0.223587    
#   Economic                     912.48     151.63   6.018 1.80e-09 ***
#   Negotiation                 1521.34     206.93   7.352 2.03e-13 ***
#   Card.Game                   -551.32      82.41  -6.690 2.28e-11 ***
#   Fantasy                     -371.17     112.73  -3.292 0.000995 ***
#   Medieval                    -151.09     164.03  -0.921 0.356998    
#   Ancient                     -652.91     191.32  -3.413 0.000645 ***
#   Territory.Building          -538.20     228.95  -2.351 0.018748 *  
#   Nautical                    -516.12     204.15  -2.528 0.011473 *  
#   Exploration                 -145.27     185.97  -0.781 0.434724    
#   Farming                    -1129.85     328.31  -3.441 0.000580 ***
#   Bluffing                    -716.11     159.05  -4.502 6.76e-06 ***
#   Collectible.Components      1942.01     275.20   7.057 1.76e-12 ***
#   Miniatures                 -1282.71     165.48  -7.751 9.49e-15 ***
#   City.Building              -1290.89     227.23  -5.681 1.36e-08 ***
#   Wargame                     1540.77     117.30  13.135  < 2e-16 ***
#   Adventure                    789.53     167.92   4.702 2.59e-06 ***
#   Renaissance                -1042.51     311.40  -3.348 0.000816 ***
#   Modern.Warfare               882.01     267.04   3.303 0.000958 ***
#   Humor                        644.15     154.25   4.176 2.98e-05 ***
#   Electronic                  1448.51     357.97   4.046 5.22e-05 ***
#   Deduction                   -842.98     162.13  -5.200 2.02e-07 ***
#   Word.Game                   -297.64     236.87  -1.257 0.208929    
#   Aviation...Flight          -1292.02     306.21  -4.219 2.46e-05 ***
#   Movies...TV...Radio.theme   1622.99     161.48  10.051  < 2e-16 ***
#   Memory                       784.85     226.03   3.472 0.000517 ***
#   Puzzle                     -1106.92     200.80  -5.512 3.58e-08 ***
#   Real.time                  -1394.40     193.83  -7.194 6.51e-13 ***
#   Trivia                      2145.91     219.72   9.766  < 2e-16 ***
#   Industry...Manufacturing    -930.05     304.77  -3.052 0.002279 ** 
#   Age.of.Reason               -911.01     393.66  -2.314 0.020668 *  
#   Trains                     -1659.07     279.53  -5.935 2.98e-09 ***
#   Animals                    -1013.80     147.54  -6.871 6.53e-12 ***
#   X.Childrens.Game.            953.79     149.87   6.364 2.00e-10 ***
#   Sports                      -117.41     228.61  -0.514 0.607549    
#   Action...Dexterity         -1098.23     168.74  -6.508 7.76e-11 ***
#   Math                         948.06     409.36   2.316 0.020571 *  
#   Video.Game.Theme             607.06     271.13   2.239 0.025164 *  
#   Mature...Adult              1797.20     435.85   4.123 3.75e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4974 on 21412 degrees of freedom
# Multiple R-squared:  0.3829,	Adjusted R-squared:  0.3815 
# F-statistic: 276.8 on 48 and 21412 DF,  p-value: < 2.2e-16


R2adj=R_extr(model_sp1)
R2adj.perm=perm_sel(model_sp1)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#all true again
#let's delete them:
to_delete_ind=which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE]))
names="Rank~"
for(i in colnames(mydata[,c(3:7)[!c(3:7)%in%to_delete_ind[which(to_delete_ind<=7)]]])){
  names=paste(names, "+ ","ns(",i,",df=2)")
}

for(i in colnames(mydata[,c(-seq(1:7),-to_delete_ind[which(to_delete_ind>7)])])){
  names=paste(names, "+ ",i)
}

formula=as.formula(names)
model_sp1.1<-lm(formula, data = mydata)
model_sp1.1$type="lm"
summary(model_sp1.1)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -40848  -3962    -35   3817  28614 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                 3505.34    2389.13   1.467 0.142335    
#   ns(Users_rated, df = 2)1  -32039.34     890.33 -35.986  < 2e-16 ***
#   ns(Users_rated, df = 2)2   42573.62    2046.81  20.800  < 2e-16 ***
#   ns(Year, df = 2)1          30993.81    4876.85   6.355 2.12e-10 ***
#   ns(Year, df = 2)2          -7145.58     501.33 -14.253  < 2e-16 ***
#   ns(weights, df = 2)1      -13342.76     241.55 -55.239  < 2e-16 ***
#   ns(weights, df = 2)2       -5297.02     331.51 -15.978  < 2e-16 ***
#   ns(Playing_time, df = 2)1 -10266.76    3648.15  -2.814 0.004894 ** 
#   ns(Playing_time, df = 2)2   9323.47    5113.60   1.823 0.068277 .  
#   Economic                     900.85     151.69   5.939 2.92e-09 ***
#   Negotiation                 1498.47     207.08   7.236 4.77e-13 ***
#   Card.Game                   -568.00      82.27  -6.904 5.20e-12 ***
#   Fantasy                     -401.77     112.34  -3.576 0.000349 ***
#   Ancient                     -663.94     191.54  -3.466 0.000529 ***
#   Territory.Building          -581.37     228.81  -2.541 0.011066 *  
#   Nautical                    -527.13     204.04  -2.583 0.009787 ** 
#   Farming                    -1128.47     328.70  -3.433 0.000598 ***
#   Bluffing                    -735.65     159.20  -4.621 3.84e-06 ***
#   Collectible.Components      2028.54     275.29   7.369 1.79e-13 ***
#   Miniatures                 -1303.69     165.48  -7.878 3.48e-15 ***
#   City.Building              -1309.87     227.06  -5.769 8.10e-09 ***
#   Wargame                     1618.75     116.09  13.944  < 2e-16 ***
#   Adventure                    734.86     158.77   4.628 3.71e-06 ***
#   Renaissance                -1076.94     311.36  -3.459 0.000543 ***
#   Modern.Warfare               883.87     267.33   3.306 0.000947 ***
#   Humor                        576.02     153.64   3.749 0.000178 ***
#   Electronic                  1470.11     358.42   4.102 4.12e-05 ***
#   Deduction                   -892.51     162.05  -5.508 3.68e-08 ***
#   Aviation...Flight          -1297.43     306.54  -4.233 2.32e-05 ***
#   Movies...TV...Radio.theme   1607.42     161.30   9.965  < 2e-16 ***
#   Memory                       836.62     226.27   3.697 0.000218 ***
#   Puzzle                     -1078.57     200.90  -5.369 8.02e-08 ***
#   Real.time                  -1430.88     193.63  -7.390 1.53e-13 ***
#   Trivia                      2024.83     218.56   9.265  < 2e-16 ***
#   Industry...Manufacturing    -962.61     305.09  -3.155 0.001606 ** 
#   Age.of.Reason               -853.89     394.12  -2.167 0.030278 *  
#   Trains                     -1584.88     279.50  -5.670 1.44e-08 ***
#   Animals                     -965.57     147.24  -6.558 5.58e-11 ***
#   X.Childrens.Game.           1179.68     147.49   7.998 1.33e-15 ***
#   Action...Dexterity         -1036.44     168.56  -6.149 7.94e-10 ***
#   Math                         984.59     409.86   2.402 0.016303 *  
#   Video.Game.Theme             591.34     271.42   2.179 0.029368 *  
#   Mature...Adult              1496.15     406.81   3.678 0.000236 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4982 on 21418 degrees of freedom
# Multiple R-squared:  0.3808,	Adjusted R-squared:  0.3796 
# F-statistic: 313.6 on 42 and 21418 DF,  p-value: < 2.2e-16

#let's remove the intercept
names="Rank~-1"
for(i in colnames(mydata[,c(3:7)[!c(3:7)%in%to_delete_ind[which(to_delete_ind<=7)]]])){
  names=paste(names, "+ ","ns(",i,",df=2)")
}

for(i in colnames(mydata[,c(-seq(1:7),-to_delete_ind[which(to_delete_ind>7)])])){
  names=paste(names, "+ ",i)
}

formula=as.formula(names)
model_sp1.2<-lm(formula, data = mydata)
model_sp1.2$type="lm"
summary(model_sp1.2)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -40829  -3968    -34   3816  28583 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   ns(Users_rated, df = 2)1  -32028.75     890.32 -35.974  < 2e-16 ***
#   ns(Users_rated, df = 2)2   42544.10    2046.76  20.786  < 2e-16 ***
#   ns(Year, df = 2)1          38110.02     509.37  74.819  < 2e-16 ***
#   ns(Year, df = 2)2          -7268.92     494.24 -14.707  < 2e-16 ***
#   ns(weights, df = 2)1      -13333.87     241.48 -55.218  < 2e-16 ***
#   ns(weights, df = 2)2       -5295.01     331.52 -15.972  < 2e-16 ***
#   ns(Playing_time, df = 2)1 -10257.96    3648.24  -2.812 0.004932 ** 
#   ns(Playing_time, df = 2)2   9307.32    5113.73   1.820 0.068763 .  
#   Economic                     899.25     151.69   5.928 3.11e-09 ***
#   Negotiation                 1497.90     207.08   7.233 4.87e-13 ***
#   Card.Game                   -565.65      82.26  -6.876 6.31e-12 ***
#   Fantasy                     -400.03     112.33  -3.561 0.000370 ***
#   Ancient                     -658.69     191.51  -3.439 0.000584 ***
#   Territory.Building          -579.66     228.81  -2.533 0.011305 *  
#   Nautical                    -527.96     204.04  -2.588 0.009674 ** 
#   Farming                    -1127.11     328.71  -3.429 0.000607 ***
#   Bluffing                    -736.04     159.21  -4.623 3.80e-06 ***
#   Collectible.Components      2024.96     275.29   7.356 1.97e-13 ***
#   Miniatures                 -1300.62     165.47  -7.860 4.02e-15 ***
#   City.Building              -1308.63     227.07  -5.763 8.37e-09 ***
#   Wargame                     1613.79     116.04  13.907  < 2e-16 ***
#   Adventure                    734.91     158.77   4.629 3.70e-06 ***
#   Renaissance                -1075.89     311.36  -3.455 0.000551 ***
#   Modern.Warfare               883.17     267.34   3.304 0.000956 ***
#   Humor                        577.78     153.64   3.761 0.000170 ***
#   Electronic                  1467.82     358.42   4.095 4.23e-05 ***
#   Deduction                   -890.26     162.05  -5.494 3.98e-08 ***
#   Aviation...Flight          -1297.25     306.55  -4.232 2.33e-05 ***
#   Movies...TV...Radio.theme   1606.79     161.30   9.961  < 2e-16 ***
#   Memory                       834.57     226.27   3.688 0.000226 ***
#   Puzzle                     -1076.31     200.90  -5.357 8.53e-08 ***
#   Real.time                  -1429.36     193.64  -7.382 1.62e-13 ***
#   Trivia                      2025.03     218.56   9.265  < 2e-16 ***
#   Industry...Manufacturing    -960.32     305.09  -3.148 0.001648 ** 
#   Age.of.Reason               -852.11     394.13  -2.162 0.030628 *  
#   Trains                     -1584.18     279.51  -5.668 1.47e-08 ***
#   Animals                     -964.20     147.24  -6.549 5.94e-11 ***
#   X.Childrens.Game.           1180.81     147.50   8.006 1.25e-15 ***
#   Action...Dexterity         -1030.35     168.51  -6.114 9.86e-10 ***
#   Math                         982.65     409.87   2.397 0.016517 *  
#   Video.Game.Theme             593.52     271.43   2.187 0.028777 *  
#   Mature...Adult              1499.35     406.82   3.686 0.000229 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4982 on 21419 degrees of freedom
# Multiple R-squared:  0.8432,	Adjusted R-squared:  0.8429 
# F-statistic:  2742 on 42 and 21419 DF,  p-value: < 2.2e-16
#prediction

ypred <- predict(model_sp1.2, newdata=xnew)
temp=sample(21461,10000)
dev.new()
plot(ypred[temp],mydata$Rank[temp],main="Response vs. Fitted Values",xlab="Fitted Values",ylab="Response")
lines(seq(0,21461,0.5),seq(0,21461,0.5),col="red")

dev.new()
plot(ypred[ypred>0],mydata$Rank[ypred>0],main="Response vs. Fitted Values",xlab="Fitted Values",ylab="Response")
lines(seq(0,21461,0.5),seq(0,21461,0.5),col="red")



#3.1.1)model assessment-------------------------
vif(model_sp1.2)

# ns(Year, df = 2)          8.114652  2        1.687786
# ns(weights, df = 2)       8.850632  2        1.724819
#borderline but ok vif

scatterplot(mydata$weights[mydata$Year>1980],mydata$Year[mydata$Year>1980])
#boh non noto correlazione
cor(mydata)
#same as before

#3.2) re for numeric-------------------

names="Rank~"
for(i in colnames(mydata[,3:7])){
  names=paste(names, "+ ","s(",i,",bs='re' )")
}

for(i in colnames(mydata[,8:ncol(mydata)])){
  names=paste(names, "+ ",i)
}

formula=as.formula(names)


model_sp2<-gam(formula,data = mydata)
model_sp2$type="gam"
summary(model_sp2)
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)               21767.67     363.93  59.812  < 2e-16 ***
#   Economic                    916.13     160.17   5.720 1.08e-08 ***
#   Negotiation                1550.72     218.66   7.092 1.36e-12 ***
#   Card.Game                  -537.67      87.08  -6.175 6.75e-10 ***
#   Fantasy                    -634.42     118.80  -5.340 9.37e-08 ***
#   Medieval                   -422.38     173.15  -2.439 0.014719 *  
#   Ancient                    -937.87     201.98  -4.643 3.45e-06 ***
#   Territory.Building         -677.01     241.90  -2.799 0.005134 ** 
#   Nautical                   -818.57     215.62  -3.796 0.000147 ***
#   Exploration                -432.71     196.48  -2.202 0.027653 *  
#   Farming                   -1469.40     346.96  -4.235 2.29e-05 ***
#   Bluffing                   -862.92     168.06  -5.135 2.85e-07 ***
#   Collectible.Components     1761.31     290.58   6.061 1.37e-09 ***
#   Miniatures                -1317.35     174.51  -7.549 4.57e-14 ***
#   City.Building             -1725.47     239.86  -7.194 6.51e-13 ***
#   Wargame                    1923.60     122.39  15.717  < 2e-16 ***
#   Adventure                   645.30     177.45   3.637 0.000277 ***
#   Renaissance               -1229.67     329.12  -3.736 0.000187 ***
#   Modern.Warfare             1073.07     282.22   3.802 0.000144 ***
#   Humor                       751.61     162.47   4.626 3.75e-06 ***
#   Electronic                 1524.24     378.36   4.029 5.63e-05 ***
#   Deduction                 -1044.49     171.29  -6.098 1.09e-09 ***
#   Word.Game                   -25.15     250.03  -0.101 0.919862    
#   Aviation...Flight         -1372.16     323.58  -4.241 2.24e-05 ***
#   Movies...TV...Radio.theme  1635.80     170.61   9.588  < 2e-16 ***
#   Memory                     1081.20     238.76   4.528 5.98e-06 ***
#   Puzzle                    -1347.49     212.04  -6.355 2.13e-10 ***
#   Real.time                 -1427.95     204.52  -6.982 3.00e-12 ***
#   Trivia                     2595.91     231.22  11.227  < 2e-16 ***
#   Industry...Manufacturing   -890.12     322.00  -2.764 0.005709 ** 
#   Age.of.Reason              -995.43     416.03  -2.393 0.016735 *  
#   Trains                    -1209.52     295.02  -4.100 4.15e-05 ***
#   Animals                   -1174.17     155.73  -7.540 4.89e-14 ***
#   X.Childrens.Game.          1489.72     155.68   9.569  < 2e-16 ***
#   Sports                      -85.73     241.56  -0.355 0.722663    
#   Action...Dexterity         -821.94     177.16  -4.640 3.51e-06 ***
#   Math                       1142.90     432.63   2.642 0.008254 ** 
#   Video.Game.Theme            538.19     286.57   1.878 0.060389 .  
#   Mature...Adult             2566.40     435.72   5.890 3.92e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df        F p-value    
#   s(Users_rated)  0.9991      1 1795.620  <2e-16 ***
#   s(Year)         0.9881      1   88.986  <2e-16 ***
#   s(weights)      1.0000      1 3725.504  <2e-16 ***
#   s(Playing_time) 0.6394      1    1.827  0.0959 .  
#   s(Min_age)      0.9901      1  135.384  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.309   Deviance explained =   31%
# GCV = 2.7707e+07  Scale est. = 2.7651e+07  n = 21461


R2adj=R_extr(model_sp2)
R2adj.perm=perm_sel(model_sp2)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#if the difference in terms of R^2adj is not so big we can delete those variables
#all true
#let's delete them:
to_delete_ind=which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE]))
names="Rank~"
for(i in colnames(mydata[,c(3:7)[!c(3:7)%in%to_delete_ind[which(to_delete_ind<=7)]]])){
  names=paste(names, "+ ","s(",i,",bs='re' )")
}

for(i in colnames(mydata[,c(-seq(1:7),-to_delete_ind[which(to_delete_ind>7)])])){
  names=paste(names, "+ ",i)
}


formula=as.formula(names)
model_sp2.1=gam(formula,data = mydata)
model_sp2.1$type="gam"
summary(model_sp2.1)
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               21223.71     360.97  58.796  < 2e-16 ***
#   Economic                    906.51     160.51   5.648 1.65e-08 ***
#   Negotiation                1493.54     219.15   6.815 9.66e-12 ***
#   Card.Game                  -560.87      87.07  -6.442 1.21e-10 ***
#   Fantasy                    -715.80     118.47  -6.042 1.55e-09 ***
#   Ancient                    -969.96     202.48  -4.790 1.68e-06 ***
#   Territory.Building         -767.93     242.07  -3.172 0.001514 ** 
#   Nautical                   -840.08     215.87  -3.892 9.99e-05 ***
#   Farming                   -1468.74     347.89  -4.222 2.43e-05 ***
#   Bluffing                   -897.11     168.46  -5.325 1.02e-07 ***
#   Collectible.Components     1888.07     291.13   6.485 9.05e-11 ***
#   Miniatures                -1368.93     174.87  -7.828 5.17e-15 ***
#   City.Building             -1771.75     240.04  -7.381 1.63e-13 ***
#   Wargame                    2020.67     121.50  16.631  < 2e-16 ***
#   Adventure                   485.40     167.95   2.890 0.003855 ** 
#   Renaissance               -1322.40     329.53  -4.013 6.02e-05 ***
#   Modern.Warfare             1086.00     282.93   3.838 0.000124 ***
#   Humor                       632.65     162.47   3.894 9.89e-05 ***
#   Electronic                 1544.37     379.40   4.071 4.71e-05 ***
#   Deduction                 -1119.71     171.46  -6.531 6.70e-11 ***
#   Aviation...Flight         -1369.12     324.42  -4.220 2.45e-05 ***
#   Movies...TV...Radio.theme  1601.05     170.75   9.376  < 2e-16 ***
#   Memory                     1156.12     239.35   4.830 1.37e-06 ***
#   Puzzle                    -1306.94     212.51  -6.150 7.89e-10 ***
#   Real.time                 -1438.07     204.64  -7.027 2.17e-12 ***
#   Trivia                     2428.18     230.72  10.524  < 2e-16 ***
#   Industry...Manufacturing   -939.10     322.82  -2.909 0.003629 ** 
#   Age.of.Reason              -917.21     417.14  -2.199 0.027904 *  
#   Trains                    -1089.39     295.37  -3.688 0.000226 ***
#   Animals                   -1093.25     155.79  -7.017 2.33e-12 ***
#   X.Childrens.Game.          1827.80     152.98  11.948  < 2e-16 ***
#   Action...Dexterity         -730.05     177.23  -4.119 3.82e-05 ***
#   Math                       1226.03     433.84   2.826 0.004718 ** 
#   Video.Game.Theme            502.04     287.31   1.747 0.080581 .  
#   Mature...Adult             1730.60     430.21   4.023 5.77e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#                     edf Ref.df       F p-value    
#   s(Users_rated)  0.9993      1 1803.77  <2e-16 ***
#   s(Year)         0.9907      1  106.31  <2e-16 ***
#   s(weights)      1.0000      1 3829.10  <2e-16 ***
#   s(Playing_time) 0.6805      1    2.16  0.0769 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.305   Deviance explained = 30.6%
# GCV = 2.7866e+07  Scale est. = 2.7816e+07  n = 21461



#3.2.1) model assessment--------------------------
gam.check(model_sp2.1)


# k'   edf k-index p-value    
# s(Users_rated)  1.000 0.999    0.54  <2e-16 ***
# s(Year)         1.000 0.991    0.95  <2e-16 ***
# s(weights)      1.000 1.000    0.81  <2e-16 ***
# s(Playing_time) 1.000 0.681    0.95  <2e-16 ***
#all BAD


concurvity(model_sp2.1,full=FALSE)$worst
# 

#                     para s(Users_rated)    s(Year) s(weights) s(Playing_time)
# para            1.00000000    0.053367318 0.98899216 0.86031714     0.027812914
# s(Users_rated)  0.05336732    1.000000000 0.05317235 0.06079448     0.001173565
# s(Year)         0.98899216    0.053172348 1.00000000 0.85384796     0.027798928
# s(weights)      0.86031714    0.060794484 0.85384796 1.00000000     0.050774638
# s(Playing_time) 0.02781291    0.001173565 0.02779893 0.05077464     1.000000000

#interactions
#weights vs year
# para... i'll first try to remove the other interactions

names="Rank~s(I(weights*Year))"
for(i in colnames(mydata[,c(3:7)[!c(3:7)%in%to_delete_ind[which(to_delete_ind<=7)]]])){
  names=paste(names, "+ ","s(",i,",bs='re' )")
}

for(i in colnames(mydata[,c(-seq(1:7),-to_delete_ind[which(to_delete_ind>7)])])){
  names=paste(names, "+ ",i)
}


formula=as.formula(names)
model_sp2.2=gam(formula,data = mydata)
model_sp2.2$type="gam"
summary(model_sp2.2)

#Parametric coefficients:
# Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)               18441.60    1716.31  10.745  < 2e-16 ***
#   Economic                    822.69     159.25   5.166 2.41e-07 ***
#   Negotiation                1537.29     217.31   7.074 1.55e-12 ***
#   Card.Game                  -587.47      86.43  -6.797 1.10e-11 ***
#   Fantasy                    -539.60     117.84  -4.579 4.69e-06 ***
#   Ancient                    -779.67     200.99  -3.879 0.000105 ***
#   Territory.Building         -619.97     240.21  -2.581 0.009859 ** 
#   Nautical                   -752.72     214.10  -3.516 0.000440 ***
#   Farming                   -1419.38     344.96  -4.115 3.89e-05 ***
#   Bluffing                   -931.28     167.09  -5.574 2.53e-08 ***
#   Collectible.Components     2121.65     288.99   7.341 2.19e-13 ***
#   Miniatures                -1401.23     173.71  -8.066 7.61e-16 ***
#   City.Building             -1571.26     238.34  -6.592 4.43e-11 ***
#   Wargame                    2027.36     120.85  16.775  < 2e-16 ***
#   Adventure                   548.78     166.55   3.295 0.000986 ***
#   Renaissance               -1265.98     327.04  -3.871 0.000109 ***
#   Modern.Warfare              996.25     280.55   3.551 0.000385 ***
#   Humor                       508.77     161.24   3.155 0.001605 ** 
#   Electronic                 1462.45     376.20   3.887 0.000102 ***
#   Deduction                 -1045.80     170.06  -6.150 7.90e-10 ***
#   Aviation...Flight         -1330.63     321.74  -4.136 3.55e-05 ***
#   Movies...TV...Radio.theme  1612.14     169.32   9.521  < 2e-16 ***
#   Memory                      998.98     237.46   4.207 2.60e-05 ***
#   Puzzle                    -1174.42     210.99  -5.566 2.63e-08 ***
#   Real.time                 -1619.51     203.27  -7.967 1.70e-15 ***
#   Trivia                     2105.57     229.45   9.176  < 2e-16 ***
#   Industry...Manufacturing  -1021.73     320.23  -3.191 0.001422 ** 
#   Age.of.Reason              -975.54     413.62  -2.359 0.018355 *  
#   Trains                    -1303.71     293.62  -4.440 9.04e-06 ***
#   Animals                   -1126.84     154.48  -7.294 3.11e-13 ***
#   X.Childrens.Game.          1270.64     156.13   8.139 4.21e-16 ***
#   Action...Dexterity        -1080.99     177.18  -6.101 1.07e-09 ***
#   Math                       1165.38     430.14   2.709 0.006748 ** 
#   Video.Game.Theme            538.34     284.88   1.890 0.058808 .  
#   Mature...Adult             1401.67     427.46   3.279 0.001043 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df       F  p-value    
#   s(I(weights * Year)) 8.6913992  8.968   42.40  < 2e-16 ***
#   s(Users_rated)       0.9992043  1.000 1802.01  < 2e-16 ***
#   s(Year)              0.8562941  1.000   11.12  0.00384 ** 
#   s(weights)           1.0000000  1.000   36.95 1.31e-06 ***
#   s(Playing_time)      0.0005467  1.000    0.00  0.41234    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.317   Deviance explained = 31.8%
# GCV = 2.7397e+07  Scale est. = 2.7338e+07  n = 21461

#let's remove Playing_time

names="Rank~s(I(weights*Year))"
for(i in colnames(mydata[,c(3:7)[!c(3:7)%in%c(to_delete_ind[which(to_delete_ind<=7)],6)]])){
  names=paste(names, "+ ","s(",i,",bs='re' )")
}
for(i in colnames(mydata[,c(-seq(1:7),-to_delete_ind[which(to_delete_ind>7)])])){
  names=paste(names, "+ ",i)
}


formula=as.formula(names)
model_sp2.3=gam(formula,data = mydata)
model_sp2.3$type="gam"
summary(model_sp2.3)

# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)               18340.75    1748.19  10.491  < 2e-16 ***
#   Economic                    822.25     159.25   5.163 2.45e-07 ***
#   Negotiation                1536.35     217.31   7.070 1.60e-12 ***
#   Card.Game                  -586.81      86.43  -6.789 1.16e-11 ***
#   Fantasy                    -539.61     117.84  -4.579 4.69e-06 ***
#   Ancient                    -779.11     201.00  -3.876 0.000106 ***
#   Territory.Building         -619.47     240.21  -2.579 0.009919 ** 
#   Nautical                   -753.04     214.10  -3.517 0.000437 ***
#   Farming                   -1418.93     344.96  -4.113 3.92e-05 ***
#   Bluffing                   -931.80     167.09  -5.577 2.48e-08 ***
#   Collectible.Components     2120.64     289.00   7.338 2.25e-13 ***
#   Miniatures                -1401.34     173.71  -8.067 7.58e-16 ***
#   City.Building             -1570.19     238.34  -6.588 4.56e-11 ***
#   Wargame                    2025.48     120.85  16.760  < 2e-16 ***
#   Adventure                   548.55     166.55   3.294 0.000991 ***
#   Renaissance               -1265.37     327.05  -3.869 0.000110 ***
#   Modern.Warfare              996.06     280.55   3.550 0.000386 ***
#   Humor                       509.97     161.24   3.163 0.001564 ** 
#   Electronic                 1462.52     376.20   3.888 0.000102 ***
#   Deduction                 -1044.97     170.06  -6.145 8.15e-10 ***
#   Aviation...Flight         -1330.27     321.74  -4.135 3.57e-05 ***
#   Movies...TV...Radio.theme  1612.77     169.32   9.525  < 2e-16 ***
#   Memory                      998.81     237.46   4.206 2.61e-05 ***
#   Puzzle                    -1174.62     210.99  -5.567 2.62e-08 ***
#   Real.time                 -1617.26     203.28  -7.956 1.87e-15 ***
#   Trivia                     2106.50     229.45   9.181  < 2e-16 ***
#   Industry...Manufacturing  -1021.13     320.24  -3.189 0.001431 ** 
#   Age.of.Reason              -975.79     413.62  -2.359 0.018326 *  
#   Trains                    -1302.66     293.63  -4.436 9.19e-06 ***
#   Animals                   -1125.55     154.48  -7.286 3.31e-13 ***
#   X.Childrens.Game.          1274.32     156.13   8.162 3.47e-16 ***
#   Action...Dexterity        -1077.88     177.18  -6.083 1.20e-09 ***
#   Math                       1165.39     430.14   2.709 0.006748 ** 
#   Video.Game.Theme            538.96     284.88   1.892 0.058522 .  
#   Mature...Adult             1405.18     427.47   3.287 0.001013 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df       F  p-value    
#   s(I(weights * Year)) 8.8018  8.987   42.66  < 2e-16 ***
#   s(Users_rated)       0.9998  1.000 1802.86  < 2e-16 ***
#   s(Year)              0.9712  1.000   10.87  0.00786 ** 
#   s(weights)           0.9243  1.000   35.93 7.74e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.317   Deviance explained = 31.8%
# GCV = 2.7397e+07  Scale est. = 2.7338e+07  n = 21461




gam.check(model_sp2.3)

# k'   edf k-index p-value    
# s(I(weights * Year)) 9.000 8.802    0.94  <2e-16 ***
# s(Users_rated)       1.000 1.000    0.55  <2e-16 ***
# s(Year)              1.000 0.971    0.93  <2e-16 ***
# s(weights)           1.000 0.924    0.85  <2e-16 ***
#ALL BAD

concurvity(model_sp2.3,full=FALSE)$worst
#                       para        s(I(weights * Year)) s(Users_rated)   s(Year)   s(weights)
# para                 1.000000e+00    1.069680e-28     0.053367318     0.988992161  0.86031714
# s(I(weights * Year)) 1.381165e-28    1.000000e+00     0.009354728     0.008949032  0.13788133
# s(Users_rated)       5.336732e-02    9.354728e-03     1.000000000     0.053172348  0.06079448
# s(Year)              9.889922e-01    8.949032e-03     0.053172348     1.000000000  0.85384796
# s(weights)           8.603171e-01    1.378813e-01     0.060794484     0.853847956  1.00000000

#interactions still.
#weights vs year
# para...year and weights

ypred <- predict(model_sp2.3, newdata=xnew)
temp=sample(21461,10000)
dev.new()
plot(ypred[temp],mydata$Rank[temp],main="Response vs. Fitted Values",xlab="Fitted Values",ylab="Response")
lines(seq(0,21461,0.5),seq(0,21461,0.5),col="red")

dev.new()
plot(ypred[ypred>0],mydata$Rank[ypred>0],main="Response vs. Fitted Values",xlab="Fitted Values",ylab="Response")
lines(seq(0,21461,0.5),seq(0,21461,0.5),col="red")


#4)comparing models (to check)--------------------------------------
#do we really need a nonparametri contribution for the categories or we can use semiparametric models?
#let's compare models in 2.3 (model_ns2.1) and 3.1 (model_sp1.1)
#                 and in 1 (model_re1)  and 3.2  (model_sp2.1)

hist(model_ns2.1$residuals)
qqnorm(model_ns2.1$residuals)
#normal!

hist(model_sp1.2$residuals)
qqnorm(model_sp1.2$residuals)
# normal too ==> i can use parametric test(can I?):
anova(model_sp1.2,model_ns2.1, test = "F")
#pvalue= boh

hist(model_re1$residuals) #normal-ish
qqnorm(model_re1$residuals) 
hist(model_sp2.1$residuals)
qqnorm(model_sp2.1$residuals)
# normal-ish ==> i can use parametric test(can I?):
anova(model_sp2.1,model_re1, test = "F")
#pvalue=0.0336 *==> i modelli in realtà sono diversi, meglio usare quello più complesso

#so let's keep sp1.2 and re1

#test non parametrici
#coming soon


#3.3)Thin plate regression splines+Tensor product smooths ----------
names="Rank~"
for(i in colnames(mydata[,3:7])){
  names=paste(names, "+ ","te(",i,",bs='tp',k=5)" )
}

for(i in colnames(mydata[,8:ncol(mydata)])){
  names=paste(names, "+ ",i)
}

formula=as.formula(names)
model_ts=gam(formula,data = mydata)
model_ts$type="gam"
summary(model_ts)
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)               10790.67      63.09 171.048  < 2e-16 ***
#   Economic                    975.51     141.87   6.876 6.32e-12 ***
#   Negotiation                1466.12     193.54   7.575 3.73e-14 ***
#   Card.Game                  -398.61      77.41  -5.150 2.63e-07 ***
#   Fantasy                    -233.57     105.57  -2.212 0.026950 *  
#   Medieval                    -41.11     153.46  -0.268 0.788783    
#   Ancient                    -407.04     178.97  -2.274 0.022955 *  
#   Territory.Building         -304.35     214.19  -1.421 0.155355    
#   Nautical                   -328.36     190.93  -1.720 0.085491 .  
#   Exploration                 118.65     173.95   0.682 0.495189    
#   Farming                    -905.40     307.08  -2.948 0.003198 ** 
#   Bluffing                   -779.59     148.79  -5.240 1.62e-07 ***
#   Collectible.Components     1549.21     257.53   6.016 1.82e-09 ***
#   Miniatures                -1017.20     155.40  -6.545 6.06e-11 ***
#   City.Building              -748.08     212.74  -3.516 0.000438 ***
#   Wargame                     913.59     114.07   8.009 1.21e-15 ***
#   Adventure                   675.63     157.09   4.301 1.71e-05 ***
#   Renaissance                -540.56     291.56  -1.854 0.063747 .  
#   Modern.Warfare              751.21     249.70   3.008 0.002629 ** 
#   Humor                       725.17     144.52   5.018 5.27e-07 ***
#   Electronic                 1343.29     334.74   4.013 6.02e-05 ***
#   Deduction                  -670.69     151.75  -4.420 9.94e-06 ***
#   Word.Game                  -332.97     221.64  -1.502 0.133038    
#   Aviation...Flight         -1186.72     286.44  -4.143 3.44e-05 ***
#   Movies...TV...Radio.theme  1546.97     151.09  10.239  < 2e-16 ***
#   Memory                      544.76     211.60   2.574 0.010046 *  
#   Puzzle                     -926.45     188.01  -4.928 8.39e-07 ***
#   Real.time                 -1069.57     181.43  -5.895 3.80e-09 ***
#   Trivia                     2021.98     206.50   9.792  < 2e-16 ***
#   Industry...Manufacturing   -446.20     285.20  -1.565 0.117711    
#   Age.of.Reason              -714.00     368.12  -1.940 0.052444 .  
#   Trains                    -1412.59     261.55  -5.401 6.70e-08 ***
#   Animals                    -815.12     138.21  -5.898 3.74e-09 ***
#   X.Childrens.Game.           893.15     149.06   5.992 2.11e-09 ***
#   Sports                     -443.93     214.11  -2.073 0.038145 *  
#   Action...Dexterity        -1023.67     158.34  -6.465 1.03e-10 ***
#   Math                        739.78     382.83   1.932 0.053321 .  
#   Video.Game.Theme            870.07     253.59   3.431 0.000602 ***
#   Mature...Adult             1449.33     424.29   3.416 0.000637 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df        F  p-value    
#   te(Users_rated)  3.998  4.000 1806.641  < 2e-16 ***
#   te(Year)         3.991  4.000  208.431  < 2e-16 ***
#   te(weights)      3.525  3.868  592.797  < 2e-16 ***
#   te(Playing_time) 3.790  3.968    6.491 3.52e-05 ***
#   te(Min_age)      3.561  3.857   10.146  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.459   Deviance explained = 46.1%
# GCV = 2.1688e+07  Scale est. = 2.163e+07  n = 21461

R2adj=R_extr(model_ts)
R2adj.perm=perm_sel(model_ts)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres) #if the difference in terms of R^2adj is not so big we can delete those variables
#all true
#let's delete them:
to_delete_ind=which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE]))
#all>7
names="Rank~"
for(i in colnames(mydata[,3:7])){
  names=paste(names, "+ ","te(",i,",bs='tp',k=5)" )
}

for(i in colnames(mydata[,c(-seq(1:7),-to_delete_ind)])){
  names=paste(names, "+ ",i)
}

formula=as.formula(names)
model_ts1=gam(formula,data = mydata)
model_ts1$type="gam"
summary(model_ts1)

# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               10761.75      60.98 176.491  < 2e-16 ***
#   Economic                    934.18     138.77   6.732 1.72e-11 ***
#   Negotiation                1476.13     193.29   7.637 2.32e-14 ***
#   Card.Game                  -387.61      77.15  -5.024 5.10e-07 ***
#   Fantasy                    -223.26     105.17  -2.123 0.033778 *  
#   Ancient                    -402.94     178.75  -2.254 0.024197 *  
#   Nautical                   -315.93     190.62  -1.657 0.097458 .  
#   Farming                    -946.29     306.39  -3.089 0.002014 ** 
#   Bluffing                   -778.28     148.72  -5.233 1.68e-07 ***
#   Collectible.Components     1556.51     257.45   6.046 1.51e-09 ***
#   Miniatures                -1002.85     155.26  -6.459 1.08e-10 ***
#   City.Building              -764.16     211.92  -3.606 0.000312 ***
#   Wargame                     931.18     113.21   8.225  < 2e-16 ***
#   Adventure                   724.91     148.39   4.885 1.04e-06 ***
#   Renaissance                -530.03     291.12  -1.821 0.068676 .  
#   Modern.Warfare              759.08     249.60   3.041 0.002359 ** 
#   Humor                       722.57     144.50   5.001 5.76e-07 ***
#   Electronic                 1353.23     334.67   4.043 5.28e-05 ***
#   Deduction                  -665.47     151.64  -4.389 1.15e-05 ***
#   Aviation...Flight         -1183.38     286.34  -4.133 3.60e-05 ***
#   Movies...TV...Radio.theme  1557.14     150.88  10.320  < 2e-16 ***
#   Memory                      547.31     211.59   2.587 0.009699 ** 
#   Puzzle                     -924.30     188.01  -4.916 8.88e-07 ***
#   Real.time                 -1080.65     181.05  -5.969 2.43e-09 ***
#   Trivia                     2010.78     206.14   9.754  < 2e-16 ***
#   Age.of.Reason              -704.95     368.04  -1.915 0.055450 .  
#   Trains                    -1381.67     260.90  -5.296 1.20e-07 ***
#   Animals                    -803.49     137.96  -5.824 5.83e-09 ***
#   X.Childrens.Game.           896.36     148.74   6.027 1.70e-09 ***
#   Sports                     -427.57     213.86  -1.999 0.045587 *  
#   Action...Dexterity        -1012.97     158.13  -6.406 1.53e-10 ***
#   Math                        712.36     382.40   1.863 0.062496 .  
#   Video.Game.Theme            877.87     253.52   3.463 0.000536 ***
#   Mature...Adult             1469.61     423.80   3.468 0.000526 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df       F  p-value    
#   te(Users_rated)  3.998  4.000 1817.12  < 2e-16 ***
#   te(Year)         3.991  4.000  209.59  < 2e-16 ***
#   te(weights)      3.518  3.864  608.09  < 2e-16 ***
#   te(Playing_time) 3.784  3.966    6.28 5.25e-05 ***
#   te(Min_age)      3.509  3.824   10.39  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.459   Deviance explained = 46.1%
# GCV = 2.1685e+07  Scale est. = 2.1632e+07  n = 21461


#3.3.1) model assessment----------------------------------
gam.check(model_ts1)

#   te(Users_rated)  4.00 4.00    0.64  <2e-16 ***
#   te(Year)         4.00 3.99    0.94  <2e-16 ***
#   te(weights)      4.00 3.52    0.93  <2e-16 ***
#   te(Playing_time) 4.00 3.78    1.00   0.370    
#   te(Min_age)      4.00 3.51    0.97   0.025 *  
#bad

concurvity(model_ts1,full=FALSE)$worst
#                         para te(Users_rated)     te(Year)  te(weights) te(Playing_time)  te(Min_age)
# para             1.000000e+00    2.899523e-28 9.184196e-17 6.187532e-29     1.442378e-20 5.965277e-19
# te(Users_rated)  2.780970e-28    1.000000e+00 1.608132e-03 2.031394e-02     9.490836e-05 1.792023e-02
# te(Year)         9.184197e-17    1.608132e-03 1.000000e+00 2.323390e-03     2.236708e-03 1.331270e-02
# te(weights)      6.232286e-29    2.031394e-02 2.323390e-03 1.000000e+00     2.599076e-01 3.617748e-01
# te(Playing_time) 1.442562e-20    9.490836e-05 2.236708e-03 2.599076e-01     1.000000e+00 7.238612e-02
# te(Min_age)      5.965279e-19    1.792023e-02 1.331270e-02 3.617748e-01     7.238612e-02 1.000000e+00
# 
#good
#let's increase k
names="Rank~"
for(i in colnames(mydata[,3:7])){
  names=paste(names, "+ ","te(",i,",bs='tp',k=7)" )
}

for(i in colnames(mydata[,c(-seq(1:7),-to_delete_ind)])){
  names=paste(names, "+ ",i)
}

formula=as.formula(names)
model_ts2=gam(formula,data = mydata)
model_ts2$type="gam"
summary(model_ts2)



# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               10824.23      57.39 188.613  < 2e-16 ***
#   Economic                    892.71     129.81   6.877 6.28e-12 ***
#   Negotiation                1564.81     180.78   8.656  < 2e-16 ***
#   Card.Game                  -312.82      72.20  -4.333 1.48e-05 ***
#   Fantasy                     -92.03      98.34  -0.936 0.349394    
#   Ancient                    -191.85     167.13  -1.148 0.251010    
#   Nautical                   -220.10     178.18  -1.235 0.216741    
#   Farming                    -747.91     286.40  -2.611 0.009024 ** 
#   Bluffing                   -628.43     139.06  -4.519 6.24e-06 ***
#   Collectible.Components     1591.06     240.72   6.610 3.95e-11 ***
#   Miniatures                 -841.87     145.33  -5.793 7.01e-09 ***
#   City.Building              -398.49     198.21  -2.010 0.044401 *  
#   Wargame                     352.67     107.75   3.273 0.001066 ** 
#   Adventure                   668.30     138.75   4.816 1.47e-06 ***
#   Renaissance                -455.06     272.18  -1.672 0.094554 .  
#   Modern.Warfare              642.80     233.29   2.755 0.005868 ** 
#   Humor                       757.98     135.12   5.610 2.05e-08 ***
#   Electronic                 1205.70     312.85   3.854 0.000117 ***
#   Deduction                  -561.00     141.76  -3.957 7.60e-05 ***
#   Aviation...Flight         -1097.46     267.64  -4.100 4.14e-05 ***
#   Movies...TV...Radio.theme  1487.58     141.12  10.541  < 2e-16 ***
#   Memory                      351.05     198.03   1.773 0.076285 .  
#   Puzzle                     -863.48     175.74  -4.913 9.02e-07 ***
#   Real.time                  -909.19     169.30  -5.370 7.94e-08 ***
#   Trivia                     1851.16     192.92   9.596  < 2e-16 ***
#   Age.of.Reason              -450.78     343.97  -1.311 0.190030    
#   Trains                    -1382.28     244.00  -5.665 1.49e-08 ***
#   Animals                    -627.37     129.05  -4.862 1.17e-06 ***
#   X.Childrens.Game.           410.56     159.01   2.582 0.009828 ** 
#   Sports                     -788.68     200.17  -3.940 8.17e-05 ***
#   Action...Dexterity        -1015.71     148.41  -6.844 7.92e-12 ***
#   Math                        603.45     357.45   1.688 0.091385 .  
#   Video.Game.Theme            892.05     236.96   3.765 0.000167 ***
#   Mature...Adult             1302.96     410.73   3.172 0.001514 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#   Approximate significance of smooth terms:
#   edf Ref.df        F p-value    
#   te(Users_rated)  5.997  6.000 1829.910 < 2e-16 ***
#   te(Year)         5.946  5.999  205.443 < 2e-16 ***
#   te(weights)      5.567  5.918  373.289 < 2e-16 ***
#   te(Playing_time) 4.232  4.898    6.552 5.7e-06 ***
#   te(Min_age)      5.675  5.929   10.522 < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.528   Deviance explained = 52.9%
# GCV = 1.8944e+07  Scale est. = 1.8889e+07  n = 21461

gam.check(model_ts2)



#                     k'  edf k-index p-value    
# te(Users_rated)  6.00 6.00    0.69  <2e-16 ***
# te(Year)         6.00 5.95    0.95  <2e-16 ***
# te(weights)      6.00 5.57    0.92  <2e-16 ***
# te(Playing_time) 6.00 4.23    0.98    0.10    
# te(Min_age)      6.00 5.68    1.00    0.43    

R2adj=R_extr(model_ts2)
R2adj.perm=perm_sel(model_ts2)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#if the difference in terms of R^2adj is not so big we can delete those variables
#all true
#let's delete them:
to_delete_ind=c(to_delete_ind,which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE]))
)


names="Rank~"
for(i in colnames(mydata[,3:7])){
  names=paste(names, "+ ","te(",i,",bs='tp',k=7)" )
}

for(i in colnames(mydata[,c(-seq(1:7),-to_delete_ind)])){
  names=paste(names, "+ ",i)
}

formula=as.formula(names)
model_ts3=gam(formula,data = mydata)
model_ts3$type="gam"
summary(model_ts3)


#now other vars are not sign


R2adj=R_extr(model_ts3)
R2adj.perm=perm_sel(model_ts3)
thres=0.05
to_delete_perm=(abs(R2adj-R2adj.perm)<=thres)
#if the difference in terms of R^2adj is not so big we can delete those variables
#all true
#let's delete them:
to_delete_ind=c(to_delete_ind,which(names(mydata) %in% names(to_delete_perm[to_delete_perm=TRUE]))
)


names="Rank~"
for(i in colnames(mydata[,3:7])){
  names=paste(names, "+ ","te(",i,",bs='tp',k=7)" )
}

for(i in colnames(mydata[,c(-seq(1:7),-to_delete_ind)])){
  names=paste(names, "+ ",i)
}

formula=as.formula(names)
model_ts4=gam(formula,data = mydata)
model_ts4$type="gam"
summary(model_ts4)

# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               10815.01      55.96 193.279  < 2e-16 ***
#   Economic                    888.97     129.42   6.869 6.65e-12 ***
#   Negotiation                1559.64     180.75   8.629  < 2e-16 ***
#   Card.Game                  -320.71      71.99  -4.455 8.44e-06 ***
#   Farming                    -743.21     286.30  -2.596 0.009442 ** 
#   Bluffing                   -635.24     139.08  -4.568 4.96e-06 ***
#   Collectible.Components     1583.20     240.18   6.592 4.45e-11 ***
#   Miniatures                 -847.29     144.38  -5.868 4.46e-09 ***
#   City.Building              -411.60     197.92  -2.080 0.037570 *  
#   Wargame                     325.70     106.50   3.058 0.002229 ** 
#   Adventure                   628.27     135.22   4.646 3.40e-06 ***
#   Renaissance                -471.21     271.76  -1.734 0.082940 .  
#   Modern.Warfare              666.83     232.54   2.868 0.004141 ** 
#   Humor                       764.13     135.08   5.657 1.56e-08 ***
#   Electronic                 1205.66     312.82   3.854 0.000116 ***
#   Deduction                  -544.39     141.56  -3.846 0.000121 ***
#   Word.Game                  -459.48     207.13  -2.218 0.026546 *  
#   Aviation...Flight         -1081.23     267.26  -4.046 5.24e-05 ***
#   Movies...TV...Radio.theme  1488.81     141.08  10.553  < 2e-16 ***
#   Memory                      346.33     198.00   1.749 0.080285 .  
#   Puzzle                     -855.15     175.65  -4.869 1.13e-06 ***
#   Real.time                  -877.75     169.57  -5.176 2.28e-07 ***
#   Trivia                     1884.87     193.05   9.764  < 2e-16 ***
#   Trains                    -1358.86     243.49  -5.581 2.42e-08 ***
#   Animals                    -630.20     128.98  -4.886 1.04e-06 ***
#   X.Childrens.Game.           395.45     158.87   2.489 0.012811 *  
#   Sports                     -787.59     200.06  -3.937 8.29e-05 ***
#   Action...Dexterity        -1026.39     148.54  -6.910 4.98e-12 ***
#   Math                        606.13     357.41   1.696 0.089921 .  
#   Video.Game.Theme            887.75     236.66   3.751 0.000176 ***
#   Mature...Adult             1295.37     410.77   3.153 0.001616 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df        F  p-value    
# te(Users_rated)  5.997  6.000 1840.430  < 2e-16 ***
#   te(Year)         5.946  5.999  207.902  < 2e-16 ***
#   te(weights)      5.562  5.916  383.368  < 2e-16 ***
#   te(Playing_time) 4.222  4.888    6.447 7.35e-06 ***
#   te(Min_age)      5.687  5.934   10.473  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.528   Deviance explained = 52.9%
# GCV = 1.8938e+07  Scale est. = 1.8887e+07  n = 21461
gam.check(model_ts4)

#   te(Users_rated)  6.00 6.00    0.71  <2e-16 ***
#   te(Year)         6.00 5.95    0.93  <2e-16 ***
#   te(weights)      6.00 5.56    0.94  <2e-16 ***
#   te(Playing_time) 6.00 4.22    0.99    0.32    
#   te(Min_age)      6.00 5.69    0.98    0.16   
concurvity(model_ts4,full=FALSE)$worst
#ok

plot(model_ts4)


ypred <- predict(model_ts4, newdata=xnew)
temp=sample(21461,10000)
dev.new()
plot(ypred[temp],mydata$Rank[temp],main="Response vs. Fitted Values",xlab="Fitted Values",ylab="Response")
lines(seq(0,21461,0.5),seq(0,21461,0.5),col="red")

dev.new()
plot(ypred[ypred>0][temp],mydata$Rank[ypred>0][temp],main="Response vs. Fitted Values",xlab="Fitted Values",ylab="Response")
lines(seq(0,21461,0.5),seq(0,21461,0.5),col="red")











