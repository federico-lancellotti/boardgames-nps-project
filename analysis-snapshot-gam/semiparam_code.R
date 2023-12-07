# To do:
#   (-qualche grafico a caso del dataset (?) [NB: per ora gli altri grafici non sono in palette (; ] --> magari se vogliamo far vedere i problemi con gli outliers)
#   (-modelli semiparametrici: esiste un test NP per selezionare le variabili in un modello come il nostro? (ho fatto qualche prova, ma per ora nulla di sodisfacente) --> Silvia)
#   -modelli semiparametrici: manca la predizione --> ~20 giochi da kickstarter
#   (-modelli semiparametrici: manca  Ftest per confronto tra i modelli --> da fare quando abbiamo abbastanza modelli)
#   -modello tp: ma il grafico??


library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(DepthProc)
library(gam)
library(ggplot2)
library(gratia)




load("C:/Users/Utente/Desktop/NPS/Projects/workspace.RData")


#Consider only cols with Ratings, Playing_time, Mean_Payers and all the selected categories and deal with NAs:
boardgames_gam=boardgames_selcat[, -c(2,4,6,7,8,10:21)]
View(boardgames_gam)
dim(boardgames_gam) #21592    66
any(is.na(boardgames_gam)) #TRUE --> I remove these lines:
boardgames_gam=na.omit(boardgames_gam)
any(is.na(boardgames_gam)) #FALSE --> no NAs :)
dim(boardgames_gam) #21461    66



#Outliers detection----
#Mean_players:
plot(boardgames_gam$Mean_players, xlab = "Games index", ylab = "Mean number of players", main = "Plot Mean_players variable")
abline(h=50, col="red", lwd = 2)

# plot(boardgames_gam$Playing_time, xlab = "Games index", ylab = "Playing time (min)", main = "Plot Playing_time variable")
# abline(h=1000, col="red", lwd = 2)

boxplot(boardgames_gam$Mean_players, col='#e6e6fa')


# #Remove outliers using the IQR method: --> questo mi sembra troppo penalizzante
# Q1=quantile(boardgames_gam$Mean_players, 0.25)
# Q3=quantile(boardgames_gam$Mean_players, 0.75)
# IQR=Q3 - Q1
# inf=Q1 - 1.5 * IQR
# sup=Q3 + 1.5 * IQR
# boardgames_gam=boardgames_gam[boardgames_gam$Mean_players>=inf & boardgames_gam$Mean_players<=sup, ]
# dim(boardgames_gam) #20935    66 


#Remove outliers using Z-score:
z_scores=scale(boardgames_gam$Mean_players)
threshold=3
boardgames_gam=boardgames_gam[abs(z_scores) <= threshold, ]
dim(boardgames_gam) #21277    66


plot(boardgames_gam$Mean_players, xlab = "Games index", ylab = "Mean number of players", main = "Plot Mean_players variable")

boxplot(boardgames_gam$Mean_players, col='#e6e6fa')



#Playing_time:
plot(boardgames_gam$Playing_time, col='#99CCFF')
abline(h=1000, col='#6495ed')

boxplot(boardgames_gam$Playing_time, col='#e6e6fa', ylim=c(0,2000))

# #Remove outliers using the IQR method: --> questo mi sembra troppo penalizzante
# Q1=quantile(boardgames_gam$Playing_time, 0.25)
# Q3=quantile(boardgames_gam$Playing_time, 0.75)
# IQR=Q3 - Q1
# inf=Q1 - 1.5 * IQR
# sup=Q3 + 1.5 * IQR
# boardgames_gam=boardgames_gam[boardgames_gam$Playing_time>=inf & boardgames_gam$Playing_time<=sup, ]
# dim(boardgames_gam) #19529    66 --> perdo circa il 10%


#Remove outliers using Z-score:
z_scores=scale(boardgames_gam$Playing_time)
threshold=3
boardgames_gam=boardgames_gam[abs(z_scores) <= threshold, ]
dim(boardgames_gam) #21212    66


plot(boardgames_gam$Playing_time, xlab = "Games index", ylab = "Playing time (min)", main = "Plot Playing_time variable")

boxplot(boardgames_gam$Playing_time, col='#e6e6fa')




#Weights:
plot(boardgames_gam$weights, col='#99CCFF')








##SEMIPARAMETRIC MODEL----
#use smooth only for quantitative variables, keep categories as linear terms:
install.packages("mgcv")
library(mgcv)
##WITH CUBIC SPLINES----
semiparam=gam(Ratings ~ s(Playing_time,bs='cr') + s(Mean_players,bs='cr') + s(weights,bs='cr') + Economic + Negotiation + Political + 
                Card_Game + Fantasy + Abstract_Strategy + Medieval + Ancient + Territory_Building + 
                Civilization + Nautical + Exploration + Farming + Mythology + Bluffing +
                Science_Fiction + Dice + Fighting + Print_and_Play + Maze + Miniatures +
                Racing + City_Building + Wargame + Adventure + Space_Exploration + Renaissance + 
                Humor + Electronic + Horror + Novel_based + Word_Game + 
                Aviation_Flight + Movies_TV_Radio_theme + Party_Game + Memory + Puzzle + Real_time + 
                Trivia + Industry_Manufacturing + Age_of_Reason + Trains + Animals + Childrens_Game + 
                Pirates + Transportation + Action_Dexterity + Spies_Secret_Agents + Educational + 
                Napoleonic + Post_Napoleonic + Math + Book + Music + Environmental + 
                Number + Pike_and_Shot + Video_Game_Theme + Mature_Adult + Expansion_for_Base_game,data = boardgames_gam)
summary(semiparam) #R-sq.(adj) =  0.358

#Remove non-sign variables (just looking at the signif; level=10%): --> ESISTE UN METODO NP PER UN MODELLO COME QUESTO?? --> CERCA!!!!
semiparam2=gam(Ratings ~ s(Playing_time,bs='cr') + s(Mean_players,bs='cr') + s(weights,bs='cr') + Economic + Negotiation + 
                Card_Game + Fantasy + Abstract_Strategy + Farming + Mythology + Bluffing +
                Science_Fiction + Dice + Print_and_Play + Miniatures +
                City_Building + Wargame + Space_Exploration + 
                Humor + Electronic + Novel_based + Movies_TV_Radio_theme + Party_Game + Puzzle + Real_time + 
                Trivia + Age_of_Reason + Trains + Animals + Childrens_Game + 
                Action_Dexterity + 
                Napoleonic + Math + Book + Mature_Adult,data = boardgames_gam)
summary(semiparam2) #R-sq.(adj) =  0.358
#ho fatto un controllo con la numerosità per ogni categoria, le cat che influenzano negativamente sul modello sono sia tra quelle più numerose sia no (magari nel primo caso si ha troppa concorrenza, mentre nel secondo non piacciono e basta)


#Check the residuals:
hist(semiparam2$residuals)#no sym --> no N
qqnorm(semiparam2$residuals) 
qqline(semiparam2$residuals) #bad for both the 2 tails, especially for the left one --> no N

ks.test(semiparam2$residuals, "pnorm") #p-value < 2.2e-16 --> I cannot accept H0 --> no N

mu.res <- mean(semiparam2$residuals) 
s.res <- sd(semiparam2$residuals)
x <- as.matrix(semiparam2$residuals) #note that here we need to use as.matrix in order to use the ddplot command below
y <- as.matrix( rnorm(length(semiparam2$residuals), mu.res, sqrt(s.res)) )
DepthProc::ddPlot(x, y,
                  depth_params = list(method='Tukey')) #Not so correlated --> no N


##SAREBBE IL CASO DI ELIMINARE DEGLI OUTLIERS
#Plot (look at the contribution of a single predictor holding all the others fixed): 
plot(semiparam2)

# Playing_time: tra 5 e 15 giocatori il gioco tende ad essere più popolare
plot(semiparam2, xlab = "Playing time (min)", ylab = " Partial effect", main = "Partial effect of Playing_time on Ratings")

# Mean_players: tra ~ 250 e 500 min la risposta ha un'impennata
plot(semiparam2, xlab = "Mean number of players", ylab = "Partial effect", main = "Partial effect of Mean_players on Ratings")

# weights: as weights increases, the same does the response (by response now I mean the response keeping the effects of other variables fixed) --> più un gioco è difficile, più ci piace
plot(semiparam2, xlab = "Complexity or Weights of the game", ylab = "Partial effect", main = "Partial effect of Weights on Ratings")


library(gratia)
draw(semiparam2, residuals=TRUE)



#Prediction:
# #Create he grid for the predictions: #DA SISTEMARE!!!!
# Playing_time.grid=seq(range(boardgames_gam$Playing_time)[1],range(boardgames_gam$Playing_time)[2],length.out = 100) #length.out = 100-->I will take 100 points in the grid
# Mean_players.grid=seq(range(boardgames_gam$Mean_players)[1],range(boardgames_gam$Mean_players)[2],length.out = 100)
# grid=expand.grid(Playing_time.grid,Mean_players.grid)
# names(grid)=c('Playing_time','Mean_players')
# 
# pred_semiparam2=predict(semiparam2,newdata=grid)

# persp3d(education.grid,income.grid,pred_gam,col='#e6e6fa') #da modificare
# with(boardgames_gam,points3d(education,income,prestige,col='#6495ed',size=5)) #da modificare








##WITH B SPLINES----
semiparam3=gam(Ratings ~ s(Playing_time,bs='bs') + s(Mean_players,bs='bs') + s(weights,bs='bs') + Economic + Negotiation + Political + 
                Card_Game + Fantasy + Abstract_Strategy + Medieval + Ancient + Territory_Building + 
                Civilization + Nautical + Exploration + Farming + Mythology + Bluffing +
                Science_Fiction + Dice + Fighting + Print_and_Play + Maze + Miniatures +
                Racing + City_Building + Wargame + Adventure + Space_Exploration + Renaissance + 
                Humor + Electronic + Horror + Novel_based + Word_Game + 
                Aviation_Flight + Movies_TV_Radio_theme + Party_Game + Memory + Puzzle + Real_time + 
                Trivia + Industry_Manufacturing + Age_of_Reason + Trains + Animals + Childrens_Game + 
                Pirates + Transportation + Action_Dexterity + Spies_Secret_Agents + Educational + 
                Napoleonic + Post_Napoleonic + Math + Book + Music + Environmental + 
                Number + Pike_and_Shot + Video_Game_Theme + Mature_Adult + Expansion_for_Base_game,data = boardgames_gam)
summary(semiparam3) #R-sq.(adj) =  0.356

#Remove non-sign variables (just looking at the signif; level=10%):
semiparam4=gam(Ratings ~ s(Playing_time,bs='bs') + s(Mean_players,bs='bs') + s(weights,bs='bs') + Economic + Negotiation + 
                 Card_Game + Fantasy + Abstract_Strategy + Farming + Mythology + Bluffing +
                 Science_Fiction + Dice + Print_and_Play + Miniatures +
                 City_Building + Wargame + Space_Exploration + 
                 Humor + Electronic + Novel_based + Movies_TV_Radio_theme + Party_Game + Puzzle + Real_time + 
                 Trivia + Age_of_Reason + Trains + Animals + Childrens_Game + 
                 Action_Dexterity + 
                 Napoleonic + Math + Book + Mature_Adult,data = boardgames_gam)
summary(semiparam4) #R-sq.(adj) =  0.356


#Check the residuals:
hist(semiparam4$residuals)#no sym --> no N
qqnorm(semiparam4$residuals) 
qqline(semiparam4$residuals) #bad for both the 2 tails, especially for the left one --> no N

ks.test(semiparam4$residuals, "pnorm") #p-value < 2.2e-16 --> I cannot accept H0 --> no N

mu.res <- mean(semiparam4$residuals) 
s.res <- sd(semiparam4$residuals)
x <- as.matrix(semiparam4$residuals) #note that here we need to use as.matrix in order to use the ddplot command below
y <- as.matrix( rnorm(length(semiparam4$residuals), mu.res, sqrt(s.res)) )
DepthProc::ddPlot(x, y,
                  depth_params = list(method='Tukey')) #Not so correlated --> no N

#Plot (look at the contribution of a single predictor holding all the others fixed): Plot semiparam2 Playing_time post
plot(semiparam4) 
# più o meno lo stesso di pirma
# Playing_time: tra 5 e 15 giocatori il gioco tende ad essere più popolare
plot(semiparam4, xlab = "Playing time (min)", ylab = "Partial effect", main = "Partial effect of Playing_time on Ratings")

# Mean_players: tra ~ 250 e 500 min la risposta ha un'impennata
plot(semiparam4, xlab = "Mean number of players", ylab = "Partial effect", main = "Partial effect of Mean_players on Ratings")

# weights: as weights increases, the same does the response (by response now I mean the response keeping the effects of other variables fixed) --> più un gioco è difficile, più ci piace
plot(semiparam4, xlab = "Complexity or Weights of the game", ylab = "Partial effect", main = "Partial effect of Weights on Ratings")


draw(semiparam4, residuals = TRUE)


#Prediction:
# #Create he grid for the predictions: #DA SISTEMARE!!!!
# Playing_time.grid=seq(range(boardgames_gam$Playing_time)[1],range(boardgames_gam$Playing_time)[2],length.out = 100) #length.out = 100-->I will take 100 points in the grid
# Mean_players.grid=seq(range(boardgames_gam$Mean_players)[1],range(boardgames_gam$Mean_players)[2],length.out = 100)
# grid=expand.grid(Playing_time.grid,Mean_players.grid)
# names(grid)=c('Playing_time','Mean_players')
# 
# pred_semiparam2=predict(semiparam2,newdata=grid)
# persp3d(education.grid,income.grid,pred_gam,col='#e6e6fa') #da modificare
# with(boardgames_gam,points3d(education,income,prestige,col='#6495ed',size=5)) #da modificare





##WITH THIN-PLATE SPLINES----
semiparam5 = gam(Ratings ~ s(Playing_time, Mean_players, weights, bs="tp", m=3) + Economic + Negotiation + Political + 
               Card_Game + Fantasy + Abstract_Strategy + Medieval + Ancient + Territory_Building + 
               Civilization + Nautical + Exploration + Farming + Mythology + Bluffing +
               Science_Fiction + Dice + Fighting + Print_and_Play + Maze + Miniatures +
               Racing + City_Building + Wargame + Adventure + Space_Exploration + Renaissance + 
               Humor + Electronic + Horror + Novel_based + Word_Game + 
               Aviation_Flight + Movies_TV_Radio_theme + Party_Game + Memory + Puzzle + Real_time + 
               Trivia + Industry_Manufacturing + Age_of_Reason + Trains + Animals + Childrens_Game + 
               Pirates + Transportation + Action_Dexterity + Spies_Secret_Agents + Educational + 
               Napoleonic + Post_Napoleonic + Math + Book + Music + Environmental + 
               Number + Pike_and_Shot + Video_Game_Theme + Mature_Adult + Expansion_for_Base_game, data = boardgames_gam) 
summary(semiparam5) #R-sq.(adj) = 0.366

#Remove non-sign variables (just looking at the signif; level=10%):
semiparam6 = gam(Ratings ~ s(Playing_time, Mean_players, weights, bs="tp", m=3) + Economic + Negotiation + 
                   Card_Game + Fantasy + Abstract_Strategy + Farming + Mythology + Bluffing +
                   Science_Fiction + Print_and_Play + Miniatures +
                   City_Building + Wargame + Space_Exploration + 
                   Humor + Electronic + Novel_based + Movies_TV_Radio_theme + Party_Game + Memory + Puzzle + Real_time + 
                   Trivia + Age_of_Reason + Trains + Animals + Childrens_Game + 
                   Action_Dexterity + 
                   Napoleonic + Math + Book + Mature_Adult, data = boardgames_gam)  
summary(semiparam6) #R-sq.(adj) = 0.366




#Check the residuals:
hist(semiparam6$residuals)#no sym --> no N
qqnorm(semiparam6$residuals) 
qqline(semiparam6$residuals) #bad for both the 2 tails, especially for the left one --> no N

ks.test(semiparam6$residuals, "pnorm") #p-value < 2.2e-16 --> I cannot accept H0 --> no N

mu.res <- mean(semiparam6$residuals) 
s.res <- sd(semiparam6$residuals)
x <- as.matrix(semiparam6$residuals) #note that here we need to use as.matrix in order to use the ddplot command below
y <- as.matrix( rnorm(length(semiparam6$residuals), mu.res, sqrt(s.res)) )
DepthProc::ddPlot(x, y,
                  depth_params = list(method='Tukey')) #Not so correlated --> no N

#Plot (look at the contribution of a single predictor holding all the others fixed):
plot(semiparam6) #emmmm panico --> sei un po' tanto brutto... ti devo aggiustare!!

draw(semiparam6, residuals=TRUE)


# #Prediction: #DA SISTEMARE!!!!
# pred_tp = predict(gam_tp, newdata = data.frame(grid))
# persp3d(education.grid, income.grid, pred_tp, col = '#e6e6fa') #da modificare
# with(Prestige, points3d(education, income, prestige, col = '#6495ed', size = 5)) #da modificare
# 
# #Plot (similar to level curves):
# plot(gam_tp) #boh io sto grafico non so interpretarlo :(






##COMPARISION BETWEEN DIFFERENT MODELS----
# #Ftest for comparing models:
# #version for N residuals:
# anova(semiparam,gam_cubic, test = "F") #H0: W seimparam vs H1: W gam_cubic
# UNFORTUNATELY RES ARE ALWAYS NON N SO...
# 
# 
# 
# 
# USE: DA FARE PER QUESTI MODELLI!!!!
# #version for nonN residuals:
# B = 1e3
# seed = 26111992
# fit <- aov(semiparam ~ gam_cubic)
# summary(fit) #we perform it (even if it is parametric) since we need to have the value of the test stat!!
# T0 <- summary(fit)[[1]][1,4]  # extract the test statistic
# T0
# T_stat <- numeric(B) 
# n <- dim(boardgames_gam)[1]
# 
# for(perm in 1:B){
#   # Permutation:
#   permutation <- sample(1:n) #pooled sample
#   semiparam_perm <- semiparam[permutation] #non sono molto sicura di questo pezzo, da controllare!!
#   fit_perm <- aov(semiparam_perm ~ gam_cubic)
#   
#   # Test statistic:
#   T_stat[perm] <- summary(fit_perm)[[1]][1,4]
# }
# # p-value
# p_val <- sum(T_stat>=T0)/B
# p_val #H0:no differenze significative vs H1: si differenze



















# #DA FARE!!!!
# #Models for interactions: (1), (2): potrei fare interazione tra varabili quantitative e quelle categoriche!!!!
# ##GAM with INTERACTION (1):
# gam_inter = gam(Ratings ~ s(Playing_time, bs = 'cr') + 
#                     s(Mean_players, bs ='cr') + 
#                     s(I(Playing_time * Mean_players), bs = 'cr') + Economic + Negotiation + Political + 
#                   Card_Game + Fantasy + Abstract_Strategy + Medieval + Ancient + Territory_Building + 
#                   Civilization + Nautical + Exploration + Farming + Mythology + Bluffing +
#                   Science_Fiction + Dice + Fighting + Print_and_Play + Maze + Miniatures +
#                   Racing + City_Building + Wargame + Adventure + Space_Exploration + Renaissance + 
#                   Humor + Electronic + Horror + Novel_based + Word_Game + 
#                   Aviation_Flight + Movies_TV_Radio_theme + Party_Game + Memory + Puzzle + Real_time + 
#                   Trivia + Industry_Manufacturing + Age_of_Reason + Trains + Animals + Childrens_Game + 
#                   Pirates + Transportation + Action_Dexterity + Spies_Secret_Agents + Educational + 
#                   Napoleonic + Post_Napoleonic + Math + Book + Music + Environmental + 
#                   Number + Pike_and_Shot + Video_Game_Theme + Mature_Adult + Expansion_for_Base_game,
#                   data = boardgames_gam) 
# summary(gam_inter) #R-sq.(adj) =   0.261
# 
# gam_inter2 = gam(Ratings ~ s(Playing_time, bs = 'cr') + 
#                   s(Mean_players, bs ='cr') + 
#                   s(I(Playing_time * Mean_players), bs = 'cr') + Negotiation + Political + 
#                   Card_Game + Fantasy + Medieval + Ancient + Territory_Building + 
#                   Civilization + Farming + Mythology + Bluffing +
#                   Science_Fiction + Fighting + Print_and_Play + Miniatures +
#                   Racing + City_Building + Space_Exploration + Renaissance + 
#                   Humor + Electronic + Horror + Novel_based + 
#                   Aviation_Flight + Movies_TV_Radio_theme + Party_Game + Memory + Puzzle + Real_time + 
#                   Trivia + Industry_Manufacturing + Age_of_Reason + Trains + Animals + Spies_Secret_Agents + Educational + 
#                   Napoleonic + Post_Napoleonic + Math + Book + Environmental + 
#                   Mature_Adult + Expansion_for_Base_game,
#                 data = boardgames_gam) 
# summary(gam_inter2) #R-sq.(adj) =   0.25
# 
# gam_inter3 = gam(Ratings ~ s(Playing_time, bs = 'cr') + 
#                    s(Mean_players, bs ='cr') + 
#                    s(I(Playing_time * Mean_players), bs = 'cr') + Negotiation + Political + 
#                    Card_Game + Fantasy + Medieval + Ancient + Territory_Building + 
#                    Civilization + Farming + Mythology + Bluffing +
#                    Science_Fiction + Fighting + Print_and_Play + Miniatures +
#                    Racing + City_Building + Space_Exploration + Renaissance + 
#                    Humor + Electronic + Horror + Novel_based + 
#                    Aviation_Flight + Movies_TV_Radio_theme + Party_Game + Memory + Puzzle + Real_time + 
#                    Trivia + Industry_Manufacturing + Age_of_Reason + Trains + Spies_Secret_Agents + 
#                    Napoleonic + Post_Napoleonic + Math + Book + Environmental + 
#                    Mature_Adult + Expansion_for_Base_game,
#                  data = boardgames_gam) 
# summary(gam_inter3) #R-sq.(adj) =   0.25
# 
# 
# #Prediction:  #DA SISTEMARE!!!!
# pred_inter = predict(model_gam_inter,newdata = data.frame(grid, inter = grid$education * grid$income))
# persp3d(education.grid, income.grid, pred_inter, col = '#e6e6fa') #da modificare
# with(boardgames_gam,points3d(education, income, prestige, col = '#6495ed', size = 5)) #da modificare
# 
# 
# 
# 
# 
# 
# gam_inter4 = gam(Ratings ~ s(Playing_time, bs = 'bs') + 
#                   s(Mean_players, bs ='bs') + 
#                   s(I(Playing_time * Mean_players), bs = 'bs') + Economic + Negotiation + Political + 
#                   Card_Game + Fantasy + Abstract_Strategy + Medieval + Ancient + Territory_Building + 
#                   Civilization + Nautical + Exploration + Farming + Mythology + Bluffing +
#                   Science_Fiction + Dice + Fighting + Print_and_Play + Maze + Miniatures +
#                   Racing + City_Building + Wargame + Adventure + Space_Exploration + Renaissance + 
#                   Humor + Electronic + Horror + Novel_based + Word_Game + 
#                   Aviation_Flight + Movies_TV_Radio_theme + Party_Game + Memory + Puzzle + Real_time + 
#                   Trivia + Industry_Manufacturing + Age_of_Reason + Trains + Animals + Childrens_Game + 
#                   Pirates + Transportation + Action_Dexterity + Spies_Secret_Agents + Educational + 
#                   Napoleonic + Post_Napoleonic + Math + Book + Music + Environmental + 
#                   Number + Pike_and_Shot + Video_Game_Theme + Mature_Adult + Expansion_for_Base_game,
#                 data = boardgames_gam) 
# summary(gam_inter4) #R-sq.(adj) =   0.236
# 
# gam_inter5 = gam(Ratings ~ s(Playing_time, bs = 'bs') + 
#                    s(Mean_players, bs ='bs') + 
#                    s(I(Playing_time * Mean_players), bs = 'bs') + Negotiation + Political + 
#                    Fantasy + Medieval + Ancient + Territory_Building + 
#                    Civilization + Farming + Mythology + Bluffing +
#                    Science_Fiction + Dice + Fighting + Print_and_Play + Miniatures +
#                    Racing + City_Building + Wargame + Space_Exploration + Renaissance + 
#                    Humor + Electronic + Horror + Novel_based + 
#                    Aviation_Flight + Movies_TV_Radio_theme + Party_Game + Memory + Puzzle + 
#                    Trivia + Industry_Manufacturing + Age_of_Reason + Trains + Animals + Childrens_Game + 
#                    Action_Dexterity + Spies_Secret_Agents + 
#                    Napoleonic + Post_Napoleonic + Math + Book + Environmental + 
#                    Number + Mature_Adult + Expansion_for_Base_game,
#                  data = boardgames_gam) 
# summary(gam_inter5) #R-sq.(adj) =   0.236
# 
# 
# 
# 
# 
# 
# 
# 
# ##GAM with THIN-PLATE SPLINES (2)----
# gam_tp = gam(Ratings ~ s(Playing_time, Mean_players, bs="tp", m=2) + Economic + Negotiation + Political + 
#                Card_Game + Fantasy + Abstract_Strategy + Medieval + Ancient + Territory_Building + 
#                Civilization + Nautical + Exploration + Farming + Mythology + Bluffing +
#                Science_Fiction + Dice + Fighting + Print_and_Play + Maze + Miniatures +
#                Racing + City_Building + Wargame + Adventure + Space_Exploration + Renaissance + 
#                Humor + Electronic + Horror + Novel_based + Word_Game + 
#                Aviation_Flight + Movies_TV_Radio_theme + Party_Game + Memory + Puzzle + Real_time + 
#                Trivia + Industry_Manufacturing + Age_of_Reason + Trains + Animals + Childrens_Game + 
#                Pirates + Transportation + Action_Dexterity + Spies_Secret_Agents + Educational + 
#                Napoleonic + Post_Napoleonic + Math + Book + Music + Environmental + 
#                Number + Pike_and_Shot + Video_Game_Theme + Mature_Adult + Expansion_for_Base_game, data = boardgames_gam) #da modificare 
# summary(gam_tp) #R-sq.(adj) =   0.24
# 
# gam_tp2 = gam(Ratings ~ s(Playing_time, Mean_players, bs="tp", m=2) + Economic + Negotiation + Political + 
#                Card_Game + Fantasy + Abstract_Strategy + Medieval + Ancient + Territory_Building + 
#                Civilization + Farming + Mythology + Bluffing +
#                Science_Fiction + Fighting + Print_and_Play + Miniatures +
#                Racing + City_Building + Wargame + Space_Exploration + Renaissance + 
#                Humor + Electronic + Horror + Novel_based + 
#                Aviation_Flight + Movies_TV_Radio_theme + Memory + Puzzle + Real_time + 
#                Trivia + Industry_Manufacturing + Age_of_Reason + Trains + Animals + Childrens_Game + 
#                Spies_Secret_Agents + 
#                Napoleonic + Post_Napoleonic + Math + Book + Environmental + 
#                Mature_Adult + Expansion_for_Base_game, data = boardgames_gam) #da modificare 
# summary(gam_tp2) #R-sq.(adj) =   0.24
# 
# 
# #Prediction: #DA SISTEMARE!!!!
# pred_tp = predict(gam_tp, newdata = data.frame(grid))
# persp3d(education.grid, income.grid, pred_tp, col = '#e6e6fa') #da modificare
# with(Prestige, points3d(education, income, prestige, col = '#6495ed', size = 5)) #da modificare
# 
# #Plot (similar to level curves):
# plot(gam_tp)
# 
# 
# 
# 
# 
#
##BOOTSTRAP METHOD for CONSTRUCTING NP CONFIDENCE BANDS----
#da studiare