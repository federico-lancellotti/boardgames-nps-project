# Cose da fare:
#   1) sistema Year (ok)
#   2) Dataset storico:
#      -day=ultimo giorno di vita (calcolato a distanza del tempo x scelto)=data_di_morte-1 (ok)
#      -time=# giorni di vita (calcolati a distanza del tempo x scelto) (ok)
#      -COME TEMPO DI VITA TENGO 8 MESI (ok)
#   3) Dataset snapshot:
#      -aggiungi due colonne, una per i tempi e una per lo status(1=censurato, 2=morto) (ok)

#istante iniziale: 12 aprile 2016
#istante finale: 4 novembre 2023

#anni bisestili: 2016, 2020 (farò finta di niente? ovviamente sì)

#tot gg:2579

#tempo di vita = 8 mesi -->243 d



sum(usersRated_preproc[,"X2016.10.12"] != 0) #12624 --> giochi anziani




# Import necessary libraries --------------------------------------------------
library(roahd)
library(dplyr)
library(tidyr)
library(fda)
library(fields)


setwd("C:/Users/Utente/Desktop/NPS/Projects/git_clone/boardgames-nps-project/survival")


# Import data -----------------------------------------------------------------

# usersRated <- read.csv("C:/Users/Utente/Desktop/NPS/Projects/git_clone/boardgames-nps-project/survival/usersRated.csv")
# View(usersRated)
# 
# usersRated_preproc <- read.csv("C:/Users/Utente/Desktop/NPS/Projects/git_clone/boardgames-nps-project/survival/usersRated_preproc.csv")
# View(usersRated_preproc)
# data=usersRated_preproc
# 
# ## How many categories
# catList.index <- which(colnames(data) == "Category")
# start.index <- which(colnames(data) == "X2016.10.12")
# n_categories <- start.index - catList.index - 1
# n_categories




# # Plots 
# P <- ncol(data) - 4 - n_categories  # from 2016.10.12 to 2023.11.04 should be 2579
# abscissa <-  seq(0, 1, length.out=P)
# sample_rows <- sample(1:nrow(data), 100)
# f_data <- fData(abscissa, data[sample_rows, start.index:ncol(data)])
# plot(f_data)






# FIX PROBLEMS WITH YEARS------------------------------------------------------
boardgames.weights.publishers <- read.csv("C:/Users/Utente/Desktop/NPS/Projects/git_clone/boardgames-nps-project/data-snapshot/boardgames-weights-publishers.csv")
View(boardgames.weights.publishers)
boardgames.weights.publishers=boardgames.weights.publishers[,-4]

load("C:/Users/Utente/Desktop/NPS/Projects/git_clone/boardgames-nps-project/survival/workspace.RData")
board=boardgames_selcat[, c(1,6)]
View(board)
names(board)[1] ="id"

boardgames_weights_publishers=merge(boardgames.weights.publishers, board, by = "id")
View(boardgames_weights_publishers)

write.csv(boardgames_weights_publishers, file = "boardgames_weights_publishers.csv", row.names = FALSE)




# Identify Survival times------------------------------------------------------
usersRated_preproc <- read.csv("C:/Users/Utente/Desktop/NPS/Projects/git_clone/boardgames-nps-project/survival/usersRated_preproc.csv")
View(usersRated_preproc)

#I keep only id and users_rated:
usersRated_preproc=usersRated_preproc[,-c(2:88)]

#Change the name of id col:
names(usersRated_preproc)[1]="id"

#NAs?:
any(is.na(usersRated_preproc)) #FALSE

# #FIRST ATTEMPT: 1 YEAR
# #Def the death of a game (after A YEAR (365 d) with constant # of ratings):
# #for 1000 rows:
# start=which(colnames(usersRated_preproc) == "X2016.10.12")
# end=ncol(usersRated_preproc)
# count=0
# day=vector("character", length = 1000)
# time=rep(NA, times = 1000)
# 
# for (j in 1:1000) {
#   init=0
#   for (i in start:(end-1)) {
#     if(usersRated_preproc[j,i] != 0){
#       init=init+1
#       if(usersRated_preproc[j,i]==usersRated_preproc[j,i+1]){
#         count=count+1
#         if(count==365){
#           day[j]=colnames(usersRated_preproc)[i] #ok
#           time[j]=init #ok 
#           break
#         }
#       }
#       else
#         count=0
#     }
#   }
# }
# day
# time
# deaths=0
# zombies=0
# for (i in 1:length(time)) {
#   if(!is.na(time[i]))
#     deaths=deaths+1
#   if(!is.na(time[i]) && time[i]==365)
#     zombies=zombies+1
# }
# deaths 
# zombies 
# nrow(usersRated_preproc) #25033
# ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games
# 
# # #qual è la proporzione di morti sul totale nel dataset lung?
# # morti=0
# # for (i in 1:dim(lung)[1]) {
# #   if(lung$status[i]==2)
# #     morti=morti+1
# # }
# # morti #165-->57% --> 
# # censurati=dim(lung)[1]-morti
# # censurati #63 #dim(lung)[1]=228
# 
# 
# #FIRST ATTEMPT: 6 MONTHS
# #Def the death of a game (after 6 MONTHS (182 d) with constant # of ratings):
# #for 1000 rows:
# start=which(colnames(usersRated_preproc) == "X2016.10.12")
# end=ncol(usersRated_preproc)
# count=0
# day=vector("character", length = 1000)
# time=rep(NA, times = 1000)
# 
# for (j in 1:1000) {
#   for (i in start:end) {
#     if(usersRated_preproc[j,i-1]==usersRated_preproc[j,i]  && usersRated_preproc[j,i] != 0){
#       count=count+1
#       if(count==182){
#         day[j]=colnames(usersRated_preproc)[i] #--> ultimo giorno di vita
#         time[j]=i-1-1 #ok (-1=tolgo id, -1=tolgo il giorno corrente perchè è quello della morte) --> # gg di vita
#         break
#       }
#     }  
#     else
#       count=0
#   }
# }
# day
# time
# deaths=0
# zombies=0
# for (i in 1:length(time)) {
#   if(!is.na(time[i]))
#     deaths=deaths+1
#   if(!is.na(time[i]) && time[i]==182)
#     zombies=zombies+1
# }
# deaths #418/1000 deaths
# zombies #85/418 "nati morti"
# nrow(usersRated_preproc) #25033
# ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games
# 
# # #qual è la proporzione di morti sul totale nel dataset lung?
# # morti=0
# # for (i in 1:dim(lung)[1]) {
# #   if(lung$status[i]==2)
# #     morti=morti+1
# # }
# # morti #165-->57% --> PROVIAMO A PRENDERE 6 MESI COME TEMPO DI MORTE PER I BOARDGAMES!!!!
# # censurati=dim(lung)[1]-morti
# # censurati #63 #dim(lung)[1]=228
# 
# 
# #FIRST ATTEMPT: 4 MONTHS
# #Def the death of a game (after 4 MONTHS (121 d) with constant # of ratings):
# #for 1000 rows:
# start=which(colnames(usersRated_preproc) == "X2016.10.12")
# end=ncol(usersRated_preproc)
# count=0
# day=vector("character", length = 1000)
# time=rep(NA, times = 1000)
# 
# for (j in 1:1000) {
#   for (i in start:end) {
#     if(usersRated_preproc[j,i-1]==usersRated_preproc[j,i]  && usersRated_preproc[j,i] != 0){
#       count=count+1
#       if(count==121){
#         day[j]=colnames(usersRated_preproc)[i] #--> ultimo giorno di vita
#         time[j]=i-1-1 #ok (-1=tolgo id, -1=tolgo il giorno corrente perchè è quello della morte) --> # gg di vita
#         break
#       }
#     }  
#     else
#       count=0
#   }
# }
# day
# time
# deaths=0
# zombies=0
# for (i in 1:length(time)) {
#   if(!is.na(time[i]))
#     deaths=deaths+1
#   if(!is.na(time[i]) && time[i]==121)
#     zombies=zombies+1
# }
# deaths
# zombies
# nrow(usersRated_preproc) #25033
# ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games
# 
# # #qual è la proporzione di morti sul totale nel dataset lung?
# # morti=0
# # for (i in 1:dim(lung)[1]) {
# #   if(lung$status[i]==2)
# #     morti=morti+1
# # }
# # morti #165-->57% --> PROVIAMO A PRENDERE 6 MESI COME TEMPO DI MORTE PER I BOARDGAMES!!!!
# # censurati=dim(lung)[1]-morti
# # censurati #63 #dim(lung)[1]=228
# 
# 
# #FIRST ATTEMPT: 3 MONTHS
# #Def the death of a game (after 3 MONTHS (91 d) with constant # of ratings):
# #for 1000 rows:
# start=which(colnames(usersRated_preproc) == "X2016.10.12")
# end=ncol(usersRated_preproc)
# count=0
# day=vector("character", length = 1000)
# time=rep(NA, times = 1000)
# 
# for (j in 1:1000) {
#   for (i in start:end) {
#     if(usersRated_preproc[j,i-1]==usersRated_preproc[j,i]  && usersRated_preproc[j,i] != 0){
#       count=count+1
#       if(count==91){
#         day[j]=colnames(usersRated_preproc)[i] #--> ultimo giorno di vita
#         time[j]=i-1-1 #ok (-1=tolgo id, -1=tolgo il giorno corrente perchè è quello della morte) --> # gg di vita
#         break
#       }
#     }  
#     else
#       count=0
#   }
# }
# day
# time
# deaths=0
# zombies=0
# for (i in 1:length(time)) {
#   if(!is.na(time[i]))
#     deaths=deaths+1
#   if(!is.na(time[i]) && time[i]==91)
#     zombies=zombies+1
# }
# deaths 
# zombies 
# nrow(usersRated_preproc) #25033
# ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games
# 
# # #qual è la proporzione di morti sul totale nel dataset lung?
# # morti=0
# # for (i in 1:dim(lung)[1]) {
# #   if(lung$status[i]==2)
# #     morti=morti+1
# # }
# # morti #165-->57% --> PROVIAMO A PRENDERE 6 MESI COME TEMPO DI MORTE PER I BOARDGAMES!!!!
# # censurati=dim(lung)[1]-morti
# # censurati #63 #dim(lung)[1]=228








# #FINAL 2: 6 MONTHS:
# #Def the death of a game (after 6 MONTHS (182 d) with constant # of ratings):
# start=which(colnames(usersRated_preproc) == "X2016.10.12")
# end=ncol(usersRated_preproc)
# count_6=0
# day_6=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
# time_6=rep(NA, times = nrow(usersRated_preproc))
# 
# for (j in 1:nrow(usersRated_preproc)) {
#   init_6=0
#   for (i in start:(end-1)) {
#     if(usersRated_preproc[j,i] != 0){
#       init_6=init_6+1
#       if(usersRated_preproc[j,i]==usersRated_preproc[j,i+1]){
#         count_6=count_6+1
#         if(count_6==182){
#           day_6[j]=colnames(usersRated_preproc)[i] #--> ultimo giorno di vita
#           time_6[j]=init_6 #ok (-1=tolgo id, -1=tolgo il giorno corrente perchè è quello della morte) --> # gg di vita
#           break
#         }
#       }  
#       else
#         count_6=0
#     }
#   }
# }
# #day_6
# #time_6
# deaths_6=0
# zombies_6=0
# for (i in 1:length(time_6)) {
#   if(!is.na(time_6[i]))
#     deaths_6=deaths_6+1
#   if(!is.na(time_6[i]) && time_6[i]==182)
#     zombies_6=zombies_6+1
# }
# deaths_6 #9927
# zombies_6 #90
# ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games
# 
# 
# #FINAL 3: 1 YEAR:
# #Def the death of a game (after A YEAR (365 d) with constant # of ratings):
# start=which(colnames(usersRated_preproc) == "X2016.10.12")
# end=ncol(usersRated_preproc)
# count_12=0
# day_12=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
# time_12=rep(NA, times = nrow(usersRated_preproc))
# 
# for (j in 1:nrow(usersRated_preproc)) {
#   init_12=0
#   for (i in start:(end-1)) {
#     if(usersRated_preproc[j,i] != 0){
#       init_12=init_12+1
#       if(usersRated_preproc[j,i]==usersRated_preproc[j,i+1]){
#         count_12=count_12+1
#         if(count_12==365){
#           day_12[j]=colnames(usersRated_preproc)[i] #--> ultimo giorno di vita
#           time_12[j]=init_12 # gg di vita
#           break
#         }
#       }  
#       else
#         count_12=0
#     }
#   }
# }
# #day_12
# #time_12
# deaths_12=0
# zombies_12=0
# for (i in 1:length(time_12)) {
#   if(!is.na(time_12[i]))
#     deaths_12=deaths_12+1
#   if(!is.na(time_12[i]) && time_12[i]==365)
#     zombies_12=zombies_12+1
# }
# deaths_12 #4179
# zombies_12#24
# ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games
# 
# 
# #FINAL 4: 10 MONTHS:
# #Def the death of a game (after 10 MONTHS (304 d) with constant # of ratings):
# start=which(colnames(usersRated_preproc) == "X2016.10.12")
# end=ncol(usersRated_preproc)
# count_10=0
# day_10=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
# time_10=rep(NA, times = nrow(usersRated_preproc))
# 
# for (j in 1:nrow(usersRated_preproc)) {
#   init_10=0
#   for (i in start:(end-1)) {
#     if(usersRated_preproc[j,i] != 0){
#       init_10=init_10+1
#       if(usersRated_preproc[j,i]==usersRated_preproc[j,i+1]){
#         count_10=count_10+1
#         if(count_10==304){
#           day_10[j]=colnames(usersRated_preproc)[i] #--> ultimo giorno di vita
#           time_10[j]=init_10 # gg di vita
#           break
#         }
#       }  
#       else
#         count_10=0
#     }
#   }
# }
# #day_10
# #time_10
# deaths_10=0
# zombies_10=0
# for (i in 1:length(time_10)) {
#   if(!is.na(time_10[i]))
#     deaths_10=deaths_10+1
#   if(!is.na(time_10[i]) && time_10[i]==304)
#     zombies_10=zombies_10+1
# }
# deaths_10 #5587
# zombies_10 #39
# ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games
# 
# 
# #FINAL 5: 8 MONTHS:
# #Def the death of a game (after 8 MONTHS (243 d) with constant # of ratings):
# start=which(colnames(usersRated_preproc) == "X2016.10.12")
# end=ncol(usersRated_preproc)
# count_8=0
# day_8=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
# time_8=rep(NA, times = nrow(usersRated_preproc))
# 
# for (j in 1:nrow(usersRated_preproc)) {
#   init_8=0
#   for (i in start:(end-1)) {
#     if(usersRated_preproc[j,i] != 0){
#       init_8=init_8+1
#       if(usersRated_preproc[j,i]==usersRated_preproc[j,i+1]){
#         count_8=count_8+1
#         if(count_8==243){
#           day_8[j]=colnames(usersRated_preproc)[i] #--> ultimo giorno di vita
#           time_8[j]=init_8 #ok (-1=tolgo id, -1=tolgo il giorno corrente perchè è quello della morte) --> # gg di vita
#           break
#         }
#       }  
#       else
#         count_8=0
#     }
#   }
# }
# #day_8
# #time_8
# deaths_8=0
# zombies_8=0
# for (i in 1:length(time_8)) {
#   if(!is.na(time_8[i]))
#     deaths_8=deaths_8+1
#   if(!is.na(time_8[i]) && time_8[i]==243)
#     zombies_8=zombies_8+1
# }
# deaths_8 #7520
# zombies_8 #57
# ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games




## 4 MONTHS--------------------------------------------------------------------

#FINAL 1: 4 MONTHS:
#Def the death of a game (after 4 MONTHS (121 d) with constant # of ratings):
start=which(colnames(usersRated_preproc) == "X2016.10.12")
end=ncol(usersRated_preproc)
count_4=0
day_4=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
time_4=rep(NA, times = nrow(usersRated_preproc))

for (j in 1:nrow(usersRated_preproc)) {
  init_4=0
  for (i in start:(end-1)) {
    if(usersRated_preproc[j,i] != 0){
      init_4=init_4+1
      if(usersRated_preproc[j,i]==usersRated_preproc[j,i+1]){
        count_4=count_4+1
        if(count_4==121){
          day_4[j]=colnames(usersRated_preproc)[i] # ultimo giorno di vita
          time_4[j]=init_4 # gg di vita
          break
        }
      }  
      else
        count_4=0
    }
  }
}
#day_4
#time_4
deaths_4=0
zombies_4=0
for (i in 1:length(time_4)) {
  if(!is.na(time_4[i]))
    deaths_4=deaths_4+1
  if(!is.na(time_4[i]) && time_4[i]==121)
    zombies_4=zombies_4+1
}
deaths_4 #13048
zombies_4 #145
ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games


#FINAL 1.2: 4 MONTHS AND RATINGS <=2:
#Def the death of a game (after 4 MONTHS (121 d) with ratings <=2):
start=which(colnames(usersRated_preproc) == "X2016.10.12")
end=ncol(usersRated_preproc)
count_4_2=0
day_4_2=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
time_4_2=rep(NA, times = nrow(usersRated_preproc))

for (j in 1:nrow(usersRated_preproc)) {
  init_4_2=0
  for (i in start:(end-1)) {
    if(usersRated_preproc[j,i] != 0){
      init_4_2=init_4_2+1
      if(usersRated_preproc[j,i+1]-usersRated_preproc[j,i] <= 2){
        count_4_2=count_4_2+1
        if(count_4_2==121){
          day_4_2[j]=colnames(usersRated_preproc)[i] #--> ultimo giorno di vita
          time_4_2[j]=init_4_2 # gg di vita
          break
        }
      }  
      else
        count_4_2=0
    }
  }
}
#day_4_2
#time_4_2
deaths_4_2=0
zombies_4_2=0
for (i in 1:length(time_4_2)) {
  if(!is.na(time_4_2[i]))
    deaths_4_2=deaths_4_2+1
  if(!is.na(time_4_2[i]) && time_4_2[i]==121)
    zombies_4_2=zombies_4_2+1
}
deaths_4_2 #7767 
zombies_4_2 #314 
ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games


#FINAL 1.3: 4 MONTHS AND RATINGS <=5:
#Def the death of a game (after 4 MONTHS (121 d) with ratings <=5):
start=which(colnames(usersRated_preproc) == "X2016.10.12")
end=ncol(usersRated_preproc)
count_4_5=0
day_4_5=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
time_4_5=rep(NA, times = nrow(usersRated_preproc))

for (j in 1:nrow(usersRated_preproc)) {
  init_4_5=0
  for (i in start:(end-1)) {
    if(usersRated_preproc[j,i] != 0){
      init_4_5=init_4_5+1
      if(usersRated_preproc[j,i+1]-usersRated_preproc[j,i] <= 5){
        count_4_5=count_4_5+1
        if(count_4_5==121){
          day_4_5[j]=colnames(usersRated_preproc)[i] #--> ultimo giorno di vita
          time_4_5[j]=init_4_5 # gg di vita
          break
        }
      }  
      else
        count_4_5=0
    }
  }
}
#day_4_5
#time_4_5
deaths_4_5=0
zombies_4_5=0
for (i in 1:length(time_4_5)) {
  if(!is.na(time_4_5[i]))
    deaths_4_5=deaths_4_5+1
  if(!is.na(time_4_5[i]) && time_4_5[i]==121)
    zombies_4_5=zombies_4_5+1
}
deaths_4_5 #3490 
zombies_4_5 #137 
ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games




## 2 MONTHS--------------------------------------------------------------------

#FINAL 2: 2 MONTHS:
#Def the death of a game (after 2 MONTHS (61 d) with constant # of ratings):
start=which(colnames(usersRated_preproc) == "X2016.10.12")
end=ncol(usersRated_preproc)
count_2=0
day_2=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
time_2=rep(NA, times = nrow(usersRated_preproc))

for (j in 1:nrow(usersRated_preproc)) {
  init_2=0
  for (i in start:(end-1)) {
    if(usersRated_preproc[j,i] != 0){
      init_2=init_2+1
      if(usersRated_preproc[j,i]==usersRated_preproc[j,i+1]){
        count_2=count_2+1
        if(count_2==61){
          day_2[j]=colnames(usersRated_preproc)[i] # ultimo giorno di vita
          time_2[j]=init_2 # gg di vita
          break
        }
      }  
      else
        count_2=0
    }
  }
}
#day_2
#time_2
deaths_2=0
zombies_2=0
for (i in 1:length(time_2)) {
  if(!is.na(time_2[i]))
    deaths_2=deaths_2+1
  if(!is.na(time_2[i]) && time_2[i]==61)
    zombies_2=zombies_2+1
}
deaths_2 #17227
zombies_2 #263
ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games


#FINAL 2.2: 2 MONTHS AND RATINGS <=2:
#Def the death of a game (after 2 MONTHS (61 d) with ratings <=2):
start=which(colnames(usersRated_preproc) == "X2016.10.12")
end=ncol(usersRated_preproc)
count_2_2=0
day_2_2=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
time_2_2=rep(NA, times = nrow(usersRated_preproc))

for (j in 1:nrow(usersRated_preproc)) {
  init_2_2=0
  for (i in start:(end-1)) {
    if(usersRated_preproc[j,i] != 0){
      init_2_2=init_2_2+1
      if(usersRated_preproc[j,i+1]-usersRated_preproc[j,i] <= 2){
        count_2_2=count_2_2+1
        if(count_2_2==61){
          day_2_2[j]=colnames(usersRated_preproc)[i] #--> ultimo giorno di vita
          time_2_2[j]=init_2_2 # gg di vita
          break
        }
      }  
      else
        count_2_2=0
    }
  }
}
#day_2_2
#time_2_2
deaths_2_2=0
zombies_2_2=0
for (i in 1:length(time_2_2)) {
  if(!is.na(time_2_2[i]))
    deaths_2_2=deaths_2_2+1
  if(!is.na(time_2_2[i]) && time_2_2[i]==61)
    zombies_2_2=zombies_2_2+1
}
deaths_2_2 #8069
zombies_2_2 #326
ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games


#FINAL 2.3: 2 MONTHS AND RATINGS <=5:
#Def the death of a game (after 2 MONTHS (61 d) with ratings <=5):
start=which(colnames(usersRated_preproc) == "X2016.10.12")
end=ncol(usersRated_preproc)
count_2_5=0
day_2_5=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
time_2_5=rep(NA, times = nrow(usersRated_preproc))

for (j in 1:nrow(usersRated_preproc)) {
  init_2_5=0
  for (i in start:(end-1)) {
    if(usersRated_preproc[j,i] != 0){
      init_2_5=init_2_5+1
      if(usersRated_preproc[j,i+1]-usersRated_preproc[j,i] <= 5){
        count_2_5=count_2_5+1
        if(count_2_5==61){
          day_2_5[j]=colnames(usersRated_preproc)[i] #--> ultimo giorno di vita
          time_2_5[j]=init_2_5 # gg di vita
          break
        }
      }  
      else
        count_2_5=0
    }
  }
}
#day_2_5
#time_2_5
deaths_2_5=0
zombies_2_5=0
for (i in 1:length(time_2_5)) {
  if(!is.na(time_2_5[i]))
    deaths_2_5=deaths_2_5+1
  if(!is.na(time_2_5[i]) && time_2_5[i]==61)
    zombies_2_5=zombies_2_5+1
}
deaths_2_5 #3558
zombies_2_5 #138
ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games




## 1 MONTH--------------------------------------------------------------------

#FINAL 3: 1 MONTH:
#Def the death of a game (after 1 MONTH (31 d) with constant # of ratings):
start=which(colnames(usersRated_preproc) == "X2016.10.12")
end=ncol(usersRated_preproc)
count_1=0
day_1=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
time_1=rep(NA, times = nrow(usersRated_preproc))

for (j in 1:nrow(usersRated_preproc)) {
  init_1=0
  for (i in start:(end-1)) {
    if(usersRated_preproc[j,i] != 0){
      init_1=init_1+1
      if(usersRated_preproc[j,i]==usersRated_preproc[j,i+1]){
        count_1=count_1+1
        if(count_1==31){
          day_1[j]=colnames(usersRated_preproc)[i] # ultimo giorno di vita
          time_1[j]=init_1 # gg di vita
          break
        }
      }  
      else
        count_1=0
    }
  }
}
#day_1
#time_1
deaths_1=0
zombies_1=0
for (i in 1:length(time_1)) {
  if(!is.na(time_1[i]))
    deaths_1=deaths_1+1
  if(!is.na(time_1[i]) && time_1[i]==31)
    zombies_1=zombies_1+1
}
deaths_1 #20221
zombies_1 #352
ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games


#FINAL 3.2: 1 MONTH AND RATINGS <=2:
#Def the death of a game (after 1 MONTHS (31 d) with ratings <=2):
start=which(colnames(usersRated_preproc) == "X2016.10.12")
end=ncol(usersRated_preproc)
count_1_2=0
day_1_2=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
time_1_2=rep(NA, times = nrow(usersRated_preproc))

for (j in 1:nrow(usersRated_preproc)) {
  init_1_2=0
  for (i in start:(end-1)) {
    if(usersRated_preproc[j,i] != 0){
      init_1_2=init_1_2+1
      if(usersRated_preproc[j,i+1]-usersRated_preproc[j,i] <= 2){
        count_1_2=count_1_2+1
        if(count_1_2==31){
          day_1_2[j]=colnames(usersRated_preproc)[i] #--> ultimo giorno di vita
          time_1_2[j]=init_1_2 # gg di vita
          break
        }
      }  
      else
        count_1_2=0
    }
  }
}
#day_1_2
#time_1_2
deaths_1_2=0
zombies_1_2=0
for (i in 1:length(time_1_2)) {
  if(!is.na(time_1_2[i]))
    deaths_1_2=deaths_1_2+1
  if(!is.na(time_1_2[i]) && time_1_2[i]==31)
    zombies_1_2=zombies_1_2+1
}
deaths_1_2 #8311 
zombies_1_2 #305
ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games


#FINAL 3.3: 1 MONTH AND RATINGS <=5:
#Def the death of a game (after 1 MONTH (31 d) with ratings <=5):
start=which(colnames(usersRated_preproc) == "X2016.10.12")
end=ncol(usersRated_preproc)
count_1_5=0
day_1_5=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
time_1_5=rep(NA, times = nrow(usersRated_preproc))

for (j in 1:nrow(usersRated_preproc)) {
  init_1_5=0
  for (i in start:(end-1)) {
    if(usersRated_preproc[j,i] != 0){
      init_1_5=init_1_5+1
      if(usersRated_preproc[j,i+1]-usersRated_preproc[j,i] <= 5){
        count_1_5=count_1_5+1
        if(count_1_5==31){
          day_1_5[j]=colnames(usersRated_preproc)[i] #--> ultimo giorno di vita
          time_1_5[j]=init_1_5 # gg di vita
          break
        }
      }  
      else
        count_1_5=0
    }
  }
}
#day_1_5
#time_1_5
deaths_1_5=0
zombies_1_5=0
for (i in 1:length(time_1_5)) {
  if(!is.na(time_1_5[i]))
    deaths_1_5=deaths_1_5+1
  if(!is.na(time_1_5[i]) && time_1_5[i]==31)
    zombies_1_5=zombies_1_5+1
}
deaths_1_5 #3595 
zombies_1_5 #132 
ncol(usersRated_preproc)-1 #2579=censoring time for non-dead games







## QUANTILES METHOD------------------------------------------------------------
start=which(colnames(usersRated_preproc) == "X2016.10.12")
end=ncol(usersRated_preproc)
start_col = vector("list", length = nrow(usersRated_preproc)) #first col in which ratings != 0
for(i in 1: nrow(usersRated_preproc)){
  for(j in start:end){
    if(usersRated_preproc[i,j]!=0){
      start_col[[i]]=j 
      break
    }
  }
}
day=vector("character", length = nrow(usersRated_preproc)) #nrow(usersRated_preproc)=25033
time=rep(NA, times = nrow(usersRated_preproc))
diff=rep(NA, times= nrow(usersRated_preproc))
for (i in 1: nrow(usersRated_preproc)) {
  diff[i]=usersRated_preproc[i, end]- usersRated_preproc[i, start_col[[i]]]
}
head(diff)
#diff

for (i in 1:nrow(usersRated_preproc)) {
  init=0
  for (j in start:end) {
    if(usersRated_preproc[i,j]!=0){
      init=init+1
      if((usersRated_preproc[i,j] - usersRated_preproc[i,start_col[[i]]]) > (diff[i] * 95/100)){
        day[i]=colnames(usersRated_preproc)[j-1] #--> ultimo giorno di vita
        time[i]=init # gg di vita
        break
      }
    }
  }
}
#day
#time 
which(is.na(time)) 
length(which(is.na(time))) #272










#New Cols: Time (=survival time) and Status------------------------------------

## 4 MONTHS -------------------------------------------------------------------

# 4 MONTHS (Const ratings):
# deaths_4 #13048
# zombies_4 #145
usersRated_preproc$time=time_4
usersRated_preproc$status=NA

time_col.index=which(colnames(usersRated_preproc) == "time")
status_col.index=which(colnames(usersRated_preproc) == "status")
for (i in 1:nrow(usersRated_preproc)) {
  if(is.na(usersRated_preproc[i, time_col.index])){
    usersRated_preproc[i, time_col.index]= 2579 #=censoring time per i giochi che non muoiono
    usersRated_preproc[i, status_col.index]=1 #censored
  }
  else
    usersRated_preproc[i, status_col.index]=2 #morti
}

board=usersRated_preproc[, c(1, 2581, 2582)]
survival_4=merge(boardgames_weights_publishers, board, by = "id")
View(survival_4)

write.csv(survival_4, file = "survival_4months.csv", row.names = FALSE)

# count(which(data[,"X2016.10.12"]==0))




# 4 MONTHS (ratings <=2):
# deaths_4_2 #7767 
# zombies_4_2 #314 
usersRated_preproc$time=time_4_2
usersRated_preproc$status=NA

time_col.index=which(colnames(usersRated_preproc) == "time")
status_col.index=which(colnames(usersRated_preproc) == "status")
for (i in 1:nrow(usersRated_preproc)) {
  if(is.na(usersRated_preproc[i, time_col.index])){
    usersRated_preproc[i, time_col.index]= 2579 #=censoring time per i giochi che non muoiono
    usersRated_preproc[i, status_col.index]=1 #censored
  }
  else
    usersRated_preproc[i, status_col.index]=2 #morti
}

board=usersRated_preproc[, c(1, 2581, 2582)]
survival_4_2=merge(boardgames_weights_publishers, board, by = "id")
View(survival_4_2)

write.csv(survival_4_2, file = "survival_4months_2ratings.csv", row.names = FALSE)

# count(which(data[,"X2016.10.12"]==0))




# 4 MONTHS (ratings <=5):
# deaths_4_5 #3490 
# zombies_4_5 #137 
usersRated_preproc$time=time_4_5
usersRated_preproc$status=NA

time_col.index=which(colnames(usersRated_preproc) == "time")
status_col.index=which(colnames(usersRated_preproc) == "status")
for (i in 1:nrow(usersRated_preproc)) {
  if(is.na(usersRated_preproc[i, time_col.index])){
    usersRated_preproc[i, time_col.index]= 2579 #=censoring time per i giochi che non muoiono
    usersRated_preproc[i, status_col.index]=1 #censored
  }
  else
    usersRated_preproc[i, status_col.index]=2 #morti
}

board=usersRated_preproc[, c(1, 2581, 2582)]
survival_4_5=merge(boardgames_weights_publishers, board, by = "id")
View(survival_4_5)

write.csv(survival_4_5, file = "survival_4months_5ratings.csv", row.names = FALSE)

# count(which(data[,"X2016.10.12"]==0))




## 2 MONTHS -------------------------------------------------------------------

# 2 MONTHS (Const ratings):
# deaths_2 #17227
# zombies_2 #263
usersRated_preproc$time=time_2
usersRated_preproc$status=NA

time_col.index=which(colnames(usersRated_preproc) == "time")
status_col.index=which(colnames(usersRated_preproc) == "status")
for (i in 1:nrow(usersRated_preproc)) {
  if(is.na(usersRated_preproc[i, time_col.index])){
    usersRated_preproc[i, time_col.index]= 2579 #=censoring time per i giochi che non muoiono
    usersRated_preproc[i, status_col.index]=1 #censored
  }
  else
    usersRated_preproc[i, status_col.index]=2 #morti
}

board=usersRated_preproc[, c(1, 2581, 2582)]
survival_2=merge(boardgames_weights_publishers, board, by = "id")
View(survival_2)

write.csv(survival_2, file = "survival_2months.csv", row.names = FALSE)

# count(which(data[,"X2016.10.12"]==0))




# 2 MONTHS (ratings <=2):
# deaths_2_2 #8069
# zombies_2_2 #326
usersRated_preproc$time=time_2_2
usersRated_preproc$status=NA

time_col.index=which(colnames(usersRated_preproc) == "time")
status_col.index=which(colnames(usersRated_preproc) == "status")
for (i in 1:nrow(usersRated_preproc)) {
  if(is.na(usersRated_preproc[i, time_col.index])){
    usersRated_preproc[i, time_col.index]= 2579 #=censoring time per i giochi che non muoiono
    usersRated_preproc[i, status_col.index]=1 #censored
  }
  else
    usersRated_preproc[i, status_col.index]=2 #morti
}

board=usersRated_preproc[, c(1, 2581, 2582)]
survival_2_2=merge(boardgames_weights_publishers, board, by = "id")
View(survival_2_2)

write.csv(survival_2_2, file = "survival_2months_2ratings.csv", row.names = FALSE)

# count(which(data[,"X2016.10.12"]==0))




# 2 MONTHS (ratings <=5):
# deaths_2_5 #3558
# zombies_2_5 #138 
usersRated_preproc$time=time_2_5
usersRated_preproc$status=NA

time_col.index=which(colnames(usersRated_preproc) == "time")
status_col.index=which(colnames(usersRated_preproc) == "status")
for (i in 1:nrow(usersRated_preproc)) {
  if(is.na(usersRated_preproc[i, time_col.index])){
    usersRated_preproc[i, time_col.index]= 2579 #=censoring time per i giochi che non muoiono
    usersRated_preproc[i, status_col.index]=1 #censored
  }
  else
    usersRated_preproc[i, status_col.index]=2 #morti
}

board=usersRated_preproc[, c(1, 2581, 2582)]
survival_2_5=merge(boardgames_weights_publishers, board, by = "id")
View(survival_2_5)

write.csv(survival_2_5, file = "survival_2months_5ratings.csv", row.names = FALSE)

# count(which(data[,"X2016.10.12"]==0))




## 1 MONTH --------------------------------------------------------------------

# 1 MONTH (Const ratings):
# deaths_1 #20221
# zombies_1 #352
usersRated_preproc$time=time_1
usersRated_preproc$status=NA

time_col.index=which(colnames(usersRated_preproc) == "time")
status_col.index=which(colnames(usersRated_preproc) == "status")
for (i in 1:nrow(usersRated_preproc)) {
  if(is.na(usersRated_preproc[i, time_col.index])){
    usersRated_preproc[i, time_col.index]= 2579 #=censoring time per i giochi che non muoiono
    usersRated_preproc[i, status_col.index]=1 #censored
  }
  else
    usersRated_preproc[i, status_col.index]=2 #morti
}

board=usersRated_preproc[, c(1, 2581, 2582)]
survival_1=merge(boardgames_weights_publishers, board, by = "id")
View(survival_1)

write.csv(survival_1, file = "survival_1month.csv", row.names = FALSE)

# count(which(data[,"X2016.10.12"]==0))


#%%%%%%%%%%%%%%%%
# Outliers:
survival_1month <- read.csv("C:/Users/Utente/Desktop/NPS/Projects/git_clone/boardgames-nps-project/survival/survival_1month.csv")
View(survival_1month)
covariates.MCD <- c("maxplayers", "playingtime", "Year")
covariates.MCD.ind <- which(colnames(survival_1month) %in% covariates.MCD)
covariates.MCD.ind

fit_MCD <- covMcd(x = survival_1month[,covariates.MCD.ind], alpha = .95, nsamp = 1000)
fit_MCD

ind_out_obs <- setdiff(1:nrow(survival_1month), fit_MCD$best)
length(ind_out_obs)

# plot(fit_MCD,classic=TRUE)

data.out <- survival_1month[-ind_out_obs,]
range(data.out$Year)
range(data.out$playingtime)
range(data.out$maxplayers)

write.csv(survival_1month, file = "survival_final.csv", row.names = FALSE)

#%%%%%%%%%%%%%%%%







# 1 MONTH (ratings <=2):
# deaths_1_2 #8311 
# zombies_1_2 #305
usersRated_preproc$time=time_1_2
usersRated_preproc$status=NA

time_col.index=which(colnames(usersRated_preproc) == "time")
status_col.index=which(colnames(usersRated_preproc) == "status")
for (i in 1:nrow(usersRated_preproc)) {
  if(is.na(usersRated_preproc[i, time_col.index])){
    usersRated_preproc[i, time_col.index]= 2579 #=censoring time per i giochi che non muoiono
    usersRated_preproc[i, status_col.index]=1 #censored
  }
  else
    usersRated_preproc[i, status_col.index]=2 #morti
}

board=usersRated_preproc[, c(1, 2581, 2582)]
survival_1_2=merge(boardgames_weights_publishers, board, by = "id")
View(survival_1_2)

write.csv(survival_1_2, file = "survival_1month_2ratings.csv", row.names = FALSE)

# count(which(data[,"X2016.10.12"]==0))




# 1 MONTH (ratings <=5):
# deaths_1_5 #3595 
# zombies_1_5 #132
usersRated_preproc$time=time_1_5
usersRated_preproc$status=NA

time_col.index=which(colnames(usersRated_preproc) == "time")
status_col.index=which(colnames(usersRated_preproc) == "status")
for (i in 1:nrow(usersRated_preproc)) {
  if(is.na(usersRated_preproc[i, time_col.index])){
    usersRated_preproc[i, time_col.index]= 2579 #=censoring time per i giochi che non muoiono
    usersRated_preproc[i, status_col.index]=1 #censored
  }
  else
    usersRated_preproc[i, status_col.index]=2 #morti
}

board=usersRated_preproc[, c(1, 2581, 2582)]
survival_1_5=merge(boardgames_weights_publishers, board, by = "id")
View(survival_1_5)

write.csv(survival_1_5, file = "survival_1month_5ratings.csv", row.names = FALSE)

# count(which(data[,"X2016.10.12"]==0))




## QUANTILES METHOD -----------------------------------------------------------
usersRated_preproc$time=time
usersRated_preproc$status=NA

time_col.index=which(colnames(usersRated_preproc) == "time")
status_col.index=which(colnames(usersRated_preproc) == "status")
for (i in 1:nrow(usersRated_preproc)) {
  if(!(is.na(usersRated_preproc[i, time_col.index]))){
    usersRated_preproc[i, status_col.index]=2 #morti
  }
}

board=usersRated_preproc[, c(1, 2581, 2582)]
survival=merge(boardgames_weights_publishers, board, by = "id")
View(survival)

write.csv(survival, file = "survival_quantiles_new.csv", row.names = FALSE)

# count(which(data[,"X2016.10.12"]==0))

