library(tidytuesdayR)
library(bggAnalytics)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(tidyr)
library(dplyr)
library(exactRankTests)
library(coin)



## Import of the data ----
boardgames=tidytuesdayR::tt_load('2022-01-25')
ratings=boardgames$ratings
details=boardgames$details




## Histograms ----
#hist(ratings$average)
#vars=bgg_variables




## Cleaning ----
dim(ratings) #21831    10
dim(details) #21631    23  --> we can see that there's a different number of games


#Are there games repeated more than once in the ratings dataset?
repeated_games=any(duplicated(ratings$id))
if (repeated_games) {
  cat("We have repeated games.\n")
} else {
  cat("We have not repeated games.\n")
} #We have not repeated games --> So we have more games in the ratings dataset


#Remove unuseful cols (from both the 2 datasets):
ratings=ratings[, -c(1, 3, 4, 7, 9, 10)]
details=details[, -c(1, 4, 13, 15, 16)]


#Rename the cols (of both the 2 datasets):
new_names_ratings=c("ID", "Rank", "Ratings", "Users_rated")
colnames(ratings)=new_names_ratings
new_names_details=c("ID", "Name", "Year", "Min_players", "Max_players", "Playing_time", "Min_playing_time", "Max_playing_time", "Min_age", "Category", "Family", "Designer", "Artist", "Publisher", "Owned", "Trading", "Wanting", "Wishing")
colnames(details)=new_names_details  


#Merge the 2 datsets:
boardgames=merge(ratings, details, by = "ID")
dim(boardgames) #21631    21
str(boardgames)
unique(boardgames$Year) #pos years= a.C.; neg years= b.C.




##CATEGORY----
#Create a vector containing all the categories:
boardgames_cat=as.data.frame(boardgames)
new_column_name=c()
for(i in 1:dim(boardgames_cat)[1]) { #length(boardgames$Category)=dim(boardgames)[1]=21631
  Category_i_no_brackets <- gsub("\\[|\\]|'", "", boardgames_cat$Category[i])
  Category_i_split <- strsplit(Category_i_no_brackets, ", ")
  Category_i_values <- unlist(Category_i_split)
  for (j in 1:length(Category_i_values)) {
      if(!is.na(Category_i_values[j])){
        new_column_name=c(new_column_name, Category_i_values[j])
    }
  }
}
catname=na.omit(unique(new_column_name))
catname
length(catname) #84



#Add these categories as new cols in our dataset:
for (i in 1:length(catname)) {
  boardgames_cat=mutate(boardgames_cat, !!catname[i]:=0)
}

#Assign binary values to each new col wrt the presence of a category in a game:
for(i in 1:dim(boardgames_cat)[1]) { #length(boardgames_noNA$Category)=dim(boardgames_noNA)[1]=21631
  Category_i_no_brackets <- gsub("\\[|\\]|'", "", boardgames_cat$Category[i])
  Category_i_split <- strsplit(Category_i_no_brackets, ", ")
  Category_i_values <- unlist(Category_i_split)
  for (k in 1:length(catname)) {
    for (j in 1:length(Category_i_values)) {
      if(!is.na(Category_i_values[j]) && !is.na(catname[k]) && Category_i_values[j]==catname[k]){
        boardgames_cat[i,k+21]=1
        next
      }
    }
  }
} 
View(boardgames_cat)




#How can I divide the categories in a more clever way?:
colsum=colSums(boardgames_cat[, c(22:105)])
colsum
sort(colsum)

#I delete cols for particular types of war:
boardgames_cat=boardgames_cat[, -c(52, 67, 68, 70, 88, 89, 90, 102, 103)]

#I create a dataset containing only the categories and Ratings:
boardgames_onlycat=boardgames_cat[, -c(1,2,4:21)]

#Check if all col contain at least one value:
sumcols=colSums(boardgames_cat[, c(22:96)])
sumcols
sort(sumcols)


#Mann-Whitney U test for all categories:
results=list()
for (i in names(boardgames_onlycat)[2:ncol(boardgames_onlycat)]) {
  test_result=wilcox.test(boardgames_onlycat$Ratings ~ boardgames_onlycat[[i]])
  results[[i]]=test_result
}
print(results) #kepp categories with a p-val significanlty low (at a level of 5%??)
# #Remove:
# Travel --> p-value = 0.2579 |34
#    (Bluffing --> p-value = 0.01197)
# `Collectible Components` --> p-value = 0.4584 |39
#    (Maze --> p-value = 0.01738)
# `American West` --> p-value = 0.2424 |46
#    (Horror --> p-value = 0.0003253)
#    (`Novel-based` --> p-value = 0.00807)
# Deduction --> p-value = 0.5518 |56
#    (Puzzle --> p-value = 0.0004127)
#    (Pirates --> p-value = 0.0002466)
# `Murder/Mystery` --> p-value = 0.397 |71
# Prehistoric --> p-value = 0.9147 |73
# Sports --> p-value = 0.1842 |74
# `Game System` --> p-value = 0.2612 |75
#    (`Spies/Secret Agents` --> 0.04195)
# Medical --> p-value = 0.7826 |79
# Mafia --> p-value = 0.731 |80
# Zombies --> p-value = 0.9062 |81
# `Comic Book / Strip` --> p-value = 0.2334 |82
#    (Music --> p-value = 0.004164)
# Arabian --> p-value = 0.3267 |89
# Religious --> p-value = 0.1802 |91
#    (`Video Game Theme` --> p-value = 0.03805)
# `Fan Expansion` --> p-value = 0.08338 |96
#
# -15 categories

#Remove the selected categories:
boardgames_selcat=boardgames_cat[, -c(34, 39, 46, 56, 71, 73, 74, 76, 79, 80, 81, 82, 89, 91, 96)]
View(boardgames_selcat)


#Rename certain cols:
names(boardgames_selcat)[25] ="Card_Game"
names(boardgames_selcat)[27] ="Abstract_Strategy"
names(boardgames_selcat)[30] ="Territory_Building"
names(boardgames_selcat)[37] ="Science_Fiction"
names(boardgames_selcat)[40] ="Print_and_Play"
names(boardgames_selcat)[44] ="City_Building"
names(boardgames_selcat)[47] ="Space_Exploration"
names(boardgames_selcat)[52] ="Novel_based"
names(boardgames_selcat)[53] ="Word_Game"
names(boardgames_selcat)[54] ="Aviation_Flight"
names(boardgames_selcat)[55] ="Movies_TV_Radio_theme"
names(boardgames_selcat)[56] ="Party_Game"
names(boardgames_selcat)[59] ="Real_time"
names(boardgames_selcat)[61] ="Industry_Manufacturing"
names(boardgames_selcat)[62] ="Age_of_Reason"
names(boardgames_selcat)[65] ="Childrens_Game"
names(boardgames_selcat)[68] ="Action_Dexterity"
names(boardgames_selcat)[69] ="Spies_Secret_Agents"
names(boardgames_selcat)[72] ="Post_Napoleonic"
names(boardgames_selcat)[78] ="Pike_and_Shot"
names(boardgames_selcat)[79] ="Video_Game_Theme"
names(boardgames_selcat)[80] ="Mature_Adult"
names(boardgames_selcat)[81] ="Expansion_for_Base_game"







##MIN/MAX Players----
#add a col for mean(min,max):
boardgames_selcat$Mean_players=floor((boardgames_selcat$Min_players+boardgames_selcat$Max_players)/2)
#NB: in realtà esiste una variabile in bgg_variables che si chiama Best_Players --> sarebbe meglio usare questa? E' fattibile scaricarla?
View(boardgames_selcat)




##WEIGHT----
#Import weight:
weights=read.csv("C:/Users/Utente/Desktop/NPS/Projects/weights.csv")

#Remove all cols a part from ID and weight:
weights=weights[, c(2,5)]

#I add the variable Weight=complexity of the game:
boardgames_selcat=merge(boardgames_selcat, weights, by = "ID")
View(boardgames_selcat)
dim(boardgames_selcat) #21592    83
































# ##FAMILY --> si tratta di 3239 valori... ha veramente senso? Spero di no :( ---- 
# #Create a vector containing all the families:
# new_column_name=c()
# for(i in 1:dim(boardgames_noNA)[1]) { #length(boardgames$Category)=dim(boardgames)[1]=21631
#   Family_i_no_brackets <- gsub("\\[|\\]|'", "", boardgames_noNA$Family[i])
#   Family_i_split <- strsplit(Family_i_no_brackets, ", ")
#   Family_i_values <- unlist(Family_i_split)
#   for (j in 1:length(Family_i_values)) {
#     found=TRUE
#     if(length(new_column_name)==0){
#       found=FALSE
#     } else{
#       for (k in 1:length(new_column_name)) {
#         if(!is.na(Family_i_values[j]) && !is.na(new_column_name[k]) && Family_i_values[j]==new_column_name[k]){
#           found=TRUE
#           break
#         } else {
#           found=FALSE
#         }
#       }
#     }
#     if (!found) {
#       new_column_name=c(new_column_name, Family_i_values[j])
#     }
#   }
# }
# new_column_name
# length(new_column_name) #3239
# length(unique(new_column_name)) #3239
# 
# #Add these families as new cols in our dataset:
# for (i in 1:length(new_column_name)) {
#   boardgames_noNA=mutate(boardgames_noNA, !!new_column_name[i]:=NA)
# }
# 
# #Assign binary values to each new col wrt the presence of a Family in a game:
# names_cat=names(boardgames_noNA)[22:104]
# for(i in 1:dim(boardgames_noNA)[1]) { #length(boardgames_noNA$Family)=dim(boardgames_noNA)[1]=21631
#   Family_i_no_brackets <- gsub("\\[|\\]|'", "", boardgames_noNA$Family[i])
#   Family_i_split <- strsplit(Family_i_no_brackets, ", ")
#   Family_i_values <- unlist(Family_i_split)
#   for (k in 1:length(names_cat)) {
#     found=FALSE
#     for (j in 1:length(Family_i_values)) {
#       if(!is.na(Family_i_values[j]) && !is.na(names_cat[k]) && Family_i_values[j]==names_cat[k]){
#         found=TRUE
#         boardgames_noNA[i,k+21]=1
#         next
#       }
#     }
#     if(!found){
#       boardgames_noNA[i,k+21]=0
#     }
#   }
# } #aiuto ci mette una vita-->da ottimizzare!!!! PERO' SEMBRA FUNZIONARE
# View(boardgames_noNA)




# ##DESIGNER --> qui abbiamo 6869 valori... PANICO----
# #Create a vector containing all the categories:
# new_column_name=c()
# for(i in 1:dim(boardgames_noNA)[1]) { #length(boardgames$Designer)=dim(boardgames)[1]=21631
#   Designer_i_no_brackets <- gsub("\\[|\\]|'", "", boardgames_noNA$Designer[i])
#   Designer_i_split <- strsplit(Designer_i_no_brackets, ", ")
#   Designer_i_values <- unlist(Designer_i_split)
#   for (j in 1:length(Designer_i_values)) {
#     found=TRUE
#     if(length(new_column_name)==0){
#       found=FALSE
#     } else{
#       for (k in 1:length(new_column_name)) {
#         if(!is.na(Designer_i_values[j]) && !is.na(new_column_name[k]) && Designer_i_values[j]==new_column_name[k]){
#           found=TRUE
#           break
#         } else {
#           found=FALSE
#         }
#       }
#     }
#     if (!found) {
#       new_column_name=c(new_column_name, Designer_i_values[j])
#     }
#   }
# }
# new_column_name
# length(new_column_name) #6869
# length(unique(new_column_name)) #6869
# 
# #Add these categories as new cols in our dataset:
# for (i in 1:length(new_column_name)) {
#   boardgames_noNA=mutate(boardgames_noNA, !!new_column_name[i]:=NA)
# }
# 
# #Assign binary values to each new col wrt the presence of a Designer in a game:
# names_cat=names(boardgames_noNA)[22:104]
# for(i in 1:dim(boardgames_noNA)[1]) { #length(boardgames_noNA$Designer)=dim(boardgames_noNA)[1]=21631
#   Designer_i_no_brackets <- gsub("\\[|\\]|'", "", boardgames_noNA$Designer[i])
#   Designer_i_split <- strsplit(Designer_i_no_brackets, ", ")
#   Designer_i_values <- unlist(Designer_i_split)
#   for (k in 1:length(names_cat)) {
#     found=FALSE
#     for (j in 1:length(Designer_i_values)) {
#       if(!is.na(Designer_i_values[j]) && !is.na(names_cat[k]) && Designer_i_values[j]==names_cat[k]){
#         found=TRUE
#         boardgames_noNA[i,k+21]=1
#         next
#       }
#     }
#     if(!found){
#       boardgames_noNA[i,k+21]=0
#     }
#   }
# } #aiuto ci mette una vita-->da ottimizzare!!!! PERO' SEMBRA FUNZIONARE
# View(boardgames_noNA)




# ##ARTIST --> qui abbiamo 9815 valori... Lasciamo stare----
# #Create a vector containing all the categories:
# new_column_name=c()
# for(i in 1:dim(boardgames_noNA)[1]) { #length(boardgames$Artist)=dim(boardgames)[1]=21631
#   Artist_i_no_brackets <- gsub("\\[|\\]|'", "", boardgames_noNA$Artist[i])
#   Artist_i_split <- strsplit(Artist_i_no_brackets, ", ")
#   Artist_i_values <- unlist(Artist_i_split)
#   for (j in 1:length(Artist_i_values)) {
#     found=TRUE
#     if(length(new_column_name)==0){
#       found=FALSE
#     } else{
#       for (k in 1:length(new_column_name)) {
#         if(!is.na(Artist_i_values[j]) && !is.na(new_column_name[k]) && Artist_i_values[j]==new_column_name[k]){
#           found=TRUE
#           break
#         } else {
#           found=FALSE
#         }
#       }
#     }
#     if (!found) {
#       new_column_name=c(new_column_name, Artist_i_values[j])
#     }
#   }
# }
# new_column_name
# length(new_column_name) #9815
# length(unique(new_column_name)) #9815
# 
# #Add these categories as new cols in our dataset:
# for (i in 1:length(new_column_name)) {
#   boardgames_noNA=mutate(boardgames_noNA, !!new_column_name[i]:=NA)
# }
# 
# #Assign binary values to each new col wrt the presence of a Artist in a game:
# names_cat=names(boardgames_noNA)[22:104]
# for(i in 1:dim(boardgames_noNA)[1]) { #length(boardgames_noNA$Artist)=dim(boardgames_noNA)[1]=21631
#   Artist_i_no_brackets <- gsub("\\[|\\]|'", "", boardgames_noNA$Artist[i])
#   Artist_i_split <- strsplit(Artist_i_no_brackets, ", ")
#   Artist_i_values <- unlist(Artist_i_split)
#   for (k in 1:length(names_cat)) {
#     found=FALSE
#     for (j in 1:length(Artist_i_values)) {
#       if(!is.na(Artist_i_values[j]) && !is.na(names_cat[k]) && Artist_i_values[j]==names_cat[k]){
#         found=TRUE
#         boardgames_noNA[i,k+21]=1
#         next
#       }
#     }
#     if(!found){
#       boardgames_noNA[i,k+21]=0
#     }
#   }
# } #aiuto ci mette una vita-->da ottimizzare!!!! PERO' SEMBRA FUNZIONARE
# View(boardgames_noNA)




# ##PUBLISHER --> qui abbiamo 5354 valori... Aiuto----
# #Create a vector containing all the categories:
# new_column_name=c()
# for(i in 1:dim(boardgames_noNA)[1]) { #length(boardgames$Publisher)=dim(boardgames)[1]=21631
#   Publisher_i_no_brackets <- gsub("\\[|\\]|'", "", boardgames_noNA$Publisher[i])
#   Publisher_i_split <- strsplit(Publisher_i_no_brackets, ", ")
#   Publisher_i_values <- unlist(Publisher_i_split)
#   for (j in 1:length(Publisher_i_values)) {
#     found=TRUE
#     if(length(new_column_name)==0){
#       found=FALSE
#     } else{
#       for (k in 1:length(new_column_name)) {
#         if(!is.na(Publisher_i_values[j]) && !is.na(new_column_name[k]) && Publisher_i_values[j]==new_column_name[k]){
#           found=TRUE
#           break
#         } else {
#           found=FALSE
#         }
#       }
#     }
#     if (!found) {
#       new_column_name=c(new_column_name, Publisher_i_values[j])
#     }
#   }
# }
# new_column_name
# length(new_column_name) #5354
# length(unique(new_column_name)) #5354
# 
# #Add these categories as new cols in our dataset:
# for (i in 1:length(new_column_name)) {
#   boardgames_noNA=mutate(boardgames_noNA, !!new_column_name[i]:=NA)
# }
# 
# #Assign binary values to each new col wrt the presence of a Publisher in a game:
# names_cat=names(boardgames_noNA)[22:104]
# for(i in 1:dim(boardgames_noNA)[1]) { #length(boardgames_noNA$Publisher)=dim(boardgames_noNA)[1]=21631
#   Publisher_i_no_brackets <- gsub("\\[|\\]|'", "", boardgames_noNA$Publisher[i])
#   Publisher_i_split <- strsplit(Publisher_i_no_brackets, ", ")
#   Publisher_i_values <- unlist(Publisher_i_split)
#   for (k in 1:length(names_cat)) {
#     found=FALSE
#     for (j in 1:length(Publisher_i_values)) {
#       if(!is.na(Publisher_i_values[j]) && !is.na(names_cat[k]) && Publisher_i_values[j]==names_cat[k]){
#         found=TRUE
#         boardgames_noNA[i,k+21]=1
#         next
#       }
#     }
#     if(!found){
#       boardgames_noNA[i,k+21]=0
#     }
#   }
# } #aiuto ci mette una vita-->da ottimizzare!!!! PERO' SEMBRA FUNZIONARE
# View(boardgames_noNA)