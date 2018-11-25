#install.packages('hexbin')
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("randomForest")
#install.packages("scales")
#install.packages("ggcorrplot")
library(scales)
library(randomForest)
library(rpart.plot)
library(rpart)
library(hexbin)
library(tidyverse)
library(ggcorrplot)
library(stringr)

gc()

####  import data  ####
pubg_data = read.csv("~/Documents/DataMining/Proj_PUBG/train_V2.csv", header=TRUE)
pubg_data<-na.omit(pubg_data)
#dim(pubg_data)
##4446965      24

playerJoined_df=pubg_data %>% 
  group_by(matchId) %>% summarise(playerJoined=n())

pubg_data=pubg_data %>% left_join(playerJoined_df, by = "matchId")


sum(pubg_data$rankPoints==0)/nrow(pubg_data)##0.020
sum(pubg_data$rankPoints==-1)/nrow(pubg_data)##0.38
sum(pubg_data$winPoints==0)/nrow(pubg_data)##0.597
sum(pubg_data$winPoints==-1)/nrow(pubg_data)##0
sum(pubg_data$killPoints==0)/nrow(pubg_data)##0.597

##according to the dataset explaination and result above, 
##gona remove rankPoints, winPoints, killPoints


pubg_data$matchType=as.character(pubg_data$matchType)

pubg_data = pubg_data %>% 
  mutate(matchType=ifelse(matchType %in%
                            c("duo","normal-duo","normal-solo",
                              "normal-squad","solo","squad"),
                              paste(matchType,"-tpp",sep = ""),matchType))


pubg_data_crashflare=pubg_data %>% filter(matchType %in% 
                                            c("crashfpp","crashtpp","flarefpp","flaretpp")) %>% 
  separate(matchType,c("matchMode","perspective"),sep = -3) %>% mutate(isNormal=0)

pubg_data_notcrashflare=pubg_data %>%
  anti_join(pubg_data_crashflare, by = "Id")

pubg_data_notcrashflare=pubg_data_notcrashflare %>% 
  separate(matchType, into = c("isNormal", "matchMode","perspective"),
            sep = "-",extra = "merge", fill = "left")

pubg_data=rbind(pubg_data_crashflare, pubg_data_notcrashflare)
pubg_data= pubg_data %>% mutate(isNormal=ifelse(isNormal==0|is.na(isNormal),0,1))

n_distinct(pubg_data$isNormal)
summary(pubg_data$isNormal)

pubg_data=pubg_data %>% mutate(matchMode=factor(matchMode),
                               perspective=factor(perspective),
                               isNormal=factor(isNormal))
pubg_data=pubg_data %>% mutate(distance=walkDistance+swimDistance+rideDistance)

####------------------------------######
pubg_data = pubg_data[,-c(1:3,11,20,29)]
str(pubg_data)

## special event, can have chance only 2 groups in the game, also can get more than 20 kills.
pubg_data %>% filter(isNormal==1|matchMode=="crash"|matchMode=="flare")

## check <=0, some var have so many 0
sort(round(colSums(pubg_data<=0)/nrow(pubg_data),3))

rm(playerJoined_df)
rm(pubg_data_crashflare)
rm(pubg_data_notcrashflare)
#### splite from big dataset into small data set with removing na row ####
#set.seed(100)
#index = sample(1:nrow(pubg_data), size = round(0.1 * nrow(pubg_data)))
#pubg_data_small = pubg_data[index, ]

#write.csv(pubg_data_small, file = "train_V2_small.csv")

#### import small dataset and removing id, groupid, matchid ####

pubg_data_small = read.csv("~/Documents/DataMining/Proj_PUBG/train_V2_small.csv", header=TRUE)
pubg_data_small = pubg_data_small[,-c(1:4)]

####  EDA & outlier check ####
##Assume: assist, assist will usually happen in a duo or squad model,
##Assume: The more assist, the more teamwork, the more chance the team will win the game

pubg_data %>% group_by(matchType) %>% 
  summarise(count=n()) %>% arrange(desc(count))

pubg_data %>% group_by(assists) %>% 
  summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))

pubg_data %>% ggplot(aes(x=assists))+geom_histogram()

pubg_data %>% ggplot(aes(x=assists,y=winPlacePerc,
                         group=assists))+geom_boxplot()

  #pubg_data_small %>% 
   # ggplot(aes(x=assists,y=winPlacePerc))+
  #geom_point(color="blue",position = "jitter",size=0.3,alpha=0.05)+geom_smooth()
  
##normal,no abnormal

#### boosts and heals   ####
## Assume: 
pubg_data %>% group_by(boosts) %>% 
  summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))

pubg_data %>% group_by(heals) %>% 
  summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))

summary(pubg_data$heals)
pubg_data %>% ggplot(aes(x=heals))+geom_histogram()

pubg_data %>% ggplot(aes(x=boosts))+geom_histogram()

pubg_data %>% ggplot(aes(x=heals,y=winPlacePerc,
                         group=heals))+geom_boxplot()

pubg_data %>% ggplot(aes(x=boosts,y=winPlacePerc,
                         group=boosts))+geom_boxplot()

## walkDistance
summary(pubg_data$walkDistance)
pubg_data %>% ggplot(aes(x=walkDistance))+geom_histogram()
## roadDistance
summary(pubg_data$rideDistance)

pubg_data %>% filter(rideDistance>20000)

## swimDistance
summary(pubg_data$swimDistance)
pubg_data %>% filter(swimDistance>2000)
## kill
pubg_data %>% group_by(kills) %>% 
  summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))


View(pubg_data %>% group_by(kills) %>% filter(kills==0&winPlacePerc==1&matchMode=="solo"))
  summarise(count = n(),per=count/nrow(pubg_data))
  
 ## killStreaks     longestKill                DBNOs   
## killStreaks
  View(pubg_data %>% filter(killStreaks>5))
  pubg_data %>% group_by(killStreaks) %>% 
    summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))
  
  pubg_data %>% ggplot(aes(x=killStreaks))+geom_histogram()
  
  pubg_data %>% ggplot(aes(x=killStreaks,y=winPlacePerc,
                           group=killStreaks))+geom_boxplot()  
  
  
##longestkill
  pubg_data %>% group_by(longestKill) %>% 
    summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))
  summary(pubg_data$longestKill)
  pubg_data %>%  ggplot(aes(x=longestKill))+geom_histogram()
  
## very slow
  pubg_data %>%  ggplot(aes(x=longestKill,y=winPlacePerc))+
    geom_point(size=0.3,alpha= 0.2)
  
##  DBNOs  
summary(pubg_data$DBNOs>11)
pubg_data %>% group_by(DBNOs) %>% 
  summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))
pubg_data %>%  ggplot(aes(x=DBNOs))+geom_histogram()
pubg_data %>% ggplot(aes(x=DBNOs,y=winPlacePerc,
                         group=DBNOs))+geom_boxplot()

#headshotKills  
summary(pubg_data$headshotKills)
View(pubg_data %>% filter(headshotKills>20))

pubg_data %>% group_by(headshotKills) %>% 
  summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))

pubg_data %>%  ggplot(aes(x=headshotKills))+geom_histogram()
pubg_data %>% ggplot(aes(x=headshotKills,y=winPlacePerc,
                         group=headshotKills))+geom_boxplot()
#teamKills vehicleDestroys       roadKills 


## revives
pubg_data %>% filter(revives>30)

pubg_data %>% group_by(revives) %>% 
  summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))
pubg_data %>%  ggplot(aes(x=revives))+geom_histogram()
pubg_data %>% ggplot(aes(x=revives,y=winPlacePerc,
                         group=revives))+geom_boxplot()

## interesting to figure out how this happen, weird

summary(pubg_data)

pubg_data %>% filter(kills<20) %>% ggplot(aes(x=kills))+geom_histogram()

pubg_data %>% ggplot(aes(x=kills,y=winPlacePerc,
                         group=kills))+geom_boxplot()

## killPlace
pubg_data %>% filter(winPlacePerc==0)%>% 
  summarise(count = n(),mean(killPlace),median(killPlace))

## the more killplace, the more higher percentage!

pubg_data %>% ggplot(aes(x=killPlace))+geom_histogram()

## perspective
pubg_data %>% group_by(perspective) %>% filter(winPlacePerc==1)%>% 
  summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))

## damageDealt
pubg_data %>% group_by(damageDealt) %>% 
  summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))
summary(pubg_data$damageDealt>200)

pubg_data %>% filter(damageDealt==0&winPlacePerc==1) %>% 
  summarise(count = n(),per=count/nrow(pubg_data))

pubg_data %>% filter(damageDealt<200)%>% ggplot(aes(x=damageDealt))+geom_histogram()

pubg_data %>% ggplot(aes(x=damageDealt,y=winPlacePerc))+geom_point()
##take long to load this scatter plot

## weaponsAcquired
pubg_data %>% group_by(weaponsAcquired) %>% 
  summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))

pubg_data %>% filter(weaponsAcquired>100)
q()
pubg_data %>% ggplot(aes(x=weaponsAcquired))+geom_histogram()

pubg_data %>% ggplot(aes(x=weaponsAcquired,y=winPlacePerc,
                         group=weaponsAcquired))+geom_point()
## matchDuration

pubg_data %>% group_by(matchDuration) %>% 
  summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))

pubg_data %>% ggplot(aes(x=matchDuration,fill=matchMode))+geom_histogram()

## playerJoined
pubg_data %>% group_by(playerJoined) %>% 
  summarise(count = n(),mean(winPlacePerc),median(winPlacePerc))

pubg_data %>% ggplot(aes(x=playerJoined,fill=matchMode))+geom_histogram()

pubg_data %>% ggplot(aes(x=playerJoined,y=winPlacePerc
                         ))+geom_point()
## distance


## winPer ==1


##interesting var

#### split to train and valid datasets ####
set.seed(10)
ind = sample(2, nrow(pubg_data_small), replace=TRUE, prob=c(0.9, 0.1))
train = pubg_data_small[ind==1,]
valid = pubg_data_small[ind==2,]

####  baseline decision tree model ####
tree_train = rpart(winPlacePerc ~ ., data = train,
                     method = "anova",
                     cp = 0.001,
                     maxdepth = 30,
                     minbucket = 1,
                     xval =5)

####  tree summary ####
printcp(tree_train)
tree_train
####  tree plot ####
rpart.plot(tree_train)
#prp(tree_train,type=1,extra = 1, split.font = 1, varlen = -10)


#### funtion for mean abs error ####
mae = function(act,pred){
  mean(abs(act-pred))
}

####  result for both train and valid ####
pred_train = predict(tree_train, train, type = "vector")
mae_train = mae(train$winPlacePerc,pred_train)
mae_train##0.08824

pred_valid = predict(tree_train, valid, type = "vector")
mae_valid = mae(valid$winPlacePerc,pred_valid)
mae_valid##0.08819

#############################################################
## very slow, may try spark rf
rf=randomForest(winPlacePerc ~ .,data= train)
rf
rf_pred = predict(rf, valid,type = "class")

mae_valid = mae(valid$winPlacePerc,rf_pred)
mae_valid

