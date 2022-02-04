# import lib
library(varhandle)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(factoextra)
library(lme4)

# import dataset
getwd()
df <- read.table("germancredit.csv", 
                 header = TRUE,
                 sep = ",")

#investigate numeric data in dataset
df_numeric<- select_if(df, is.numeric)


#plot histogram
par(mfrow=c(2,4))
for(i in 2:8) {
  hist(df_numeric[,i], main=names(df_numeric)[i])
}

# plot boxplot
par(mfrow=c(1,4))
for(i in 1:8) {
  boxplot(df_numeric[,i], main=names(df_numeric)[i])
}


#duration
g1<- ggplot(df, aes(x = as.factor(Default), y = duration, fill = as.factor(Default))) + geom_boxplot() + theme(legend.position = "none")

#amount
g2<-ggplot(df, aes(x = as.factor(Default), y = amount, fill = as.factor(Default))) + geom_boxplot() +
  theme(legend.position = "none")

#AGE
g4<-ggplot(df, aes(x = as.factor(Default), y = age, fill = as.factor(Default))) + 
  geom_boxplot() + theme(legend.position = "none")

ggarrange(g1, g2,g4,
          labels = c("duration", "amount","age"),
          ncol = 3, nrow = 1)

#plot bar graph
g3<-ggplot(df, aes(factor(installment), ..count..)) + 
  geom_bar(aes(fill = as.factor(Default)), position = "dodge") 

g4<-ggplot(df, aes(checkingstatus1, ..count..)) + 
  geom_bar(aes(fill = as.factor(Default)), position = "dodge") 

g5<-ggplot(df, aes(history, ..count..)) + 
  geom_bar(aes(fill = as.factor(Default)), position = "dodge") 

g6<-ggplot(df, aes(purpose, ..count..)) + 
  geom_bar(aes(fill = as.factor(Default)), position = "dodge")

g7<-ggplot(df, aes(savings, ..count..)) + 
  geom_bar(aes(fill = as.factor(Default)), position = "dodge") 

g8<-ggplot(df, aes(others, ..count..)) + 
  geom_bar(aes(fill = as.factor(Default)), position = "dodge")

g9<-ggplot(df, aes(status, ..count..)) + 
  geom_bar(aes(fill = as.factor(Default)), position = "dodge")

g10<-ggplot(df, aes(otherplans, ..count..)) + 
  geom_bar(aes(fill = as.factor(Default)), position = "dodge") 

g11<-ggplot(df, aes(foreign, ..count..)) + 
  geom_bar(aes(fill = as.factor(Default)), position = "dodge") 

g12<-ggplot(df, aes(job, ..count..)) + 
  geom_bar(aes(fill = as.factor(Default)), position = "dodge") 


ggarrange(g3, g4,
          labels = c("Installment", "checkingstatus1"),
          ncol = 1, nrow = 2)
ggarrange(g5, g6,
          labels = c("history", "purpose"),
          ncol = 1, nrow = 2)

ggarrange(g7, g8,
          labels = c("savings", "others"),
          ncol = 1, nrow = 2)

ggarrange(g9, g10,
          labels = c("status", "otherplans"),
          ncol = 1, nrow = 2)

ggarrange(g11,g12,
          labels = c("foreign","job"),
          ncol = 1, nrow = 2)

# correlation plot
correlations <- cor(df_numeric[,1:8])
corrplot(correlations, method="circle",mar=c(1,1,1,1))

# set Default as category instead of numeric
df$Default <- as.factor(df$Default)

#Scatter plot of default loans if it is related to amount and duration
g <- ggplot(df,aes(x=duration,y=amount))
g+geom_point()+geom_smooth(se=FALSE)+
  facet_grid(. ~ Default)

#plot amount vs duration for different job type
g <- ggplot(df,aes(x=duration,y=amount, color=Default))
g+geom_point()+geom_smooth(se=FALSE,method='lm')+
  facet_grid(. ~ job)

##### to review if need to keep####
g <- ggplot(df,aes(x=duration,y=amount, color=job))
g+geom_point()+geom_smooth(se=FALSE,method='lm')+
  facet_grid(. ~ Default)
##################################


#############################   CLUSTERING    ##############################

# do clustering for age, amount, duration

defaultChoosed<-df[c('age','amount','duration')]
defaultPrepared<-log(defaultChoosed+1) 
defaultScaled<-scale(defaultPrepared)

# decide how many clusters
# elbow method
fviz_nbclust(defaultScaled, kmeans, method = "wss")
# gap method
fviz_nbclust(defaultScaled, kmeans, method = "gap_stat")

#fit kmeans on scaled data for cluster =3
# this can be used for predict future data to identify clusters
defaultkmeans<-kmeans(defaultScaled,3)

# PLot clustering
defaultkmeans<-eclust(defaultScaled, "kmeans", hc_metric="euclidean",k=3)

#clusters investigation using scatter plot
fviz_cluster(list(data=df[c("age","amount")], cluster=defaultkmeans$cluster), ellipse.type="norm", geom="point",
             stand=FALSE, palette="jco", ggtheme=theme_classic())

fviz_cluster(list(data=df[c("duration","amount")], cluster=defaultkmeans$cluster), ellipse.type="norm", geom="point",
             stand=FALSE, palette="jco", ggtheme=theme_classic())

fviz_cluster(list(data=df[c("duration","age")], cluster=defaultkmeans$cluster), ellipse.type="norm", geom="point",
             stand=FALSE, palette="jco", ggtheme=theme_classic())

##############################       DATA PREPARATION     ##############################

# convert rest of the columns to factors
head(df,3)
str(df)
cols <-  c('checkingstatus1', 'history', 'purpose', 'savings', 'employ', 'status', 'others', 'property','otherplans','housing','job','tele','foreign')
df[cols] <- lapply(df[cols],factor)
str(df)
#scaling all remaining data
df = df %>% mutate_at(c('duration','amount','installment','residence','cards','liable','age'),~(scale(.) %>% as.vector))
str(df)
                      
                      
#add clusters labels to original data
df$cluster <- as.factor(defaultkmeans$cluster)

# separate 3 dataframe for each clusters: df1=374,df2=258,df3=368 data points
df1 <- filter(df, cluster=="1")
df2 <- filter(df, cluster=="2")
df3 <- filter(df, cluster=="3")

#remove cluster & age labels
df <- subset(df,select = -c(cluster))
df1 <- subset(df1,select = -c(cluster))
df2 <- subset(df2,select = -c(cluster))
df3 <- subset(df3,select = -c(cluster))


# split df, df1,df2,df3 into train and test set
#df
set.seed(1000)
training.idx<-sample(1:nrow(df),nrow(df)*0.7)
train.data<- df[training.idx,]
test.data <- df[-training.idx,]
#df1
training.idx <- sample(1: nrow(df1), nrow(df1)*0.7)
train1.data <- df1[training.idx, ]
test1.data <- df1[-training.idx, ]
#df2
training.idx <- sample(1: nrow(df2), nrow(df2)*0.7)
train2.data <- df2[training.idx, ]
test2.data <- df2[-training.idx, ]
#df3
training.idx <- sample(1: nrow(df3), nrow(df3)*0.7)
train3.data <- df3[training.idx, ]
test3.data <- df3[-training.idx, ]



###################       train model glm with all variables         ###################
glm<-glm(formula = Default ~., family = binomial(), data = train.data)
summary(glm)# checkingstatus1,purpose, amount, employ, installment

#stepwise variable selection using AIC
glm.AIC <-step(glm,direction ='backward')
summary(glm.AIC)

# stepwise variable selection using BIC
glm.BIC <- step(glm, k = log(nrow(train.data)))
summary(glm.BIC)

# glmm select purpose as random factor
glmm.null <- glmer(Default ~ 1+(1|purpose), family = binomial(), data = train.data)
summary(glmm.null)
glmm <- glmer(Default ~ checkingstatus1 + duration  + amount + history +
                savings + employ + installment + others + otherplans + housing + 
                tele + foreign + (1|purpose) , family = binomial(), data = train.data,glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(glmm)
anova(glmm.null,glmm)
# glmm is a better model over null model


###############################    train cluster 1       ###############################
cluster1_glm <-glm(formula = Default ~ ., family = binomial(),data = train1.data)
summary (cluster1_glm)# checkingstatus1,purpose, savings,duration

#stepwise variable selection using AIC
cluster1_glm.AIC <-step(cluster1_glm,direction ='backward')
summary (cluster1_glm.AIC)
# stepwise variable selection using BIC
cluster1_glm.BIC <- step(cluster1_glm, k = log(nrow(train1.data)))
summary(cluster1_glm.BIC)


# glmm select purpose as random factor
glmm1.null <-glmer(Default ~ 1+(1|purpose), family = binomial(), data = train1.data)
summary(glmm.null)

glmm1 <- glmer(Default ~ checkingstatus1 + duration + history + amount + savings + 
                 employ + installment + cards + (1|purpose) , family = binomial(), data = train1.data,glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(glmm1)
anova(glmm1.null,glmm1)
# glmm1 is a better model over null model

#############################       train cluster 2      ################################
cluster2_glm <-glm(formula = Default ~ ., family = binomial(),data = train2.data)
summary(cluster2_glm)# checkingstatus1,duration, purpose,property

#stepwise variable selection using AIC
cluster2_glm.AIC <-step(cluster2_glm,direction ='backward')
summary (cluster2_glm.AIC)

# stepwise variable selection using BIC
cluster2_glm.BIC <- step(cluster2_glm, k = log(nrow(train2.data)))
summary(cluster2_glm.BIC)

# glmm select purpose as random factor
glmm2.null <-glmer(Default ~ 1+(1|purpose), family = binomial(), data = train2.data)
summary(glmm2.null)
glmm2 <- glmer(Default ~ checkingstatus1 + duration + history + amount + 
                 savings + employ + status + others + property + otherplans + 
                 job + foreign + (1|purpose) , family = binomial(), data = train2.data,glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(glmm2)
anova(glmm2.null,glmm2)
# glmm2 is a better model over null model

#############################        train cluster 3        ################################
cluster3_glm <-glm(formula = Default ~ ., family = binomial(),data = train3.data)
summary(cluster3_glm)# checkingstatus1,history, purpose, savings, otherplans

#stepwise variable selection using AIC 
cluster3_glm.AIC<-step(cluster3_glm,direction ='backward')
summary (cluster3_glm.AIC)

# stepwise variable selection using BIC
cluster3_glm.BIC <-step(cluster3_glm, k = log(nrow(train3.data)))
summary (cluster3_glm.BIC)

# glmm select purpose as random factor
glmm3.null <-glmer(Default ~ 1+(1|purpose), family = binomial(), data = train3.data)
summary(glmm3.null)
glmm3 <- glmer(Default ~ checkingstatus1 + history + savings + status + 
                 otherplans + housing + job + (1|purpose) , family = binomial(), data = train3.data,glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(glmm3)
anova(glmm3.null,glmm3)
# glmm3 is a better model over null model


################################   FUNCTIONS  ####################################


## determine optimal cutoff probability
#cost function assuming that it cost bank 5 times more for a missed bad loan than a missed good loan
costfunction = function(obs, pred.p, pcut){
  weight1 = 5   # define the weight for false negative, bank missed a bad loan
  weight0 = 1    # define the weight for false positive, bank missed a good loan
  c1 = (obs==1)&(pred.p<pcut)    # count for (FN)
  c0 = (obs==0)&(pred.p>=pcut)   # count for (FP)
  cost = mean(weight1*c1 + weight0*c0)  # misclassification with weight
  return(cost) # you have to return to a value when you write R functions
}
#search probability from 0.01 to 1 at 0.01 step
p.seq = seq(0.001, 1, 0.001) 



##function to fit the models
fitfunction = function(model,testdata){
  Pred <- predict(model,newdata = testdata, type ="response")
  #choose cutoff prob
  cost = rep(1, length(p.seq))
  for(i in 1:length(p.seq)){ 
    cost[i] = costfunction(obs = testdata$Default, pred.p = Pred, pcut = p.seq[i])  
  }
  plot(p.seq,cost)
  optimal_cut_glm = p.seq[which(cost==min(cost))]
  cutoff <-max(optimal_cut_glm)
  #confusion matrix
  tab<-table(testdata$Default,(Pred > cutoff)*1,dnn = c("True", "Predicted")) 
  #accuracy
  y_pred_glm_num <-ifelse(Pred > cutoff,1,0)
  acc <- mean(y_pred_glm_num == testdata$Default)
  list <-list(tab,acc,cutoff, min(cost))
  return(list)
}

 
############################       PREDICTION      #######################################

# Output data will give table, accuracy, cutoff prob, cost and repeat for each model

#Non clustered dataset
model_fit = list(glm , glm.AIC, glm.BIC, glmm)
output<-list()
for(i in 1:length(model_fit)){
  output[[i]] <- fitfunction(model=model_fit[[i]],testdata =test.data)
  
}
output
#glmm perform better for full model for cost and accuracy


#cluster1
model_fit = list(cluster1_glm , cluster1_glm.AIC, cluster1_glm.BIC, glmm1)
output_c1 <- list()
for(i in 1:length(model_fit)){
  output_c1[[i]] <- fitfunction(model=model_fit[[i]],testdata =test1.data)
  
}
output_c1
#cluster1_glm perform best for cost and accuracy

#cluster2
model_fit = list(cluster2_glm , cluster2_glm.AIC, cluster2_glm.BIC, glmm2)
output_c2 <- list()
for(i in 1:length(model_fit)){
  output_c2[[i]] <- fitfunction(model=model_fit[[i]],testdata =test2.data)
  
}
output_c2
#cluster2_glm.BIC perform best for c2 in cost , slight reduction in accuracy but lower cost
#cluster2_glm perform best for c2 in accuracy

#cluster3
model_fit = list(cluster3_glm , cluster3_glm.AIC, cluster3_glm.BIC, glmm3)
output_c3 <- list()
for(i in 1:length(model_fit)){
  output_c3[[i]] <- fitfunction(model=model_fit[[i]],testdata =test3.data)
  
}
output_c3
#cluster3_glm perform best for c3 in cost , slight reduction in accuracy but lower cost
#glmm3 perform best for accuracy

#############################      EVALUATION       ################################


# selecting model based on MAXIMISING accuracy 
#cluster 1: cluster1_glm
#cluster 2: cluster2_glm
#cluster 3: glmm3
overall_acc <- (length(test1.data) * output_c1[[1]][[2]][1] 
+ length(test2.data) * output_c2[[1]][[2]][1] 
+ length(test3.data) * output_c3[[4]][[2]][1])/(length(test1.data)+length(test2.data)+length(test3.data))
overall_acc
#non-clustered: glmm
output[[4]][[2]][1]
## overall clustered data perform slightly better than non-clusters###

# selecting model based on MINIMISING cost 
#cluster 1: cluster1_glm
#cluster 2: cluster2_glm.BIC
#cluster 3: cluster3_glm
overall_cost <- (length(test1.data) * output_c1[[1]][[4]][1] 
                + length(test2.data) * output_c2[[3]][[4]][1] 
                + length(test3.data) * output_c3[[1]][[4]][1])/(length(test1.data)+length(test2.data)+length(test3.data))

overall_cost
#non-clustered: glmm
output[[4]][[4]][1]
## overall clustered data perform slightly better than non-clusters when lowering cost to bank

######################################################################################
#RECOMMENDED METHOD FOR PREDICTING NEW DATA
#1) use model: defaultkmeans to cluster new data
#2) select model for fitting new data based on the predicted cluster
#3) use associated cutoff prob to determine if new data will default

############################ FINALIZED MODEL (COST) ########################################
# fit Cluster1
cluster1_final <-glm(formula = Default ~ ., family = binomial(),data = df1)
summary (cluster1_final)
# fit Cluster2
cluster2_temp <-glm(formula = Default ~ ., family = binomial(),data = df2)
cluster2_final <-step(cluster2_temp, k = log(nrow(df2)))
summary (cluster2_final)
# fit Cluster3
cluster3_final <-glm(formula = Default ~ ., family = binomial(),data = df3)
summary (cluster3_final)

####cut off prob ######
output1 <- fitfunction(model=cluster1_final,testdata = df1)
output1
cutoffP1 <- output1 [[3]][1]
cutoffP1  ## 0.261

output2 <- fitfunction(model=cluster2_final,testdata = df2)
output2
cutoffP2 <- output2 [[3]][1]
cutoffP2  ## 0.117

output3 <- fitfunction(model=cluster3_final,testdata = df3)
output3
cutoffP3 <- output3 [[3]][1]
cutoffP3  ## 0.148

############ overall cost ###########
final_cost <- (length(df1) * output1[[4]][1] 
                 + length(df2) * output2[[4]][1] 
                 + length(df3) * output3[[4]][1])/(length(df1)+length(df2)+length(df3))

final_cost
