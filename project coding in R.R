rm(list=ls())
setwd("C:/Users/Samsung/Desktop/project")
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','ggplot2','readxl')

#install.packages(x)
lapply(x, require, character.only = TRUE)

library(readxl)
data <- read_excel("Absenteeism.xls")
View(data)

# See thew dimensions
dim(data)

#Strcucture of the data
str(data)
#head(data)
##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
missing_val
write.csv(missing_val, "Miisingfile.csv", row.names = F)



# Rename  column names
names(data)[1]<-"ID"
names(data)[2]<-"Reasonforabsence"
names(data)[3]<-"Monthofabsence"
names(data)[4]<-"Dayofweek"
names(data)[5]<-"Seasons"
names(data)[6]<-"Transportationexpense"
names(data)[7]<-"Distancefromresidence"
names(data)[8]<-"Servicetime"
names(data)[9]<-"Age"
names(data)[10]<-"WorkloadAverage"
names(data)[11]<-"Hittarget"
names(data)[12]<-"Disciplinaryfailure"
names(data)[13]<-"Education"
names(data)[14]<-"Son"
names(data)[15]<-"Socialdrinker"
names(data)[16]<-"Socialsmoker"
names(data)[17]<-"Pet"
names(data)[18]<-"Weight"
names(data)[19]<-"Height"
names(data)[20]<-"Bodymassindex"
names(data)[21]<-"Absenteesmtimeinhours"
#Missing value imputation

colnames(data)
#KNN Imputation
library("DMwR")
data <- as.data.frame(data)
data = knnImputation(data, k = 3)
sum(is.na(data))


colnames(data)
View(data)
############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
continuous_vars=c("Transportationexpense",
                  "Distancefromresidence","Servicetime","Age",                  
                  "WorkloadAverage","Hittarget","Weight",               
                  "Height","Bodymassindex","Absenteesmtimeinhours")
continuous_vars
Categorical_vars=c('ID','Reasonforabsence','Monthofabsence','Dayofweek','Seasons',
                   'Disciplinaryfailure','Socialsmoker','Socialdrinker','Son','Pet','Education')
Categorical_vars
colnames(data)
library(ggplot2)

for (i in 1:length(continuous_vars))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (continuous_vars[i]), x = "Absenteesmtimeinhours"), data = subset(data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=continuous_vars[i],x="Absenteesmtimeinhours")+
           ggtitle(paste("Box plot of Absenteeism for",continuous_vars[i])))
}

# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
gridExtra::grid.arrange(gn6,gn7,gn4,gn3,ncol=4)
gridExtra::grid.arrange(gn8,gn9,gn10,ncol=3)


# # #loop to remove from all variables
for(i in continuous_vars){
  print(i)
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  print(length(val))
  data = data[which(!data[,i] %in% val),]
  data[,i][data[,i] %in% val] = NA
}

#for(i in continuous_vars){
 # val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  #print(length(val))
  #data[,i][data[,i] %in% val] = NA
#}
table(is.na(data))
#Imputing missing values
data=knnImputation(data,k=3)

##################################Feature Selection################################################
## Correlation Plot 
library(corrgram) 
corrgram(data[,continuous_vars], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
## ANOVA test for Categprical variable
summary(aov(formula = Absenteesmtimeinhours~ID,data = data))
summary(aov(formula = Absenteesmtimeinhours~Reasonforabsence,data = data))
summary(aov(formula = Absenteesmtimeinhours~Monthofabsence,data = data))
summary(aov(formula = Absenteesmtimeinhours~Dayofweek,data = data))
summary(aov(formula = Absenteesmtimeinhours~Seasons,data = data))
summary(aov(formula = Absenteesmtimeinhours~Disciplinaryfailure,data = data))
summary(aov(formula = Absenteesmtimeinhours~Education,data = data))
summary(aov(formula = Absenteesmtimeinhours~Socialdrinker,data = data))
summary(aov(formula = Absenteesmtimeinhours~Socialsmoker,data = data))
summary(aov(formula = Absenteesmtimeinhours~Son,data = data))
summary(aov(formula = Absenteesmtimeinhours~Pet,data = data))


## Dimension Reduction
data = subset(data, select = -c(Weight))
dim(data)
#*******************************Feature Scalling****************************
#Update coninuou and categorical variables
continuous_vars=c("Transportationexpense",
                  "Distancefromresidence","Servicetime","Age",                  
                  "WorkloadAverage","Hittarget",               
                  "Height","Bodymassindex")
continuous_vars
colnames(continuous_vars)
Categorical_vars=c('ID','Reasonforabsence','Monthofabsence','Dayofweek','Seasons',
                   'Disciplinaryfailure','Socialsmoker','Socialdrinker','Son','Pet','Education')
Categorical_vars
#for(i in cnames){
# print(i)
#  data[,i] = (data[,i] - min(data[,i]))/(max(data[,i] - min(data[,i])))
#}

# #Standardisation
for(i in continuous_vars){
  print(i)
  data[,i] = (data[,i] - mean(data[,i]))/sd(data[,i])
}


View(data)



##############################Feature Splitting################################


#Divide data into train and test using stratified sampling method
set.seed(123)
train.index = sample(1:nrow(data), 0.8 * nrow(data))
train = data[ train.index,]
test  = data[-train.index,]
#**********************Model Development Phase*********************************
dim(train)
dim(test)
##Decision tree for classification
#Develop Model on training data
fit_DT = rpart(Absenteesmtimeinhours ~., data = train, method = "anova")

#Summary of DT model
summary(fit_DT)

#write rules into disk
write(capture.output(summary(fit_DT)), "Rules.txt")

#Lets predict for training data
pred_DT_train = predict(fit_DT, train[,names(test) != "Absenteesmtimeinhours"])

#Lets predict for training data
pred_DT_test = predict(fit_DT,test[,names(test) != "Absenteesmtimeinhours"])


# For training data 
print(postResample(pred = pred_DT_train, obs = train[,20]))

# For testing data 
print(postResample(pred = pred_DT_test, obs = test[,20]))


#************************************************Linear Regression*****************************************#
set.seed(123)

#Develop Model on training data
fit_LR = lm(Absenteesmtimeinhours ~ ., data = train)

#Lets predict for training data
pred_LR_train = predict(fit_LR, train[,names(test) != "Absenteesmtimeinhours"])

#Lets predict for testing data
pred_LR_test = predict(fit_LR,test[,names(test) != "Absenteesmtimeinhours"])

# For training data 
print(postResample(pred = pred_LR_train, obs = train[,20]))

# For testing data 
print(postResample(pred = pred_LR_test, obs = test[,20]))


#********************************************************Random Forest***************************************************#

set.seed(123)

#Develop Model on training data
fit_RF = randomForest(Absenteesmtimeinhours~., data = train)

#Lets predict for training data
pred_RF_train = predict(fit_RF, train[,names(test) != "Absenteesmtimeinhours"])

#Lets predict for testing data
pred_RF_test = predict(fit_RF,test[,names(test) != "Absenteesmtimeinhours"])

# For training data 
print(postResample(pred = pred_RF_train, obs = train[,20]))

# For testing data 
print(postResample(pred = pred_RF_test, obs = test[,20]))


#**************************************************XGBoost*********************************************#

set.seed(123)

#Develop Model on training data
fit_XGB = gbm(Absenteesmtimeinhours~., data = train, n.trees = 300, interaction.depth = 2)

#Lets predict for training data
pred_XGB_train = predict(fit_XGB, train[,names(test) != "Absenteesmtimeinhours"], n.trees = 300)

#Lets predict for testing data
pred_XGB_test = predict(fit_XGB,test[,names(test) != "Absenteesmtimeinhours"], n.trees = 300)

# For training data 
print(postResample(pred = pred_XGB_train, obs = train[,20]))

# For testing data 
print(postResample(pred = pred_XGB_test, obs = test[,20]))



