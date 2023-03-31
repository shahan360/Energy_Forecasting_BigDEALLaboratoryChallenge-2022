#BigDeal Challenge model development
install.packages("ipred", repos="http://R-Forge.R-project.org")
install.packages("fpp")
install.packages("forecast")
install.packages("ffp3")
install.packages("tidyverse")

library(readxl)
library(tree)
library(readxl)
library(rpart)
library(ipred)
library(tidyverse)
library(npreg)
library(fpp)
library(gbm)
library(randomForest)

Qualifying_Math_Data <- read_excel("C:/Users/Shashank/Desktop/INDE 6360/BigDEAL Challenge 2022 Qualifying Match/Qualifying Math Data.xlsx")
df3<-data.frame(Qualifying_Math_Data)



rmse_reg <- function(model_obj, testing = NULL, target = NULL) {
  #Calculates rmse for a regression decision tree
  #Arguments:
  # testing - test data set
  # target  - target variable (length 1 character vector)
  yhat <- predict(model_obj, newdata = testing)
  actual <- testing[[target]]
  sqrt(mean((yhat-actual)^2))
}



#create a date column 
#df3$Date<-as.Date(with(df3,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")
#head(df3)

#subset the data for months May through October
library(plyr)
winter_data1<-subset(df3, subset = df3$Month >= 11 & df3$Year< 2007)
winter_data2<-subset(df3, subset = df3$Month <= 4 & df3$Year< 2007)
combined_winter_data <- rbind.fill(winter_data1, winter_data2)
head(combined_winter_data)
tail(combined_winter_data)

#Create training and test data sets
#make this example reproducible
set.seed(1)
rows <- sample(x=nrow(combined_winter_data), size=.7*nrow(combined_winter_data))
data.train <- combined_winter_data[rows,]
data.test <- combined_winter_data[-rows,]

#review load at different hours to further reduce the data set
boxplot(data.train$Load ~ data.train$Month)

#mydata0 only looks at one month of data between hours of 4 and 10pm
mydata0<-subset(data.train, subset = data.train$Month==11 & data.train$Hour>=16 & data.train$Hour<=22 & data.train$Year ==2003) 
head(mydata0)
plot(mydata0$Load~mydata0$Day)
mdl0<-lm(Load~., data = mydata0)


#shows the relationship between load and the predictor variables is non-linear
summary(mdl0)
plot(mdl0)

plot(predict(mdl0), mydata0$Load, xlab = "Predicted Values", ylab = "Observed Values")
abline(a= 0, b = 1, lwd=2,
       col = "green")

#try a regression tree using book method
Load.tree<-tree(Load~.,data.train)
plot(Load.tree)
text(Load.tree, cex = 0.7)
summary(Load.tree)
Load.tree

Load.tree.error<-rmse_reg(Load.tree, data.test, "Load")
Load.tree.error

#Apply Cross Validation to tree to reduce model variance
cv.Load.tree <- cv.tree(Load.tree)
plot(cv.Load.tree$size, cv.Load.tree$dev, type = "b")

#Prune tree based on cross validation results
prune.Load.tree <- prune.tree(Load.tree, best = 7)
plot(prune.Load.tree)
text(prune.Load.tree, pretty = 0)

#Evaluate pruned tree performance
prune.Load.tree.error<-rmse_reg(prune.Load.tree, data.train, "Load")
yhat <- predict(Load.tree, newdata = data.test)
prune.Load.tree.error
actual <- data.test$Load
plot(yhat, actual)
abline(0,1)

#try a regression tree to model non-linear system using rpart
treeMdl<-rpart(Load~.,data = data.train, method = "anova")
printcp(treeMdl)
plotcp(treeMdl)
plot(treeMdl)
text(treeMdl, cex = 0.7)

tree_error2<-rmse_reg(treeMdl, data.test, "Load")
tree_error2

cv.treeMdl <- cv.tree(treeMdl)
# plot(cv.boston$size, cv.boston$dev, type = "b")

##Develop a boosted tree model
Load.boost <- gbm(Load ~ ., data = data.train, distribution = "gaussian", n.trees = 1000, shrinkage = 0.01)
summary(Load.boost)
yhat<-predict(Load.boost, newdata = data.test, n.trees = 1000, type = "response")
boosted_error<-rmse_reg(Load.boost, data.test, "Load")
boosted_error

#Bagging the load predictions
bagged_ml <- bagging(Load~.,data = data.train, coob = T)
summary(bagged_ml)
bag.error<-rmse_reg(bagged_ml, data.test, "Load")
bag.error

#Bagging load predictions with randomForest()
bag.fit <- randomForest(Load ~ ., data = data.train, mtry = 2, n.trees = 1000)
bag.fit
bag.error<-rmse_reg(bag.fit, data.test, "Load")
bag.error
summary(bag.fit)

#Prepare prediction vectors for 2007
winter_data1_2007<-subset(df3, subset = df3$Month >= 11 & df3$Year== 2007)
winter_data2_2007<-subset(df3, subset = df3$Month <= 4 & df3$Year== 2007)
combined_winter_data_2007 <- rbind.fill(winter_data1_2007, winter_data2_2007)
data2007<-data.frame(combined_winter_data_2007)
estLoad.winter07<-predict(bagged_ml, newdata = data2007)
dim(combined_winter_data_2007)

#
data2007$Date<-as.Date(with(data2007,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")
data2007$LoadEst<-estLoad.winter07
head(data2007)
plot(estLoad.winter07~data2007$Hour)

#exporting predicted hourly load vector for winter months data
install.packages("xlsx")
library("xlsx")



# Write the first data set in a new workbook 
# function outputs to my documents
write.table(data2007$LoadEst, file = "summerloads_Hourly.csv", sep = ",", col.names = NA)

##following code generates a table with max daily load with associated hour
#prototype code: extract daily predicted temperatures for a specific day
dailyTemps<-subset(data2007, subset = data2007$Month == 11 & data2007$Day == 1 )
head(dailyTemps)
#extract row with min column value
maxdailytemp<-dailyTemps[which.max(dailyTemps$LoadEst),]

#prepare o/p data frame for 6 months of thirty days
MaxTemp_hr<-data.frame(matrix(ncol = 12, nrow = 180))
k<-0

#Populate data frame with daily max load values
for (i in 11:12) {
  for (j in 1:30){
    k=k+1
    dailyTemps1<-subset(data2007, subset = data2007$Month == i & data2007$Day == j )
    MaxTemp_hr[k,]<-dailyTemps1[which.max(dailyTemps1$LoadEst),]
  }
}



combined_winter_dailyTemps_2007 <- rbind.fill(dailyTemps1, dailyTemps2)
#combined_winter_MaxTemp_hr_2007 <- rbind.fill(MaxTemp_hr1, MaxTemp_hr2)

#extract column vectors of MaxTemp_hr dataframe
#daily.max.load is the vector of max load values for first thirty days of each month May through October
#daily.max.load values for 31st day of months with 31 days are input to excel table manually prior to submission
#MaxTemp_hr column X12 are max load values for each day for six months (thrity days per month only considered)
daily.max.load<-MaxTemp_hr$X12
write.table(daily.max.load, file = "summerloads_daily.csv", sep = ",", col.names = NA)
#daily.max.time is a column of time values that represent the time at which max load is forecasted for each summer day in 2007
daily.max.time<-MaxTemp_hr$X5
write.table(daily.max.time, file = "summerloads_timely.csv", sep = ",", col.names = NA)

