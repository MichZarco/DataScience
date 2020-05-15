library(dplyr)
library(ggplot2)

#Load Data
df <- read.csv("~/DataScience/myTraining/DataSets/MarketingEngagement/WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv", header = TRUE,sep = ',')
read
head(df)

#Encode response as 0 and 1
df$Engaged <- as.integer(df$Response)-1
head(df)

#Engagement rate
engagementRate <- df%>%
  group_by(Engaged) %>%
  summarise(Count=n()) %>%
  mutate(Percentage=Count/nrow(df)*100.0)

print(engagementRate)

#Transpose engament rate
transposed <- t(engagementRate)
print(transposed)

colnames(transposed) <- engagementRate$Engaged
transposed <- transposed[-1,]
print(transposed)

#Sales Channels
salesChannel <- df %>%
  group_by(Engaged, Channel=Sales.Channel) %>%
  summarise(Count=n())

print(salesChannel)

#Pie Chart
ggplot(salesChannel, aes(x="",y=Count,fill=Channel))+
  geom_bar(width = 1, stat = 'identity', position = position_fill())+
  geom_text(aes(x=1.25, label=Count), position = position_fill(vjust = 0.5))+
  coord_polar('y')+
  facet_wrap(~Engaged)+
  ggtitle('Sales Channel (0: Not Engaged, 1: Engaged)')+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = 'bottom'
  )

#Total Claim Amounts
ggplot(df,aes(x='', y=Total.Claim.Amount))+
  geom_boxplot()+
  facet_wrap(~Engaged)+
  ylab('Total Claim Amount')+
  xlab('0: Not Engaged, 1: Engaged')+
  ggtitle('Engaged vs Not Engaged: Total Claim Amount')+
  theme(plot.title = element_text(hjust = 0.5))

#dataframe datatypes
sapply(df, class)
#summary statistics
summary(df)

#Selecting Columns
X_cat <- subset(df, select = c(Coverage,Education,EmploymentStatus,Gender))
X_continous <- select_if(df, is.numeric)

head(X_cat)
head(X_continous)

#standarize continous variables
X_scaled <- scale(X_continous)
X_scaled <- subset(X_scaled, select = -Engaged)
head(X_scaled)

#Joining categorical and numerical into one dataframe
X <- cbind(X_cat,X_scaled)
head(X)

#Target column
y <- df$Engaged
y

#Train Test Split
library(caret)
library(rpart)
index = createDataPartition(y=y, p = 0.7, list = FALSE)
X_train = X[index,]
X_test = X[-index,]
y_train = y[index]
y_test = y[-index]

traindata = cbind(X_train,y_train)
head(traindata)
traindata <- traindata %>%
  mutate(y_train = as.factor(y_train))
sapply(traindata, class)


testdata = cbind(X_test,y_test)
head(testdata)
testdata <- testdata %>%
  mutate(y_test = as.factor(y_test))

#Train Decision Tree
tree = train(y_train ~ ., data=traindata,method = 'rpart',trControl = trainControl(method = "cv") )
tree
plot(tree)
#Best Parameter
tree$bestTune
plot(tree$finalModel)
text(tree$finalModel, digits = 3)
tree$finalModel

summary(tree$finalModel)

#prediction
pred = predict(tree, newdata = testdata)
table(pred, testdata$y_test)

error.rate = round(mean(pred!= testdata$y_test),2)
error.rate

confusionMatrix(testdata$y_test, pred)

#XGBOOST Tree
grid_default <- expand.grid(
  nrounds = seq(from = 0, to = 10, by = 50),
  eta = c(0.1, 0.3, 0.5),
  max_depth = c(2,5, 10,15),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = 'cv',
  number = 5,
  verboseIter = FALSE,
  allowParallel = TRUE
)

tree = train(
  y_train ~ .,
  data=traindata,
  tuneGrid = grid_default,
  trControl = train_control,
  method = 'xgbTree'
)
tree
plot(tree)
#Best Parameter
tree$bestTune
xgb.plot.tree(model = tree$finalModel)
text(tree$finalModel, digits = 3)
tree$finalModel

summary(tree$finalModel)

#prediction
pred = predict(tree, newdata = testdata)
table(pred, testdata$y_test)

error.rate = round(mean(pred!= testdata$y_test),2)
error.rate

confusionMatrix(testdata$y_test, pred)

