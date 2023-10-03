library(liver) #call library

data(bank) #Call data

dim(bank) #Check rows and columns in the data

colSums(is.na(bank)) #check for null values
################################################################################
#plot the variables
library(ggplot2)
ggplot(bank)+geom_bar(aes(x=bank$job),col="black",fill="orange")+coord_flip()+ggtitle("JOB")

ggplot(bank,aes(x=bank$job))+geom_bar(aes(y=(..count..)/sum(..count..)),col="black",fill="orange")+coord_flip()

data.frame(sort(table("job" = bank$job),decreasing = T))
ggplot(bank)+geom_bar(aes(x=bank$deposit),col="black",fill="orange")+coord_flip()+ggtitle("JOB")

ggplot(data=bank, aes(x=deposit)) +
  geom_bar(col="black",fill="orange") +
  geom_text(stat='count', aes(label=..count..),hjust=1)+coord_flip()+ggtitle("Deposit")

ggplot(data=bank, aes(x=job)) +
  geom_bar(col="black",fill="orange") +
  geom_text(stat='count', aes(label=..count..),hjust=1)+coord_flip()+ggtitle("Job")

ggplot(data=bank, aes(x=month)) +
  geom_bar(col="black",fill="orange") +
  geom_text(stat='count', aes(label=..count..),hjust=1)+coord_flip()+ggtitle("Month")

ggplot(data=bank, aes(x=poutcome)) +
  geom_bar(col="black",fill="orange") +
  geom_text(stat='count', aes(label=..count..),hjust=1)+coord_flip()+ggtitle("poutcome")




##MODELS
data=bank
library(ggplot2)
library(caret) # Accuracy
library(e1071)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(caTools)
library(descr)
library(MASS)
#Perform linear discriminant analysis
set.seed(100)
sample=sample.int(n=nrow(data),size=floor(0.8*nrow(data)),replace=F) #80% of the data in train set and 20%in test set

traindata=data[sample,]

testdata=data[-sample,]

train=data.frame(traindata)

test=data.frame(testdata)

model=lda(deposit~.,data=train)
model

predicted <- predict(model, test)
head(predicted$class)

d=table(predicted$class,test$deposit)

library(klaR)
data[,1] <- as.factor(data[,1])
partimat(deposit~.,data=data,method="lda")
library(stargazer)
stargazer(d,type="text")
CrossTable(test$deposit, predicted$class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
##Perform K-Nearest Neighbour
bank.knn= caret::train(deposit ~ ., data = train, method = "knn", 
                  maximize = TRUE,
                  trControl = trainControl(method = "cv", number = 10),
                  preProcess=c("center", "scale"))
predictedkNN <- predict(bank.knn , newdata = test)
confusionMatrix(predictedkNN , testdata$deposit)
CrossTable(testdata$deposit, predictedkNN,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


##Perform Classification and Regression Tree
bank.cart<-rpart::rpart(deposit ~ ., train , method = 'class')
par(mfrow=c(1,1))
rattle::fancyRpartPlot(bank.cart , digits=2 , palettes = c("Purples", "Oranges"))
cart_pred <- predict( bank.cart , test , type = "class")
cart_prob <- predict( bank.cart , test, type = "prob")
confusionMatrix(cart_pred , test$deposit)
CrossTable(test$deposit, cart_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
