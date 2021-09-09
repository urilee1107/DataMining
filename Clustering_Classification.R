################################################################################
#1. Use Universal Bank dataset. Note that Personal.Loan is the outcome variable of interest.
#a. Perform a k-NN classification with k=3 for a new data (You are welcome to choose your
#   own values for the new data. Please clearly state it in your report).

uniBanks.df <- read.csv("UniversalBank.csv", header = TRUE)
uniBanks.df$Personal.Loan <- as.factor(uniBanks.df$Personal.Loan)
head(uniBanks.df)
dim(uniBanks.df)
summary(uniBanks.df)

set.seed(111)
train.rows <- sample(rownames(uniBanks.df), dim(uniBanks.df)[1]*0.6)
train.data <- uniBanks.df[train.rows, ]
valid.rows <- setdiff(rownames(uniBanks.df), train.rows)
valid.data <- uniBanks.df[valid.rows, ]
new.df <- data.frame(ID = 5001, Age = 32 ,Experience = 18, Income = 150, ZIP.Code= 93021, Family= 3, CCAvg= 1.5,
                     Education = 3 , Mortgage = 0, Securities.Account = 0,
                     CD.Account = 0, Online = 1 , CreditCard = 0)

plot(uniBanks.df$Income ~ uniBanks.df$Family, ylab = "Income", xlab ="Family Size", 
     col = ifelse(uniBanks.df$Personal.Loan == "1", "black", "gray"))
plot(uniBanks.df$Income ~ uniBanks.df$Education, ylab = "Income", xlab ="Education", 
     col = ifelse(uniBanks.df$Personal.Loan == "1", "black", "gray"))

# initialize normalized training, validation data, assign (temporarily) data frames to originals
train.norm.df <- train.data
valid.norm.df <- valid.data
uniBanks.norm.df <- uniBanks.df
new.norm.df <- new.df

library(caret)
norm.values <- preProcess(train.data[, -10], method=c("center", "scale"))
head(norm.values)

train.norm.df[, -10]<- predict(norm.values, train.data[, -10])
head(train.norm.df)

valid.norm.df <- predict(norm.values, valid.data[, -10])
uniBanks.norm.df <- predict(norm.values, uniBanks.df[, -10])

new.norm.df <- predict(norm.values, new.df[,])
head(uniBanks.norm.df)

##KNN
library(FNN)
uniBanksKnn <- knn(train = train.norm.df[, -10], test = new.norm.df, 
                 cl = train.norm.df[, 10], k = 3) 

row.names(train.data)[attr(uniBanksKnn, "nn.index")]
train.data[attr(uniBanksKnn, "nn.index"),]
summary(uniBanksKnn)

##b. Identify the best k. Why do you think this is the best?  
##(Hint: Explain what happens if k increases and if k decreases).
############# 
# When K = 3, the accuracy is the highest 
#############
library(caret)
uniBanksaccuracy.df <- data.frame(k = seq(1, 30, 1), accuracy = rep(0, 30)) 
uniBanksaccuracy.df

for(i in 1:30) {
  uniBanksKnn.pred <- knn(train = train.norm.df[, -10], test = valid.norm.df, 
                       cl = train.norm.df[, 10], k = i)
  uniBanksaccuracy.df[i, 2] <- confusionMatrix(uniBanksKnn.pred, valid.data[, 10])$overall[1]
}

uniBanksaccuracy.df  #K=3, accuracy:0.9505 the highest accuracy when k is 3.
uniBanksKnn.pred <- knn(train = train.norm.df[, -10], test = valid.norm.df, 
                      cl = train.norm.df[, 10], k = 3)
conFmatrix <- confusionMatrix(uniBanksKnn.pred, valid.data[, 10]) ##accuracy:0.9505
conFmatrix

##c. Calculate accuracy, sensitivity, and specificity for your validation data 
#using the best k (from part b) without calling or 
#using the confusion matrix (that is, compute them directly from the validation data) 
#and verify your computation by directly calling the confusion matrix using R 

uniBanksKnn.pred <- knn(train = train.norm.df[, -10], test = valid.norm.df, 
                        cl = train.norm.df[, 10], k = 3)
summary(uniBanksKnn.pred)
summary(valid.data[,10])
conFtable <-table(uniBanksKnn.pred , valid.data[,10]); conFtable
#Accuracy
modelAccuracy <-(conFtable[1,1]+ conFtable[2,2])/sum(conFtable); modelAccuracy 
#Sensitivity
modelSensitivity <-conFtable[1,1] /(conFtable[1,1]+conFtable[2,1]); modelSensitivity 
#Specificity
modelSpecificity <- conFtable[2,2]/(conFtable[1,2]+conFtable[2,2]); modelSpecificity 
conFmatrix 

##d. Partition your dataset into 3. Compare the accuracy metrics from part c for
##  both validation data and test data. Summarize your results.

##Partition the data into train, valid, and test
set.seed(1) 
## training (50%), validation (30%), and test (20%)
train.rows2 <- sample(rownames(uniBanks.df), dim(uniBanks.df)[1]*0.5)
valid.rows2 <- sample(setdiff(rownames(uniBanks.df), train.rows2), 
                      dim(uniBanks.df)[1]*0.3)
test.rows2 <- setdiff(rownames(uniBanks.df), union(train.rows2, valid.rows2))
train.data2 <- uniBanks.df[train.rows2, ]
valid.data2 <- uniBanks.df[valid.rows2, ]
test.data2 <- uniBanks.df[test.rows2, ]
dim(train.data2)
dim(valid.data2)
dim(test.data2)

#Normalize 
train.norm.df2 <- train.data2
valid.norm.df2 <- valid.data2
test.norm.df2 <- test.data2

library(caret)
norm.values <- preProcess(train.data2[, c(-10)], method=c("center", "scale"))
train.norm.df2[,c(-10)]<- predict(norm.values, train.data2[, c(-10)])
valid.norm.df2 <- predict(norm.values, valid.data2[, c(-10)])
test.norm.df2 <- predict(norm.values, test.data2[, c(-10)])
head(train.norm.df2)

head(test.data2)

# accuracy valid data set 
bank.pred1 <- knn(train=train.norm.df2[,c(-10)], test=valid.norm.df2,
                  cl= train.norm.df2[,10], k=3)
confusionMatrix(bank.pred1, as.factor(valid.data2[,10])) #Accuracy: 0.9613

#accuracy on test data set
bank.pred2 <- knn(train=train.norm.df2[,c(-10)], test=test.norm.df2,
                  cl= train.norm.df2[,10], k=3)
confusionMatrix(bank.pred2, as.factor(test.data2[,10])) #Accuracy: 0.947


################################################################################
#2. Use Universal Bank dataset.
#a. Create a classification tree(you may use the default one that we got in class).

library(rpart) 
library(rpart.plot)

uniBanks.df <- uniBanks.df[,-c(1,5)] ## Drop ID and zip code columns
head(uniBanks.df)
dim(uniBanks.df)

set.seed(1)

train.rows <- sample(rownames(uniBanks.df), dim(uniBanks.df)[1]*0.6)
train.df <- uniBanks.df[train.rows, ]
dim(train.df)
valid.rows <- setdiff(rownames(uniBanks.df), train.rows) 
valid.df <- uniBanks.df[valid.rows, ]
dim(valid.df)
uniBankdefault.tree <- rpart(Personal.Loan ~ ., data = uniBanks.df, 
                             method = "class")
prp(uniBankdefault.tree, type = 1, extra = 1, split.font = 1, varlen = -10)  

#b.How would you decide between k-NN (where k is the best one obtained in 1b) and
#  the tree you had developed in part a? Explain your answer.

library(caret)
# the training data/ Accuracy: 0.9863
Loandefault.pred.train <- predict(uniBankdefault.tree,train.df,type = "class")
confusionMatrix(Loandefault.pred.train, as.factor(train.df[, 8])) 
#the validation set/ Accuracy: 0.985 
Loandefault.pred.valid <- predict(uniBankdefault.tree,valid.df,type = "class") 
confusionMatrix(Loandefault.pred.valid, as.factor(valid.df$Personal.Loan))
# tree accuracy: 0.98 VS K-nn(K=3) accuracy:0.9505
length(uniBankdefault.tree$frame$var[uniBankdefault.tree$frame$var == "<leaf>"])


################################################################################
# 3.Use eBayAuctions data. The variable of interest is competitive auction 
#(1 if at least two bids are placed on the item auctioned). 
#Note that duration(4), opening price(7), currency(2) and endDay(5) are terms or variables chosen by the seller.

#a. Use all the predictors to create a classification tree. 
#Set minbucket=50 (ensures the terminal nodes have at least 50 items) 
#and maxdepth=7 (ensures the tree is at most 7 nodes deep). 
#Describe any one of the rules based on the tree.
eBay.df <- read.csv("eBayAuctions.csv", header = TRUE)
eBay.df$Category<-as.factor(eBay.df$Category)
head(eBay.df)
summary(eBay.df)
names(eBay.df)

library(rpart) 
library(rpart.plot)
eBayclass.tree <- rpart(Competitive. ~ ., data = eBay.df, 
                        control = rpart.control(minsplit=50, maxdepth = 7), method = "class")
prp(eBayclass.tree, type = 1, extra = 1, split.font = 1, varlen = -10)  

#b. Is your tree above helpful for prediction of a new auction? If not, how would you change
#your approach? Develop a classification tree that will be helpful for predicting a new
#auction (you may use minbucket=50 and maxdepth=7 again).

set.seed(1)
dim(eBay.df)

train.rows <- sample(rownames(eBay.df), dim(eBay.df)[1]*0.6)
train.df <- eBay.df[train.rows, ]
valid.rows <- setdiff(rownames(eBay.df), train.rows) 
valid.df <- eBay.df[valid.rows, ]
dim(train.df)
dim(valid.df)
### choosing the best model
eBaycv.ct <- rpart(Competitive. ~ .-ClosePrice, data = train.df, method = "class", 
                   cp = 0.00001, minsplit = 50,xval=5)
printcp(eBaycv.ct) 
plotcp(eBaycv.ct) 
#Find a better parsimonious pruned tree
eBaypruned.ct <- prune(eBaycv.ct, cp = 0.0097)
#Plot the pruned tree
prp(eBaypruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(eBaypruned.ct$frame$var == "<leaf>", 'gray', 'white'))  

## Including all variables
library(caret)
# the training data 
eBaydefault.pred.train <- predict(eBaypruned.ct,train.df,type = "class")
confusionMatrix(eBaydefault.pred.train, as.factor(train.df[, 8])) 
#the validation set 
eBaydefault.pred.valid <- predict(eBaypruned.ct,valid.df,type = "class") 
confusionMatrix(eBaydefault.pred.valid, as.factor(valid.df[, 8]))

# Random Forest 
library(randomForest)
eBayrf <- randomForest(as.factor(Competitive.) ~ ., data = train.df, ntree = 500, 
                       mtry = 4, nodesize = 5, importance = TRUE)  
varImpPlot(eBayrf, type = 1)
eBayRF.pred.valid <- predict(eBayrf,valid.df,type = "class")
confusionMatrix(as.factor(eBayRF.pred.valid), as.factor(valid.df$Competitive.))

###the model without ClosePrice variable
library(caret)
# the training data 
eBaycv.ct.pred.train <- predict(eBaycv.ct,train.df,type = "class")
confusionMatrix(eBaycv.ct.pred.train, as.factor(train.df[, 8])) 
#the validation set 
eBaycv.ct.pred.valid <- predict(eBaycv.ct,valid.df,type = "class") 
confusionMatrix(eBaycv.ct.pred.valid, as.factor(valid.df[, 8]))

# Random Forest
library(randomForest)
eBayrf2 <- randomForest(as.factor(Competitive.) ~ .-ClosePrice, data = train.df, ntree = 500, 
                        mtry = 4, nodesize = 5, importance = TRUE)  
## variable importance plot
varImpPlot(eBayrf2, type = 1)
eBayRF.pred2.valid <- predict(eBayrf2, valid.df, type = "class")
confusionMatrix(as.factor(eBayRF.pred2.valid), as.factor(valid.df$Competitive.))

#c. Based on the trees you have developed summarize your recommendations for your seller-friend.
#The answer is on PDF.
