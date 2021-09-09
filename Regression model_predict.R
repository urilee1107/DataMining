#Problem 1
Toyota <- read.csv("ToyotaCorolla.csv")
head(Toyota)
#a
library('fastDummies')
Toyota.dummy <-dummy_cols(Toyota, select_columns = 'Fuel_Type')
Toyota.dummy <- Toyota.dummy[,c(-8)]
names(Toyota.dummy)

#Variables: Age and 2 Fuel Types 
sel.var <- c(3,4,39,40,41)

model.df <- Toyota.dummy[,sel.var]
names(model.df)

# partition data for model
set.seed(1) 
set.seed(2)
#training (60%) and validation (40%)
train.rows <- sample(rownames(model.df), dim(model.df)[1]*0.6)
train.data <- model.df[train.rows, ]
valid.rows <- setdiff(rownames(model.df), train.rows) 
valid.data <- model.df[valid.rows, ]
dim(train.data)
dim(valid.data)
head(train.data)
head(valid.data)

#Regression model
#developing model 1 (Petrol and Diesel) on training data 
mvr1 <- lm(Price ~ .-Fuel_Type_CNG, data=train.data)
options(scipen=999) # avoid scientific notation
summary(mvr1)

#developing model 2 (Petrol and CNG) on training data 
mvr2 <- lm(Price ~ .-Fuel_Type_Diesel, data=train.data)
options(scipen=999) # avoid scientific notation
summary(mvr2)

#Accuracy measure on training set (not very useful)
library(forecast)
accuracy(mvr1$fitted.values, train.data$Price)
accuracy(mvr2$fitted.values, train.data$Price)
#ME differs between mvr1 and mvr2. 

#Predicting on Validation Set 
#Model 1
library(forecast)
pred1 <- predict(mvr1, newdata = valid.data)
accuracy(pred1, valid.data$Price)
#Model 2
library(forecast)
pred2 <- predict(mvr2, newdata = valid.data)
accuracy(pred2, valid.data$Price)
#All measures exactly the same 

#Residuals
valid.res1 <- data.frame(valid.data$Price, pred1, residuals = 
                          valid.data$Price - pred1)
head(valid.res1)

valid.res2 <- data.frame(valid.data$Price, pred2, residuals = 
                           valid.data$Price - pred2)
head(valid.res2)
#residuals exactly the same 

###b
summary(Toyota)
toyota2.df <- Toyota[1:1000,]
cat.vars <- c(5,8,10,12,14:16,19:39)
toyota2.df[,cat.vars] <- lapply(toyota2.df[,cat.vars],factor)
summary(toyota2.df)

# select variables for regression
select.var <- c(3, 4, 7, 9, 10, 12, 13, 14, 17, 18)

toyota3.df <- toyota2.df[, select.var]
names(toyota3.df)

#first partition data 
set.seed(1) 
#training (60%) and validation (40%)
train.rows <- sample(rownames(toyota3.df), dim(toyota3.df)[1]*0.6)
train.data <- toyota3.df[train.rows, ]
valid.rows <- setdiff(rownames(toyota3.df), train.rows) 
valid.data <- toyota3.df[valid.rows, ]

dim(train.data)
dim(valid.data)



#regression
mvr <- lm(Price ~ ., data=train.data)
options(scipen=999) # avoid scientific notation
summary(mvr)

#Accuracy measure on training set
library(forecast)
accuracy(mvr$fitted.values, train.data$Price)

#Predicting on Validation set
library(forecast)
pred <- predict(mvr, newdata = valid.data)
accuracy(pred, valid.data$Price)

#Residuals
res.data <- data.frame(valid.data$Price, pred, residuals = 
                                 valid.data$Price - pred)
head(res.data)


#Problem 2
airfare.df <- read.csv("Airfares.csv")
#a
dim(airfare.df)
summary(airfare.df)
head(airfare.df)

#histogram of FARE
hist(airfare.df$FARE)
#most common far is 100-150
#Right skewed 
#median less than mean 

numerical.var <- c(5, 9:13,16:18)


airfare.num.df <- airfare.df[, numerical.var]
names(airfare.num.df)
# compute mean, standard dev., min, max, median, length, and missing values for all
# variables
data.frame(mean=sapply(airfare.num.df, mean), 
           sd=sapply(airfare.num.df, sd), 
           min=sapply(airfare.num.df, min), 
           max=sapply(airfare.num.df, max), 
           median=sapply(airfare.num.df, median), 
           length=sapply(airfare.num.df, length),
           miss.val=sapply(airfare.num.df, function(x) 
                   sum(length(which(is.na(x))))))


round(cor(airfare.num.df),2)

#Heat map 
library(gplots)
heatmap.2(cor(airfare.num.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(airfare.num.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

#Scatterplot of FARE vs Distance (dist has highest corr with FARE)
plot(airfare.df$DISTANCE ~ airfare.df$FARE, 
     xlab = "Airfare", ylab = "Distance")
#distance appears to have linear distribution with fare

#b exploring categorical variables through pivot tables 

#Creating Pivot Table for Vacation and New with Fare
library(reshape) 
# stack Price values for each combination of Categorical Variables
mlt <- melt(airfare.df, id=c(7,8,14,15), measure=c("FARE"))
head(mlt, 5)
cast(mlt, VACATION+SW+SLOT ~ GATE, subset=variable=="FARE", 
     margins=c("grand_row", "grand_col"), mean)


#c developing model for FARE 

# select variables for regression
selected.var <- c(5, 6, 7, 8, 9, 10,11,12,13,14, 15, 16, 17, 18)

airfare2.df <- airfare.df[, selected.var]
names(airfare2.df)
#first partition data 
set.seed(1) 
#training (60%) and validation (40%)
train.rows <- sample(rownames(airfare2.df), dim(airfare2.df)[1]*0.6)
train.data <- airfare2.df[train.rows, ]
valid.rows <- setdiff(rownames(airfare2.df), train.rows) 
valid.data <- airfare2.df[valid.rows, ]

dim(train.data)
dim(valid.data)

library(leaps)
search <- regsubsets(FARE ~ ., data = train.data, nbest = 1, nvmax = dim(train.data)[2],
                     method = "exhaustive")
sum <- summary(search)
sum
# show models
sum$which

#show metrics
sum$rsq #Bad criteria to use. R-square always increases with number of variables
sum$adjr2
sum$cp

#choosing 10 variable model adjr^2 0.802 -> 0.805, cp10 ->15.92
choosen.variables <- c(3,4,5,6,7,8,9,11,12,13,14) #-coupon,-new,-slot
airfareModel.df <- airfare2.df[,choosen.variables]
names(airfareModel.df)

#regression
mvr <- lm(FARE ~ . -NEW -SLOT -COUPON, data=train.data)
options(scipen=999) # avoid scientific notation
summary(mvr)

#Accuracy measure on training set
library(forecast)
accuracy(mvr$fitted.values, train.data$FARE)

#Predicting on Validation set
library(forecast)
pred <- predict(mvr, newdata = valid.data)
accuracy(pred, valid.data$FARE)

#d 
#Yes, my model would be helpful for the new airline to create their prices. However, other factors are introduced because they are a new airline.
#advertisement, known, brand, etc. However, they should be able to adjust their prices in consideration of these factors. 

