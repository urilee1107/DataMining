#1.
Toyota <- read.csv("ToyotaCorolla.csv")

#a. Summarize the dataset. (Do not just put the R output. Make observations based on
#numbers)

summary(Toyota) 
dim(Toyota)

#b. Normalize the variable kilometers

normalize.Km <-scale(Toyota$KM)
summary(normalize.Km)
Toyota.normalize.Km <-cbind(normalize.Km,Toyota)
Toyota.normalize.Km <-Toyota.normalize.Km[, colnames(Toyota.normalize.Km)[c(2:8,1,9:39)]]
head(Toyota.normalize.Km)

#c. Create dummies for the variable Fuel Type
#install.packages('fastDummies')
library('fastDummies')
Toyota.dummy <-dummy_cols(Toyota, select_columns = 'Fuel_Type')
names(Toyota.dummy)


#d. Partition the data into three sets similar to what we did in class.

## partitioning into training (50%), validation (30%), test (20%) randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(Toyota), dim(Toyota)[1]*0.5)
valid.rows <- sample(setdiff(rownames(Toyota), train.rows), dim(Toyota)[1]*0.3) 
test.rows <- setdiff(rownames(Toyota), union(train.rows, valid.rows)) 

train.data <- Toyota[train.rows, ]
valid.data <- Toyota[valid.rows, ]
test.data <- Toyota[test.rows, ]

length(train.data$Id)
length(valid.data$Id)
length(test.data$Id)
length(Toyota$Id)

#2. 
appl.shipment <-read.csv('ApplianceShipments.csv')


#a. create a time plot
#install.packages('forecast')
library(forecast)
appl.shipment.ts <-ts(appl.shipment$Shipments, start = c(1985,1), end = c(1989,4), freq = 4)

plot(appl.shipment.ts, xlab = "Year", ylab = "Shipment", ylim = c(3900, 6000))


#b. is there a quarterly pattern? Zoom in to the range of 3500-5000 on the y-axis

plot(appl.shipment.ts, xlab = "Year", ylab = "Shipment", ylim = c(3500, 5000))


#c. Create 4 separate lines, one line for each of the quarters and then plot them as a4
#separate series on the line graph. Zoom in to the range of 3500-5000 on the y-axis.
#Summarize your observations. (Hint: Try exploring seq function in R)

appl.shipment.ts.1 <-ts(appl.shipment$Shipments[c(seq(from = 1, to = 20, by = 4))], start = c(1985,1), end = c(1989,1))
appl.shipment.ts.2 <-ts(appl.shipment$Shipments[c(seq(from = 2, to = 20, by = 4))], start = c(1985,1), end = c(1989,1))
appl.shipment.ts.3 <-ts(appl.shipment$Shipments[c(seq(from = 3, to = 20, by = 4))], start = c(1985,1), end = c(1989,1))
appl.shipment.ts.4 <-ts(appl.shipment$Shipments[c(seq(from = 4, to = 20, by = 4))], start = c(1985,1), end = c(1989,1))

plot(appl.shipment.ts.1, xlab = "Year", ylab = "Shipment", ylim = c(3900, 5000))
lines(appl.shipment.ts.2, col = 'red')
lines(appl.shipment.ts.3, col = 'blue')
lines(appl.shipment.ts.4, col = 'green')

legend(1985, 5000, legend = c('Q1','Q2','Q3','Q4'), col =c("black",'red','blue','green'), lty = 1)


#d. Create a line graph at a yearly aggregated level.

annual.appl.shipment.ts <- aggregate(appl.shipment.ts, FUN = mean)
plot(annual.appl.shipment.ts, xlab = "Year", ylab = "Average Shipment", ylim = c(3900, 5000))


#3. . Use RidingMowers dataset. Create a scatterplot of Lot size vs. Income, color coded by
# owner/non-owner. What do you infer? Which customer segment would you target?

#install.packages('ggplot2')
#install.packages('hrbrthemes')

library('ggplot2')
library('hrbrthemes')
RidingMowers <-read.csv('RidingMowers.csv')

ggplot(RidingMowers, aes(x=Income, y=Lot_Size, color=Ownership)) + geom_point(size=6)

