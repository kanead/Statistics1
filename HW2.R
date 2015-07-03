#Homework2 Script

setwd("/Users/karenyang/Desktop/StatisticsOne12weeks")
getwd()
install.packages("psych")
install.packages("sm")
library(psych)
library(sm)
search()
hw2data <- read.table("Stats1.13.HW.02.txt", header = T)

#How many rows of data are there in the data file?
nrow(hw2data)

#What is the name of the dependent variable?
edit(hw2data)
names(hw2data)

#What is the mean of SR across all subjects?
mean(hw2data$SR)
describe(hw2data$SR)

#What is the variance of SR across all subjects?
var(hw2data$SR)
2.56*2.56

#What is the mean of SR for all subjects at pretest?
pretest <- subset(hw2data, hw2data[,3]=="pre")
mean(pretest$SR)


#What is the standard deviation of SR for all subjects at posttest?
posttest <- subset(hw2data, hw2data[,3]=="post")
sd(posttest$SR)

#What is the median of SR for all subjects at posttest?
median(posttest$SR)

#Which group has the highest mean at posttest?
levels(posttest$condition)
describeBy(posttest, posttest$condition)

#Which one best approximates a normal distribution?
preWM <- subset(pretest, pretest[, 2]=="WM")
preDS <- subset(pretest, pretest[, 2]=="DS")
prePE <- subset(pretest, pretest[, 2]=="PE")
postWM <-subset(posttest, posttest[, 2]=="WM")
postDS <-subset(posttest, posttest[, 2]=="DS")
postPE <-subset(posttest, posttest[, 2]=="PE")

par(mfrow = c(2,3))
hist(preWM$SR)
hist(preDS$SR)
hist(prePE$SR)
hist(postWM$SR)
hist(postDS$SR)
hist(postPE$SR)

par(mfrow = c(2,3))
plot(density(preWM$SR), xlab = "Pre-Test WM", main = "")
plot(density(preDS$SR), xlab = "Pre-Test DS", main = "")
plot(density(prePE$SR), xlab = "Pre-Test PE", main = "")
plot(density(postWM$SR), xlab = "Post-Test WM", main = "")
plot(density(postDS$SR), xlab = "Post-Test DS", main = "")
plot(density(postPE$SR), xlab = "Post-Test PE", main = "")


#Which group showed the biggest gains in SR?
mean(postPE$SR)-mean(prePE$SR)
mean(postWM$SR)-mean(preWM$SR)
mean(postDS$SR)-mean(preDS$SR)