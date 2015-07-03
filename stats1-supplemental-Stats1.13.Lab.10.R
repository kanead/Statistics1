# Statistics One, 2013, Lab 10

# Lab goals
#   Conduct a binary logisitc regression 

# Example
# The data are based on a mock jury study conducted by Shari Diamond and Jonathan Casper 
# Participants (N = 100) watched a videotaped sentencing phase trial in which the defendant had 
#   already been found guilty  
# The issue for the jurors to decide was whether the defendant deserved the death penalty  
# These data were collected “pre-deliberation” (i.e., each juror was asked to provide 
#   his/her vote on the death penalty verdict, then the jurors met as a group to decide the 
#   overall jury verdict)
# The initial individual verdicts are given in this data set

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
install.packages("psych")
install.packages("aod")
install.packages("QuantPsyc")

# Load packages
library(psych)
library(aod)
library(QuantPsyc)

# Read the data into a dataframe called BL
BL <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.Lab.10.txt", header = T)

# If you want to view the data
# View(BL)
edit(BL)

# Summary statistics
describe(BL) 
        var   n  mean    sd median trimmed   mad min max range skew kurtosis   se
subject   1 100 50.50 29.01   50.5   50.50 37.06   1 100    99 0.00    -1.24 2.90
verdict   2 100  0.48  0.50    0.0    0.48  0.00   0   1     1 0.08    -2.01 0.05
danger    3 100  4.16  2.70    4.0    4.03  2.97   0  10    10 0.35    -0.76 0.27
rehab     4 100  4.89  2.91    5.0    4.80  4.45   0  10    10 0.19    -1.29 0.29
punish    5 100  4.94  3.30    4.0    4.88  4.45   0  10    10 0.19    -1.36 0.33
gendet    6 100  5.17  3.10    5.0    5.20  4.45   0  10    10 0.01    -1.36 0.31
specdet   7 100  4.76  3.01    4.0    4.70  4.45   0  10    10 0.11    -1.23 0.30
incap     8 100  4.92  3.13    5.0    4.90  2.97   0  10    10 0.08    -1.08 0.31

# Binary logistic regression
lrfit <- glm(BL$verdict ~ BL$danger + BL$rehab + BL$punish + BL$gendet + BL$specdet + BL$incap, family = binomial)
summary(lrfit)
Call:
glm(formula = BL$verdict ~ BL$danger + BL$rehab + BL$punish + 
    BL$gendet + BL$specdet + BL$incap, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.9691  -0.9320  -0.4631   0.8908   1.9566  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)   
(Intercept) -1.747575   0.917284  -1.905  0.05676 . 
BL$danger    0.293385   0.092916   3.158  0.00159 **
BL$rehab    -0.187845   0.081398  -2.308  0.02101 * 
BL$punish    0.070123   0.071113   0.986  0.32409   
BL$gendet    0.185736   0.077329   2.402  0.01631 * 
BL$specdet   0.005903   0.078648   0.075  0.94017   
BL$incap     0.003530   0.075872   0.047  0.96289   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 138.47  on 99  degrees of freedom
Residual deviance: 114.06  on 93  degrees of freedom
AIC: 128.06

Number of Fisher Scoring iterations: 3

confint(lrfit) # CIs using profiled log-likelihood (default for logistic models)
                  2.5 %      97.5 %
(Intercept) -3.65200526 -0.02216969
BL$danger    0.11998309  0.48756413
BL$rehab    -0.35425597 -0.03283358
BL$punish   -0.06763326  0.21351921
BL$gendet    0.03858352  0.34393872
BL$specdet  -0.15005626  0.16092464
BL$incap    -0.14735051  0.15284122
confint.default(lrfit) # CIs using standard errors
                  2.5 %      97.5 %
(Intercept) -3.54541947  0.05026872
BL$danger    0.11127396  0.47549632
BL$rehab    -0.34738173 -0.02830763
BL$punish   -0.06925615  0.20950253
BL$gendet    0.03417504  0.33729764
BL$specdet  -0.14824352  0.16004999
BL$incap    -0.14517698  0.15223763

# Model fit
with(lrfit, null.deviance - deviance) #difference in deviance for the two models
[1] 24.40596
with(lrfit, df.null - df.residual) #df for the difference between the two models
[1] 6
with(lrfit, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = FALSE)) #p-value
[1] 0.0004396543

# Wald tests
library(aod)
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 2) #danger
Chi-squared test:
X2 = 10.0, df = 1, P(> X2) = 0.0016
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 3) #rehab
Chi-squared test:
X2 = 5.3, df = 1, P(> X2) = 0.021
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 4) #punish
Chi-squared test:
X2 = 0.97, df = 1, P(> X2) = 0.32
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 5) #gendet
Chi-squared test:
X2 = 5.8, df = 1, P(> X2) = 0.016
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 6) #specdet
Chi-squared test:
X2 = 0.0056, df = 1, P(> X2) = 0.94
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 7) #incap
Chi-squared test:
X2 = 0.0022, df = 1, P(> X2) = 0.96

# Odds ratios
exp(coef(lrfit)) #exponentiated coefficients
(Intercept)   BL$danger    BL$rehab   BL$punish   BL$gendet  BL$specdet    BL$incap 
  0.1741958   1.3409591   0.8287434   1.0726403   1.2041047   1.0059207   1.0035366 

# Classification table
ClassLog(lrfit, BL$verdict)
$rawtab
       resp
         0  1
  FALSE 39 16
  TRUE  13 32

$classtab
       resp
                0         1
  FALSE 0.7500000 0.3333333
  TRUE  0.2500000 0.6666667

$overall
[1] 0.71

$mcFadden
[1] 0.1762553

# Significant predictors (danger, rehab, gendet)
par(mfrow=c(1,3))
plot(BL$danger, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$danger), col="blue", lwd=5)
plot(BL$rehab, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$rehab), col="blue", lwd=5)
plot(BL$gendet, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$gendet), col="blue", lwd=5)
par(mfrow=c(1,1))
title("Significant predictors")

# Non-significant predictors (punish, specdet, incap)
par(mfrow=c(1,3))
plot(BL$punish, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$punish), col="blue", lwd=5)
plot(BL$specdet, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$specdet), col="blue", lwd=5)
plot(BL$incap, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$incap), col="blue", lwd=5)
par(mfrow=c(1,1))
title("Non-significant predictors")
