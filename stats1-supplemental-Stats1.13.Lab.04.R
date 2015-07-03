# Statistics One, 2013, Lab 4

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlation analyses
#   Conduct regression analyses, unstandardized
#   Conduct regression analyses, standardized

# Example
#   A correlational study investigating predictors of physcial endurance in adults
#     Outcome variable (Y) is physical endurance
#     Predictors (X) are age and number of years actively engaged in exercise/sports
#     Sample size is N = 200

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")
setwd("/Users/karenyang/Desktop/StatisticsOne12weeks")
#check to see what packages are already there
search()
# If necessary, install packages
# install.packages("psych")
install.packages("psych")
# Load packages
library(psych)

# Read data into a dataframe called PE (physical endurance)
PE <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.Lab.04.txt", header = T)

# If you want to view the data
#View(PE)
edit(PE)

# Summary statistics
describe(PE) 
            var   n   mean    sd median trimmed   mad min max range skew kurtosis   se
pid           1 200 101.81 58.85  101.5  101.71 74.87   1 204   203 0.01    -1.21 4.16
age           2 200  49.41 10.48   48.0   49.46 10.38  20  82    62 0.06    -0.14 0.74
activeyears   3 200  10.68  4.69   11.0   10.57  4.45   0  26    26 0.30     0.46 0.33
endurance     4 200  26.50 10.84   27.0   26.22 10.38   3  55    52 0.22    -0.44 0.77

# Correlation analysis 
cor(PE[2:4]) # Omit column 1 because it contains participant id numbers
                    age activeyears   endurance
age          1.00000000   0.3289909 -0.08483813
activeyears  0.32899092   1.0000000  0.32654016
endurance   -0.08483813   0.3265402  1.00000000

round(cor(PE[2:4]), 2) # Round to 2 decimal places 

cor.test(PE$age, PE$activeyears)
#Pearson's product-moment correlation
#data:  PE$age and PE$activeyears
#t = 4.9022, df = 198, p-value = 1.969e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#0.1993491 0.4473145
#sample estimates:
#      cor 
#0.3289909 

cor.test(PE$endurance, PE$activeyears)
#Pearson's product-moment correlation
#data:  PE$endurance and PE$activeyears
#t = 4.8613, df = 198, p-value = 2.37e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1967110 0.4451154
#sample estimates:
#      cor 
#0.3265402
 
cor.test(PE$endurance, PE$age)
#Pearson's product-moment correlation
#data:  PE$endurance and PE$age
#t = -1.1981, df = 198, p-value = 0.2323
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.22097811  0.05454491
#sample estimates:
#        cor 
#-0.08483813 

# Histograms
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
hist(PE$age)
hist(PE$activeyears)
hist(PE$endurance)
layout(matrix(c(1,1), 1, 1, byrow = TRUE))


# Regression analyses, unstandardized
model1 <- lm(PE$endurance ~ PE$age)
summary(model1)
Call:
lm(formula = PE$endurance ~ PE$age)

Residuals:
     Min       1Q   Median       3Q      Max 
-23.5364  -7.7338   0.3583   6.7224  30.0426 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 30.83472    3.69813   8.338 1.26e-14 ***
PE$age      -0.08772    0.07322  -1.198    0.232    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 10.83 on 198 degrees of freedom
Multiple R-squared:  0.007198,	Adjusted R-squared:  0.002183 
F-statistic: 1.435 on 1 and 198 DF,  p-value: 0.2323

plot(PE$endurance ~ PE$age, main = "Scatterplot", ylab = "Endurance", xlab = "Age")
abline(lm(PE$endurance ~ PE$age), col="blue")

model2 <- lm(PE$endurance ~ PE$activeyears)
summary(model2)
Call:
lm(formula = PE$endurance ~ PE$activeyears)

Residuals:
     Min       1Q   Median       3Q      Max 
-20.5006  -7.8066   0.5304   5.7649  31.0511 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)     18.4386     1.8104  10.185  < 2e-16 ***
PE$activeyears   0.7552     0.1553   4.861 2.37e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 10.27 on 198 degrees of freedom
Multiple R-squared:  0.1066,	Adjusted R-squared:  0.1021 
F-statistic: 23.63 on 1 and 198 DF,  p-value: 2.37e-06

plot(PE$endurance ~ PE$activeyears, main = "Scatterplot", ylab = "Endurance", xlab = "Active Years")
abline(lm(PE$endurance ~ PE$activeyears), col="blue")

model3 <- lm(PE$endurance ~ PE$age + PE$activeyears)
summary(model3)
Call:
lm(formula = PE$endurance ~ PE$age + PE$activeyears)

Residuals:
     Min       1Q   Median       3Q      Max 
-21.4598  -7.7398   0.6984   5.5364  27.9230 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)     27.7035     3.4779   7.966 1.29e-13 ***
PE$age          -0.2229     0.0720  -3.096  0.00225 ** 
PE$activeyears   0.9192     0.1610   5.708 4.16e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 10.06 on 197 degrees of freedom
Multiple R-squared:  0.1481,	Adjusted R-squared:  0.1394 
F-statistic: 17.12 on 2 and 197 DF,  p-value: 1.394e-07

# To visualize model3, save the predicted scores as a new variable and then plot with endurance
PE$predicted <- fitted(model3)

plot(PE$endurance ~ PE$predicted, main = "Scatterplot", ylab = "Endurance", xlab = "Model 3 Predicted Scores")
abline(lm(PE$endurance ~ PE$predicted), col="blue")

# The function fitted returns predicted scores whereas the function resid returns residuals
PE$e <- resid(model3)

hist(PE$e)
plot(PE$predicted ~ PE$e, main = "Scatterplot", ylab = "Model 3 Predicted Scores", xlab = "Model 3 Residuals")
abline(lm(PE$predicted ~ PE$e), col="blue")

# Regression analyses, standardized
# In simple regression, the standardized regression coefficient will be the same as the correlation coefficient

round(cor(PE[2:4]), 2) # Round to 2 decimal places 
              age activeyears endurance
age          1.00        0.33     -0.08
activeyears  0.33        1.00      0.33
endurance   -0.08        0.33      1.00
model1.z <- lm(scale(PE$endurance) ~ scale(PE$age))
summary(model1.z)
Call:
lm(formula = scale(PE$endurance) ~ scale(PE$age))

Residuals:
     Min       1Q   Median       3Q      Max 
-2.17133 -0.71347  0.03306  0.62017  2.77155 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)
(Intercept)   -1.332e-18  7.063e-02   0.000    1.000
scale(PE$age) -8.484e-02  7.081e-02  -1.198    0.232

Residual standard error: 0.9989 on 198 degrees of freedom
Multiple R-squared:  0.007198,	Adjusted R-squared:  0.002183 
F-statistic: 1.435 on 1 and 198 DF,  p-value: 0.2323

model2.z <- lm(scale(PE$endurance) ~ scale(PE$activeyears))
summary(model2.z)Call:
lm(formula = scale(PE$endurance) ~ scale(PE$activeyears))

Residuals:
     Min       1Q   Median       3Q      Max 
-1.89126 -0.72019  0.04893  0.53184  2.86459 

Coefficients:
                       Estimate Std. Error t value Pr(>|t|)    
(Intercept)           4.871e-17  6.700e-02   0.000        1    
scale(PE$activeyears) 3.265e-01  6.717e-02   4.861 2.37e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.9476 on 198 degrees of freedom
Multiple R-squared:  0.1066,	Adjusted R-squared:  0.1021 
F-statistic: 23.63 on 1 and 198 DF,  p-value: 2.37e-06

model3.z <- lm(scale(PE$endurance) ~ scale(PE$age) + scale(PE$activeyears))
summary(model3.z)
Call:
lm(formula = scale(PE$endurance) ~ scale(PE$age) + scale(PE$activeyears))

Residuals:
     Min       1Q   Median       3Q      Max 
-1.97975 -0.71403  0.06443  0.51076  2.57601 

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
(Intercept)            5.590e-17  6.560e-02   0.000  1.00000    
scale(PE$age)         -2.156e-01  6.964e-02  -3.096  0.00225 ** 
scale(PE$activeyears)  3.975e-01  6.964e-02   5.708 4.16e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.9277 on 197 degrees of freedom
Multiple R-squared:  0.1481,	Adjusted R-squared:  0.1394 
F-statistic: 17.12 on 2 and 197 DF,  p-value: 1.394e-07
