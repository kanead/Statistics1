Homework 4

setwd("/Users/karenyang/Desktop/StatisticsOne12weeks")

install.packages("psych")
library(psych)

data1 <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/Stats1.13.HW.04.txt", header = T)
head(data1)
  ID salary years courses
1  1  67483   5.8      13
2  2  77204   7.4      18
3  3  64972   6.8      23
4  4  94143   8.3      35
5  5  78954   8.1      19
6  6  65154   6.5      12

# Summary statistics
describe(data1)        
        var   n     mean       sd  median  trimmed      mad     min      max   range  skew kurtosis     se
ID        1 200   100.50    57.88   100.5   100.50    74.13     1.0    200.0   199.0  0.00    -1.22   4.09
salary    2 200 75426.44 11149.13 74407.5 75196.26 10710.30 48424.0 103819.0 55395.0  0.19    -0.24 788.36
years     3 200     7.56     1.47     7.6     7.58     1.33     3.3     12.2     8.9 -0.10     0.43   0.10
courses   4 200    17.75     8.42    16.5    17.73     6.67     0.0     36.0    36.0  0.01    -0.37   0.60 

#Q1.  What is the correlation between salary and years of professional experience? 0.74
#Q2.  What is the correlation between salary and courses completed?  0.54
cor(data1[2:4])
round(cor(data1[2:4]), 2) # Round to 2 decimal places 
        salary years courses
salary    1.00  0.74    0.54
years     0.74  1.00    0.33
courses   0.54  0.33    1.00

#Q3.  What is the percentage of variance explained in a regression model with salary as the outcome variable and 
#professional experience as the predictor variable?  55%
model1 <- lm(data1$salary ~ data1$years)
summary(model1)
Call:
lm(formula = data1$salary ~ data1$years)

Residuals:
     Min       1Q   Median       3Q      Max 
-21356.2  -5290.8    257.5   4797.1  20298.9 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  32810.3     2763.4   11.87   <2e-16 ***
data1$years   5637.8      358.9   15.71   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 7457 on 198 degrees of freedom
Multiple R-squared:  0.5549,	Adjusted R-squared:  0.5526 
F-statistic: 246.8 on 1 and 198 DF,  p-value: < 2.2e-16


#Q4. Compared to the model from Question 3, would a regression model predicting salary from the number of courses be considered a better fit to the data? No.

#Q5.  Now let's include both predictors (years of professional experience and courses completed) in a regression model with salary as the outcome. Now what is the percentage of variance explained?  65%
model2 <-lm(data1$salary ~ data1$years + data1$courses) 
summary(model2)
Call:
lm(formula = data1$salary ~ data1$years + data1$courses)

Residuals:
     Min       1Q   Median       3Q      Max 
-17788.6  -4761.9    -66.1   4174.6  19556.4 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   31362.90    2460.46  12.747  < 2e-16 ***
data1$years    4806.65     337.86  14.227  < 2e-16 ***
data1$courses   435.62      59.09   7.373 4.52e-12 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 6619 on 197 degrees of freedom
Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6476 
F-statistic: 183.8 on 2 and 197 DF,  p-value: < 2.2e-16

#Q6.  What is the standardized regression coefficient for years of professional experience, predicting salary?  0.74
model3 <- lm(scale(data1$salary) ~ scale(data1$years))
summary(model3)
Call:
lm(formula = scale(data1$salary) ~ scale(data1$years))

Residuals:
    Min      1Q  Median      3Q     Max 
-1.9155 -0.4745  0.0231  0.4303  1.8207 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)        -1.460e-16  4.730e-02    0.00        1    
scale(data1$years)  7.449e-01  4.741e-02   15.71   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.6689 on 198 degrees of freedom
Multiple R-squared:  0.5549,	Adjusted R-squared:  0.5526 
F-statistic: 246.8 on 1 and 198 DF,  p-value: < 2.2e-16

cor(data1$years,data1$salary)  #Pearson correlation the same as the standardized regression coefficient in table above
[1] 0.7448961

#Q7.  What is the standardized regression coefficient for courses completed, predicting salary?  0.54
model4 <- lm(scale(data1$salary) ~ scale(data1$courses))
summary(model4)
Call:
lm(formula = scale(data1$salary) ~ scale(data1$courses))

Residuals:
    Min      1Q  Median      3Q     Max 
-2.1052 -0.4910 -0.0552  0.5409  3.3929 

Coefficients:
                       Estimate Std. Error t value Pr(>|t|)    
(Intercept)          -1.663e-16  5.962e-02   0.000        1    
scale(data1$courses)  5.410e-01  5.977e-02   9.052   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.8431 on 198 degrees of freedom
Multiple R-squared:  0.2927,	Adjusted R-squared:  0.2891 
F-statistic: 81.94 on 1 and 198 DF,  p-value: < 2.2e-16

cor(data1$courses,data1$salary) 

#Q8.  What is the mean of the salary distribution predicted by the model including both years of professional experience and courses completed as predictors? (with 0 decimal places)  $75,430

data1$predicted <- fitted(model2)       #fitted refers to predicted values of y
plot(data1$)> data1$predicted <- fitted(model2)
summary(data1$predicted)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  52450   70040   74660   75430   80180  100500
  
#Q9. What is the mean of the residual distribution for the model predicting salary from both years of professional experience and courses completed? (with 0 decimal places)   0
data1$residuals<- resid(model2)
summary(data1$residuals)
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-17790.00  -4762.00    -66.06      0.00   4175.00  19560.00 

#Q10.  Are the residuals from the regression model with both predictors normally distributed?  Yes  