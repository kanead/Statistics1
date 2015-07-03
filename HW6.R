HW6
setwd("/Users/karenyang/Desktop/StatisticsOne12weeks")
install.packages("psych")
library(psych)
data1 <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.HW.06.txt", header = T)
head(data1)
describe(data1)
            var   n     mean       sd  median  trimmed      mad     min      max   range  skew kurtosis     se
ID            1 200   100.50    57.88   100.5   100.50    74.13     1.0    200.0   199.0  0.00    -1.22   4.09
salary        2 200 75426.44 11149.13 74407.5 75196.26 10710.30 48424.0 103819.0 55395.0  0.19    -0.24 788.36
years         3 200     7.56     1.47     7.6     7.58     1.33     3.3     12.2     8.9 -0.10     0.43   0.10
courses       4 200    17.75     8.42    16.5    17.73     6.67     0.0     36.0    36.0  0.01    -0.37   0.60
profession*   5 200     1.92     0.76     2.0     1.89     1.48     1.0      3.0     2.0  0.14    -1.25   0.05

Question1.  In a model predicting salary, what is the unstandardized regression coefficient for years, assuming years is the only predictor variable in the model?  5638

model0 <- lm(data1$salary ~ data1$years)
summary(model0)
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

Q2.In a model predicting salary, what is the 95% confidence interval for the unstandardized regression coefficient for years, assuming years is the only predictor variable in the model? 4930 6345

confint(model0) 
                2.5 %   97.5 %
(Intercept) 27360.825 38259.76
data1$years  4930.124  6345.48

Q3.In a model predicting salary, what is the unstandardized regression coefficient for years, assuming years and courses are both included as predictor variables in the model?  4807
model1 <- lm(data1$salary ~ data1$years + data1$courses)
summary(model1)
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

Q4. In a model predicting salary, what is the 95% confidence interval for the unstandardized regression coefficient for years, assuming years and courses are both included as predictor variables in the model? 4140 5473
confint(model1)
                         2.5 %     97.5 %
   (Intercept)   26510.6684 36215.1289
data1$years    4140.3574  5472.9459
data1$courses   319.0987   552.1396

Q5.  What is the predicted difference in salary between Doctors and Lawyers assuming an equal and average number of years and courses?  -9204
profession.code <- C(data1$profession, treatment)  #doctor is the reference group set to zeros for dummy variables 1 and 2, lawyer and teacher given dummy codes
model2 <- lm(data1$salary ~ data1$years + data1$courses + (profession.code))
summary(model2)
Call:
lm(formula = data1$salary ~ data1$years + data1$courses + (profession.code))

Residuals:
     Min       1Q   Median       3Q      Max 
-15707.2  -3155.9    -37.7   3228.1  14894.4 

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
(Intercept)             59573.76    2901.53  20.532  < 2e-16 ***
data1$years              2627.15     307.33   8.548 3.60e-15 ***
data1$courses             214.26      47.57   4.504 1.14e-05 ***
profession.codelawyer   -9204.13     942.59  -9.765  < 2e-16 ***
profession.codeteacher -15902.77    1285.47 -12.371  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4945 on 195 degrees of freedom
Multiple R-squared:  0.8073,	Adjusted R-squared:  0.8033 
F-statistic: 204.2 on 4 and 195 DF,  p-value: < 2.2e-16

Q.6 Is the predicted difference between Doctors and Lawyers statistically significant?  Yes

The t-value in summary output is -9.765, which is way above 2 and the p-value is very small. Also, 

confint(model2)  #if the confidence interval does not include zero, then it is statistically significant
                                  2.5 %      97.5 %
(Intercept)                53851.3616  65296.1666
data1$years              2021.0228   3233.2755
data1$courses             120.4468    308.0672
profession.codelawyer  -11063.1194  -7345.1455
profession.codeteacher -18437.9792 -13367.5649

Q7. What is the predicted difference in salary between Doctors and Teachers assuming an equal and average number of years and courses? -15903
 -15902.77
 
 Q8. Is the predicted difference between Doctors and Teachers statistically significant? Yes
 #if the confidence interval does not include zero, then it is statistically significant
 the t-value is -12.371 and the p-value is very small.
 
 Q9. What is the actual difference in mean salary between Doctors and Teachers? 24611
 
 tapply(data1$salary, data1$profession, mean)
   doctor   lawyer  teacher 
87344.77 73489.21 62733.67 
> 87344.77-62733.67
[1] 24611.1

Q10.  What combination of predictors represents the best model in terms of predicting salary? Years, courses, and profession
Years and courses
Years and profession
Courses and profession
Years, courses, and profession