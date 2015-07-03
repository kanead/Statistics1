Homework 5


setwd("/Users/karenyang/Desktop/StatisticsOne12weeks")
search()
data1 <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/Stats1.13.HW.04.txt", header = T)

Q1.Run a regression model with salary as the outcome variable and years of experience as the predictor variable. What is the 95% confidence interval for the regression coefficient? Type your answer exactly as it appears in R but include only two decimal places (for example, if the 95% confidence interval is -1 to +1 then type -1.00 1.00)  4930.12 6345.48
model1 <- lm(data1$salary ~ data1$years)
confint(model1)

                2.5 %   97.5 %
(Intercept) 27360.825 38259.76
data1$years  4930.124  6345.48
cor.test(data1$salary, data1$years)

#Pearson's product-moment correlation

#data:  data1$salary and data1$years
#t = 15.7103, df = 198, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.6760201 0.8008691
sample estimates:
      cor 
0.7448961 


Q2. Run a regression model with salary as the outcome variable and courses as the predictor variable. What is the 95% confidence interval for the regression coefficient?  560.09  872.09
model2 <- lm(data1$salary ~ data1$courses)
confint(model2)
                   2.5 %     97.5 %
(Intercept)   59656.5374 65782.3180
data1$courses   560.0886   872.0908
cor.test(data1$salary, data1$courses)
#Pearson's product-moment correlation

data:  data1$salary and data1$courses
t = 9.0521, df = 198, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.4349309 0.6323038
sample estimates:
      cor 
0.5410249 

Q3. Run a multiple regression model with both predictors and compare it with both the model from Question 1 and the model from Question 2. Is the model with both predictors significantly better than:  both single predictor models
both single predictor models
the single predictor model based on years of experience
the single predictor model based on courses
none of the above

model3 <-lm(data1$salary ~ data1$years + data1$courses) 
summary(model3)
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

confint(model3)
                   2.5 %     97.5 %
(Intercept)   26510.6684 36215.1289
data1$years    4140.3574  5472.9459
data1$courses   319.0987   552.1396

anova(model1, model2, model3)
Analysis of Variance Table

Model 1: data1$salary ~ data1$years
Model 2: data1$salary ~ data1$courses
Model 3: data1$salary ~ data1$years + data1$courses
  Res.Df        RSS Df   Sum of Sq     F    Pr(>F)    
1    198 1.1011e+10                                   
2    198 1.7496e+10  0 -6484923673                    
3    197 8.6297e+09  1  8866082101 202.4 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Q4. Run a standardized multiple regression model with both predictors. Do the confidence interval values differ from the corresponding unstandardized model?  Yes
No
Yes
model4 <-lm(scale(data1$salary) ~ scale(data1$years) + scale(data1$courses)) 
summary(model4)
confint(model4)
                           2.5 %     97.5 %
(Intercept)          -0.08278152 0.08278152
scale(data1$years)    0.54704580 0.72311441
scale(data1$courses)  0.24108759 0.41715620
Q5.  What function could you use to take a random subset of the data?  sample

Q6.  Run the following command in R: set.seed(1). Now take a random subset of the original data so that N=15. Is the correlation coefficient between salary and years of experience in this sample higher or lower than in the whole data set?  Lower
Higher
Lower
set.seed(1)
data.15 <- data1[sample(nrow(data1), 15),]

cor.test(data.15$salary, data.15$years)
#Pearson's product-moment correlation
data:  data.15$salary and data.15$years
t = 2.6344, df = 13, p-value = 0.02062
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.1113357 0.8464182
sample estimates:
      cor 
0.5899517 

cor.test(data1$salary, data1$years)

#Pearson's product-moment correlation

data:  data1$salary and data1$years
t = 15.7103, df = 198, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.6760201 0.8008691
sample estimates:
      cor 
0.7448961 
Q7.  Take a subset of the original data from row 51 to 70. What is the percentage of variance explained by a multiple regression model with both predictors (Provide your result with no decimal place)  85
set.seed(1)
data.20 <- data1[51:70, ]
model5 <-lm(data.20$salary ~ data.20$years + data.20$courses) 
summary(model5)
Call:
lm(formula = data.20$salary ~ data.20$years + data.20$courses)

Residuals:
   Min     1Q Median     3Q    Max 
-11361  -4423   2427   3641   6866 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)      31599.5     5962.1   5.300 5.88e-05 ***
data.20$years     4566.6      899.0   5.080 9.28e-05 ***
data.20$courses    566.0      146.5   3.865  0.00124 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5584 on 17 degrees of freedom
Multiple R-squared:  0.8509,	Adjusted R-squared:  0.8333 
F-statistic: 48.49 on 2 and 17 DF,  p-value: 9.451e-08

Q8.  Using model comparison, which model provides the best fit for the subsetted data from Question 7?  model3.subset
model1.subset = lm(data.subset$salary ~ data.subset$years)
model2.subset = lm(data.subset$salary ~ data.subset$courses)
model3.subset = lm(data.subset$salary ~ data.subset$years + data.subset$courses)
They are all equal

model6 <- lm(data.20$salary ~ data.20$years)
model7 <-lm(data.20$salary ~ data.20$courses)
anova(model6, model7, model5)
Analysis of Variance Table

Model 1: data.20$salary ~ data.20$years
Model 2: data.20$salary ~ data.20$courses
Model 3: data.20$salary ~ data.20$years + data.20$courses
  Res.Df        RSS Df  Sum of Sq      F   Pr(>F)    
1     18  995791559                                  
2     18 1334669585  0 -338878026                    
3     17  530099091  1  804570495 25.802 9.28e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Q9.  What is the correlation between the salary values predicted by the multiple regression model and the actual salary scores in the subsetted data? (Provide your result rounded to 2 decimal places)  0.92
model5 <-lm(data.20$salary ~ data.20$years + data.20$courses)
data.20$predicted <- fitted(model5)
ata.20$predicted <- fitted(model5)
cor(data.20$salary, data.20$predicted)  #[1] 0.922422


Q10.  Compute the correlation between the scores predicted by the multiple regression model and the residuals from the same model. Is the correlation statistically significant? No
Yes
No

data.20$residuals<- resid(model5)
cor(data.20$predicted, data.20$residuals)   #[1] 3.786342e-16

