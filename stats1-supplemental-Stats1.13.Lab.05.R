# Statistics One, 2013, Lab 5

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlation analyses, including NHSTs
#   Conduct regression analyses, emphasis on standard error, confidence intervals, and model comparison 

# Example
#   A correlational study investigating predictors of physcial endurance in adults
#     Outcome variable (Y) is physical endurance
#     Predictors (X) are age and number of years actively engaged in exercise/sports
#     Initial analyses assume a sample size of N = 200 
#     Analyses are then repeated with a sample size of N = 20

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
install.packages("psych")
install.packages("ggplot2")

# Load packages
library(psych)
library(ggplot2)

# Read data into a dataframe called PE (physical endurance)
PE <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.Lab.05.txt", header = T)

# If you want to view the data
# View(PE)
edit(PE)

# Summary statistics
describe(PE) 
            var   n   mean    sd median trimmed   mad min max range skew kurtosis   se
pid           1 200 101.81 58.85  101.5  101.71 74.87   1 204   203 0.01    -1.21 4.16
age           2 200  49.41 10.48   48.0   49.46 10.38  20  82    62 0.06    -0.14 0.74
activeyears   3 200  10.68  4.69   11.0   10.57  4.45   0  26    26 0.30     0.46 0.33
endurance     4 200  26.50 10.84   27.0   26.22 10.38   3  55    52 0.22    -0.44 0.77

# Illustration of standard error calculation
# Standard error = Standard deviation divided by the square root of sample size
# se = sd / sqrt(N)
table1 <- describe(PE)
table1
age.sd <- table1[2,4]
age.sd   #[1] 10.48343
age.n <- table1[2,2]
age.n    3[1] 200
age.se <- table1[2,4] / sqrt(table1[2,2])
age.se   #[1] 0.7412904
age.se == table1[2,13]  #[1] TRUE

# Correlation analysis 
cor(PE[2:4]) 
                    age activeyears   endurance
age          1.00000000   0.3289909 -0.08483813
activeyears  0.32899092   1.0000000  0.32654016
endurance   -0.08483813   0.3265402  1.00000000

# NHST for each correlation coefficient
cor.test(PE$age, PE$activeyears)
#Pearson's product-moment correlation
#data:  PE$age and PE$activeyears
#t = 4.9022, df = 198, p-value = 1.969e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1993491 0.4473145
#sample estimates:
#      cor 
#0.3289909 

cor.test(PE$endurance, PE$activeyears)
#	Pearson's product-moment correlation
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
# Save the correlations in a table to illustrate calculation of regression coefficients
table2 <- cor(PE[2:4])
                    age activeyears   endurance
age          1.00000000   0.3289909 -0.08483813
activeyears  0.32899092   1.0000000  0.32654016
endurance   -0.08483813   0.3265402  1.00000000

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
# Illustration of calculation of regression coefficient
# Regression coefficient = correlation coefficient * (standard deviation of Y / standard deviation of X)
# B = r * (sdy / sdx)
table2
table2[3,1]    #[1] -0.08483813
model1.B <- table2[3,1] * (table1[4,4] / table1[2,4])
model1.B       #[1] -0.08772068

# Illustration of calculation of standard error of the regression coefficient
# Standard error = Square root [ (Sums of Squares.Residual / (N - 2) ) / (Sums of Squares.X) ]
# se.B = sqrt[ (SS.resid / (N-2) ) / SS.X )]
table3 <- anova(model1)
table3
Analysis of Variance Table
Response: PE$endurance
           Df  Sum Sq Mean Sq F value Pr(>F)
PE$age      1   168.3  168.29  1.4354 0.2323
Residuals 198 23213.7  117.24

SS.resid <- table3[2,2]
SS.resid
df <- table3[2,1]
df       #[1] 198
SS.X <- table3[1,2] + table3[2,2]
SS.X     #[1] 23213.71
se.B <- sqrt((SS.resid/df)/SS.X) 
se.B     #[1] 0.07081069

# Print 95% confidence interval for the regression coefficient
confint(model1) 
                 2.5 %     97.5 %
(Intercept) 23.5419503 38.1274847
PE$age      -0.2321052  0.0566638
# Illustration of calculation of confidence interval
# Upper value = B + (tcrit * se.B) and Lower value = B - (tcrit * se.B)
tcrit <- qt(c(.025, .975), df = 198)
tcrit     #> tcrit
[1] -1.972017  1.972017
interval <- -0.08772 + (tcrit*se.B)
interval    #[1] -0.22735992  0.05191992
# Scatterplot with confidence interval around the regression line
ggplot(PE, aes(x = age, y = endurance)) + geom_smooth(method = "lm") + 
  geom_point() 
  
  
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
confint(model2) #Prints 95% confidence intervals for the regression coefficients
                    2.5 %    97.5 %
(Intercept)    14.8685257 22.008638
PE$activeyears  0.4488298  1.061506
ggplot(PE, aes(x = activeyears, y = endurance)) + geom_smooth(method = "lm") + 
  geom_point() 


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
confint(model3) # Prints 95% confidence intervals for the regression coefficients
                    2.5 %      97.5 %
(Intercept)    20.8447350 34.56220652
PE$age         -0.3649241 -0.08093226
PE$activeyears  0.6016124  1.23679989
# To visualize model3, save the predicted scores as a new variable and then plot with endurance
PE$predicted <- fitted(model3)

ggplot(PE, aes(x = predicted, y = endurance)) + geom_smooth(method = "lm") + 
  geom_point() 

  
# Conduct a model comparison NHST to compare the fit of model2 to the fit of model3
anova(model2, model3)
Analysis of Variance Table

Model 1: PE$endurance ~ PE$activeyears
Model 2: PE$endurance ~ PE$age + PE$activeyears
  Res.Df   RSS Df Sum of Sq      F   Pr(>F)   
1    198 20889                                
2    197 19920  1    969.26 9.5858 0.002247 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Regression analyses, standardized
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
confint(model1.z)

model2.z <- lm(scale(PE$endurance) ~ scale(PE$activeyears))
summary(model2.z)
confint(model2.z)
                   2.5 %     97.5 %
(Intercept)   -0.1392904 0.13929038
scale(PE$age) -0.2244780 0.05480179
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
confint(model3.z)
                           2.5 %      97.5 %
(Intercept)           -0.1293605  0.12936051
scale(PE$age)         -0.3529325 -0.07827278
scale(PE$activeyears)  0.2601416  0.53480133
# Conduct a model comparison NHST to compare the fit of model2.z to the fit of model3.z
anova(model2.z, model3.z)
# Note that the F value and the p value are the same as from the unstandardized model comparison
anova(model2, model3)
Analysis of Variance Table

Model 1: PE$endurance ~ PE$activeyears
Model 2: PE$endurance ~ PE$age + PE$activeyears
  Res.Df   RSS Df Sum of Sq      F   Pr(>F)   
1    198 20889                                
2    197 19920  1    969.26 9.5858 0.002247 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Now take a random subset of the data such that N = 20 
PE.20 <- PE[sample(nrow(PE), 20), ]

# Summary statistics
describe(PE.20) 
            var  n   mean    sd median trimmed   mad   min    max  range  skew kurtosis    se
pid           1 20 104.40 54.08  94.00  102.38 51.15 15.00 204.00 189.00  0.37    -0.99 12.09
age           2 20  49.65  9.59  47.50   49.50  8.90 28.00  69.00  41.00  0.02    -0.30  2.15
activeyears   3 20  11.50  4.95  11.00   11.38  3.71  3.00  24.00  21.00  0.35     0.23  1.11
endurance     4 20  26.30 10.77  27.00   26.44  8.15  5.00  47.00  42.00 -0.06    -0.52  2.41
predicted     5 20  27.21  4.47  27.75   27.06  4.13 19.87  37.06  17.19  0.14    -0.67  1.00
# Correlation analysis 
round(cor(PE.20[2:4]), 2) # Round to 2 decimal places 
              age activeyears endurance
age          1.00        0.27     -0.11
activeyears  0.27        1.00      0.53
endurance   -0.11        0.53      1.00
cor.test(PE.20$age, PE.20$activeyears)
#Pearson's product-moment correlation
#data:  PE.20$age and PE.20$activeyears
#t = 1.1952, df = 18, p-value = 0.2475
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.1947348  0.6372135
#sample estimates:
#      cor 
#0.2711523 
cor.test(PE.20$endurance, PE.20$activeyears)
#Pearson's product-moment correlation
#data:  PE.20$endurance and PE.20$activeyears
#t = 2.6204, df = 18, p-value = 0.01734
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1081010 0.7853756
#sample estimates:
#      cor 
#0.5254842 
cor.test(PE.20$endurance, PE.20$age)
#Pearson's product-moment correlation
#data:  PE.20$endurance and PE.20$age
#t = -0.4848, df = 18, p-value = 0.6336
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.5294581  0.3463852
#sample estimates:
#       cor 
#-0.1135391 
# Regression analyses, unstandardized
model1.20 <- lm(PE.20$endurance ~ PE.20$age)
summary(model1.20)
Call:
lm(formula = PE.20$endurance ~ PE.20$age)

Residuals:
     Min       1Q   Median       3Q      Max 
-19.4707  -5.9933   0.0435   5.5314  21.6370 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  32.6292    13.2837   2.456   0.0244 *
PE.20$age    -0.1275     0.2629  -0.485   0.6336  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 10.99 on 18 degrees of freedom
Multiple R-squared:  0.01289,	Adjusted R-squared:  -0.04195 
F-statistic: 0.2351 on 1 and 18 DF,  p-value: 0.6336
confint(model1.20)
                  2.5 %    97.5 %
(Intercept)  4.7211652 60.537306
PE.20$age   -0.6798631  0.424909 
model2.20 <- lm(PE.20$endurance ~ PE.20$activeyears)
summary(model2.20)
Call:
lm(formula = PE.20$endurance ~ PE.20$activeyears)

Residuals:
    Min      1Q  Median      3Q     Max 
-16.584  -2.228   2.772   5.310  14.984 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)  
(Intercept)        13.1430     5.4445   2.414   0.0266 *
PE.20$activeyears   1.1441     0.4366   2.620   0.0173 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 9.415 on 18 degrees of freedom
Multiple R-squared:  0.2761,	Adjusted R-squared:  0.2359 
F-statistic: 6.866 on 1 and 18 DF,  p-value: 0.01734
confint(model2.20)
                      2.5 %    97.5 %
(Intercept)       1.7045628 24.581459
PE.20$activeyears 0.2268061  2.061366
model3.20 <- lm(PE.20$endurance ~ PE.20$age + PE.20$activeyears)
summary(model3.20)
Call:
lm(formula = PE.20$endurance ~ PE.20$age + PE.20$activeyears)

Residuals:
    Min      1Q  Median      3Q     Max 
-16.764  -3.627   1.703   5.404  13.607 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)   
(Intercept)        26.6716    11.3006   2.360  0.03048 * 
PE.20$age          -0.3103     0.2286  -1.357  0.19251   
PE.20$activeyears   1.3072     0.4434   2.948  0.00899 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 9.202 on 17 degrees of freedom
Multiple R-squared:  0.3469,	Adjusted R-squared:   0.27 
F-statistic: 4.515 on 2 and 17 DF,  p-value: 0.02676
confint(model3.20)
                       2.5 %     97.5 %
(Intercept)        2.8295519 50.5137266
PE.20$age         -0.7926412  0.1721078
PE.20$activeyears  0.3718273  2.2426252
# Conduct a model comparison NHST to compare the fit of model2.20 and model3.20
anova(model2.20, model3.20)
Analysis of Variance Table

Model 1: PE.20$endurance ~ PE.20$activeyears
Model 2: PE.20$endurance ~ PE.20$age + PE.20$activeyears
  Res.Df    RSS Df Sum of Sq      F Pr(>F)
1     18 1595.5                           
2     17 1439.6  1    155.95 1.8416 0.1925
