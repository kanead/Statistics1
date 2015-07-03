# Statistics One, 2013, Lab 6

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlation analyses
#   Conduct regression analyses, emphasis on models that include a categorical predictor variable

# Example
#   A correlational study investigating predictors of salary among University faculty members
#     Outcome variable (Y) is annual salary in US Dollars ($)
#     Predictors (X) are age, number of years as faculty member, number of publications, and academic department (History, Psychology, Sociology)
#     Sample size is N = 100 

# Check your working directory
# getwd()
# If necessary, set your working directory
setwd("/Volumes/Conway/R")
setwd("/Users/karenyang/Desktop/StatisticsOne12weeks")
getwd()
search()  #check the files loaded
# If necessary, install packages
install.packages("psych")

# Load packages
library(psych)

# Read data into a dataframe called FS (Faculty Salary)
FS <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.Lab.06.txt", header = T)
head(FS)
# If you want to view the data
# View(FS)
edit(FS)

# Summary statistics
describe(FS) 
       var   n      mean       sd   median   trimmed      mad   min    max  range  skew kurtosis      se
salary   1 100 133607.31 39527.13 133049.0 134892.41 49572.95 60072 199606 139534 -0.15    -1.18 3952.71
age      2 100     50.33    10.32     49.0     50.59    11.86    31     67     36 -0.12    -1.07    1.03
years    3 100     24.14     9.54     23.5     24.35    10.38     5     41     36 -0.14    -0.78    0.95
pubs     4 100     66.93    29.87     66.0     65.81    32.62    14    125    111  0.28    -0.81    2.99
dept*    5 100      2.09     0.81      2.0      2.11     1.48     1      3      2 -0.16    -1.46    0.08

# Correlation analysis 
cor(FS[1:4])
          salary       age     years      pubs
salary 1.0000000 0.5109749 0.6491468 0.6993987
age    0.5109749 1.0000000 0.6563766 0.5557383
years  0.6491468 0.6563766 1.0000000 0.6801977
pubs   0.6993987 0.5557383 0.6801977 1.0000000 

# Regression analyses, unstandardized (scaling is for each individual variable so can't compare across variables)
# Model0 demonstrates that age and years are, for the most part redundnant, so we will only use years in the next set of models
model0 <- lm(FS$salary ~ FS$years + FS$age)
summary(model0)
Call:
lm(formula = FS$salary ~ FS$years + FS$age)

Residuals:
   Min     1Q Median     3Q    Max 
-87991  -9708   3556  17969  60696 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  49725.3    15259.1   3.259  0.00154 ** 
FS$years      2284.3      419.5   5.445 3.91e-07 ***
FS$age         571.0      387.6   1.473  0.14396    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 30040 on 97 degrees of freedom
Multiple R-squared:  0.4341,	Adjusted R-squared:  0.4224 
F-statistic:  37.2 on 2 and 97 DF,  p-value: 1.022e-12

model1 <- lm(FS$salary ~ FS$years)
summary(model1)
Call:
lm(formula = FS$salary ~ FS$years)

Residuals:
   Min     1Q Median     3Q    Max 
-82972  -9537   4305  17703  57949 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  68672.5     8259.0   8.315 5.38e-13 ***
FS$years      2689.9      318.4   8.448 2.78e-13 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 30220 on 98 degrees of freedom
Multiple R-squared:  0.4214,	Adjusted R-squared:  0.4155 
F-statistic: 71.37 on 1 and 98 DF,  p-value: 2.779e-13
confint(model1)
                2.5 %    97.5 %
(Intercept) 52282.773 85062.130
FS$years     2058.068  3321.788
model2 <- lm(FS$salary ~ FS$pubs)
summary(model2)
Call:
lm(formula = FS$salary ~ FS$pubs)

Residuals:
   Min     1Q Median     3Q    Max 
-60702 -17388  -1969  13054  73411 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 71672.57    6995.70  10.245  < 2e-16 ***
FS$pubs       925.37      95.53   9.687 5.78e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 28400 on 98 degrees of freedom
Multiple R-squared:  0.4892,	Adjusted R-squared:  0.4839 
F-statistic: 93.84 on 1 and 98 DF,  p-value: 5.782e-16
confint(model2) 
                 2.5 %    97.5 %
(Intercept) 57789.8352 85555.305
FS$pubs       735.7989  1114.933

model3 <- lm(FS$salary ~ FS$years + FS$pubs)
summary(model3)
Call:
lm(formula = FS$salary ~ FS$years + FS$pubs)

Residuals:
   Min     1Q Median     3Q    Max 
-67835 -14589   2362  13358  69613 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  58828.7     7605.9   7.735 9.79e-12 ***
FS$years      1337.4      387.1   3.455 0.000819 ***
FS$pubs        634.9      123.6   5.137 1.44e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 26930 on 97 degrees of freedom
Multiple R-squared:  0.5451,	Adjusted R-squared:  0.5357 
F-statistic: 58.12 on 2 and 97 DF,  p-value: < 2.2e-16
confint(model3)                  
                2.5 %     97.5 %
(Intercept) 43733.1268 73924.1805
FS$years      569.0514  2105.6692
FS$pubs       389.5972   880.2303

# Compare Model3 to both Model1 and Model2 to determine if including both predictors is best
anova(model1, model3)
Analysis of Variance Table
Model 1: FS$salary ~ FS$years
Model 2: FS$salary ~ FS$years + FS$pubs
  Res.Df        RSS Df  Sum of Sq      F    Pr(>F)    
1     98 8.9497e+10                                   
2     97 7.0358e+10  1 1.9139e+10 26.386 1.444e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(model2, model3)
Analysis of Variance Table

Model 1: FS$salary ~ FS$pubs
Model 2: FS$salary ~ FS$years + FS$pubs
  Res.Df        RSS Df Sum of Sq      F    Pr(>F)    
1     98 7.9015e+10                                  
2     97 7.0358e+10  1 8.657e+09 11.935 0.0008186 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Now let's conduct regression analyses that include a categorical predictor
# We need to use dummy codes to represent the nominal variable (dept) as numeric 
# In R there are several ways to do this, the following is just one example, using the function C (for contrasts)
dept.code <- C(FS$dept, treatment)

model4 <- lm(FS$salary ~ FS$years + FS$pubs + (dept.code))
summary(model4)
Call:
lm(formula = FS$salary ~ FS$years + FS$pubs + (dept.code))

Residuals:
   Min     1Q Median     3Q    Max 
-59233 -12490  -1488  13227  67294 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  62365.4     8299.0   7.515 3.13e-11 ***
FS$years      1507.8      379.4   3.974 0.000138 ***
FS$pubs        655.5      120.2   5.452 3.93e-07 ***
dept.codeP   -5872.8     6621.8  -0.887 0.377381    
dept.codeS  -18847.7     6706.6  -2.810 0.006011 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 26080 on 95 degrees of freedom
Multiple R-squared:  0.5824,	Adjusted R-squared:  0.5648 
F-statistic: 33.12 on 4 and 95 DF,  p-value: < 2.2e-16
confint(model4)
                  2.5 %     97.5 %
(Intercept)  45889.8369 78841.0109
FS$years       754.5720  2260.9439
FS$pubs        416.8113   894.2219
dept.codeP  -19018.7013  7273.1563
dept.codeS  -32162.0662 -5533.3194

# Compre Model4 to Model3 to determine if including dept improves the predictions of the model
anova(model3, model4)
Analysis of Variance Table
Model 1: FS$salary ~ FS$years + FS$pubs
Model 2: FS$salary ~ FS$years + FS$pubs + (dept.code)
  Res.Df        RSS Df  Sum of Sq     F  Pr(>F)  
1     97 7.0358e+10                              
2     95 6.4594e+10  2 5764553224 4.239 0.01724 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Let's examine the salary difference between History and Sociology
# To quickly view the means, use the tapply function
tapply(FS$salary, FS$dept, mean)
       H        P        S 
137421.3 129067.4 135015.6 
# The actual means are not that different, so why are the means predicted by the model so different?
# There must be differences across departments in years and/or pubs
# Let's look at years
tapply(FS$years, FS$dept, mean)
       H        P        S 
22.25000 22.08571 27.51351 
# Let's look at pubs
tapply(FS$pubs, FS$dept, mean)
       H        P        S 
63.32143 59.91429 76.29730 

# The actual salary for Sociology is not that different from the other departments BUT they have more years on the job and more publications, on average, than the other departments, so their PREDICTED salary, based on an AVERAGE number of years and publications is lower, which is a more accuracte refelction of the discrepancies across departments.
