
HW 10

Welcome to assignment 10! This week we are going to work on an example at the intersection of decision-making and global warming. The simulated dataset includes a dependent variable, change, for a list of 27 countries. Change indicates whether these countries are willing to take action now against global warming, or if they would rather wait and see (1 = act now, 0 = wait and see). Predictors include: median age (age), education index (educ), gross domestic product (gdp), and CO2 emissions (co2).

# If necessary, install packages
install.packages("psych")
install.packages("aod")
install.packages("QuantPsyc")

# Load packages
library(psych)
library(aod)
library(QuantPsyc)

# Read the data into a dataframe called BL
data1 <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.HW.10.txt", header = T)

edit(data1)
tail(data1)
       country change  age educ  gdp co2
22   Argentina      1 30.3 0.74 0.40 192
23      Canada      1 40.7 0.96 1.70 507
24      Poland      1 38.2 0.85 0.50 309
25     Finland      1 41.6 0.97 0.20  56
26     Uruguay      0 33.7 0.85 0.04   8
27 SouthAfrica      0 24.7 0.84 0.40 445

describe(data1)
         var  n   mean      sd median trimmed    mad   min     max   range  skew kurtosis     se
country*   1 27  13.41    7.52  13.00   13.39   8.90  1.00   26.00   25.00  0.03    -1.28   1.45
change     2 27   0.67    0.48   1.00    0.70   0.00  0.00    1.00    1.00 -0.67    -1.61   0.09
age        3 27  33.11    8.08  33.70   33.25  10.38 19.10   44.60   25.50 -0.09    -1.52   1.56
educ       4 27   0.76    0.15   0.78    0.78   0.21  0.44    0.97    0.53 -0.46    -0.90   0.03
gdp        5 27   1.91    3.13   0.80    1.28   0.89  0.02   14.90   14.88  2.83     8.43   0.60
co2        6 27 880.67 1624.49 376.00  489.43 249.08  8.00 7031.00 7023.00  2.80     6.98 312.63

Q1. What is the mean population age for the countries which voted to take action against global warming? (round to 2 decimal places) 35.78
> changetable <- table(data1$change)
> changetable

 0  1 
 9 18 
 
> describe.by(data1$age,data1$change)
group: 0
  var n  mean   sd median trimmed  mad  min  max range skew kurtosis   se
1   1 9 27.77 7.08   25.9   27.77 6.97 19.1 38.5  19.4 0.32    -1.66 2.36
------------------------------------------------------------------------------ 
group: 1
  var  n  mean   sd median trimmed  mad min  max range  skew kurtosis   se
1   1 18 35.78 7.33  38.95   35.96 7.49  24 44.6  20.6 -0.31    -1.66 1.73
 

Q2. Run a logistic regression including all predictor variables. Which predictors are significant in this model? educ and age
educ and age,     educ, age, gdp,     gdp, age, co2,    all of them
lrfit <- glm(data1$change ~ data1$age + data1$educ + data1$gdp + data1$co2, family = binomial)
summary(lrfit)
Call:
glm(formula = data1$change ~ data1$age + data1$educ + data1$gdp + 
    data1$co2, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.8413  -0.3307   0.1902   0.5212   1.6435  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)   
(Intercept) -1.748e-02  3.571e+00  -0.005    0.996   
data1$age    3.721e-01  1.424e-01   2.612    0.009 **
data1$educ  -1.373e+01  6.942e+00  -1.977    0.048 * 
data1$gdp   -7.554e-02  4.646e-01  -0.163    0.871   
data1$co2   -3.810e-04  7.803e-04  -0.488    0.625   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 34.372  on 26  degrees of freedom     #Null deviance - residual deviance is approximately 16. See question 6 below.
Residual deviance: 18.068  on 22  degrees of freedom
AIC: 28.068

Number of Fisher Scoring iterations: 6

Q3.  What does the negative value for the estimate of educ means?  All of the above
Countries with a lower education index score are more likely to chose to act now, Countries with a higher education index score are more likely to chose to wait and see,
Educ and change are negatively correlated, All of the above

Q4.  What is the confidence interval for educ, using profiled log-likelihood? (round to 2 decimal places, and give the lower bound first and the upper bound second, separated by a space)  -31.17 -3.03
confint(lrfit)
Waiting for profiling to be done...
                    2.5 %      97.5 %
(Intercept)  -7.120294915  7.62212837
data1$age     0.151757331  0.73814438
data1$educ  -31.171217249 -3.03349629
data1$gdp    -1.677996402  0.67880052
data1$co2    -0.001889047  0.00185111

Q5.  What is the confidence interval for age, using standard errors? (round to 2 decimal places, and give the lower bound first and the upper bound second, separated by a space)  0.09 0.65
confint.default(lrfit)
                    2.5 %       97.5 %
(Intercept)  -7.016319882  6.981360625
data1$age     0.092877885  0.651260815
data1$educ  -27.332984665 -0.119112070
data1$gdp    -0.986127869  0.835048069
data1$co2    -0.001910404  0.001148391

Compare Chi-Squared with null model (no predictors)
Q6.  Compare the present model with a null model. What is the difference in deviance for the two models? (round to 2 decimal places) 16.30
with(lrfit, null.deviance - deviance)  #difference in deviance for the 2 models
[1] 16.30328

Q7.  How many degrees of freedom are there for the difference between the two models? 4
with(lrfit, df.null - df.residual) #df for the difference between the 2 models
[1] 4

Q8.  Is the p-value for the difference between the two models significant? Yes, 0.002
Yes or No
with(lrfit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
[1] 0.002638074

Q9.  Do chi-squared values differ significantly if you drop educ as a predictor in the model? Yes
Yes or No
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 3)
Chi-squared test:
X2 = 3.9, df = 1, P(> X2) = 0.048

To calculate the odds ratio:
exp(coef(lrfit))  #exponentiated coefficients
 (Intercept)    data1$age   data1$educ    data1$gdp    data1$co2 
9.826723e-01 1.450734e+00 1.093586e-06 9.272427e-01 9.996191e-01 

Q10. What is the percentage of cases that can be classified correctly based on our model? 0.81, look at overall model classification
ClassLog(lrfit, data1$change)
$rawtab
       resp
         0  1
  FALSE  6  2
  TRUE   3 16

$classtab
       resp
                0         1
  FALSE 0.6666667 0.1111111
  TRUE  0.3333333 0.8888889

$overall
[1] 0.8148148

$mcFadden
[1] 0.4743218