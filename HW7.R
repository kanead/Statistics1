HW7

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")
setwd("/Users/karenyang/Desktop/StatisticsOne12weeks")
getwd()
# If necessary, install packages
install.packages("psych")
install.packages("ggplot2")
install.packages("multilevel")

# Load packages
library(psych)
library(ggplot2)
library(multilevel)


data1 <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.HW.07.txt", header = T)

> head(data1)
  diverse happy extra
1       2     1  3.00
2       2     1  3.00
3       2     1  1.00
4       2     1  2.50
5       1     1  3.25
6       2     1  3.25

Q1.What is the correlation between extraversion and happiness? 0.19
cor(data1$extra,data1$happy)
[1] 0.193727

Q2. What is the correlation between extraversion and diversity of life experience? 0.21
cor(data1$extra,data1$diverse)
[1] 0.2096778
Q3. What is the correlation between diversity of life experience and happiness? 0.21
cor(data1$diverse,data1$happy)
[1] 0.2107102
Q4.  What percentage of variance in happiness is explained by extraversion? 0.04
modeln1 <-lm(data1$happy ~ data1$extra)
Call:
lm(formula = data1$happy ~ data1$extra)

Residuals:
     Min       1Q   Median       3Q      Max 
-2.15357 -0.49763 -0.08476  0.77762  2.05286 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   2.1902     0.3723   5.882 1.84e-08 ***
data1$extra   0.2752     0.1022   2.693  0.00773 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.9467 on 186 degrees of freedom
Multiple R-squared:  0.03753,	Adjusted R-squared:  0.03236 
F-statistic: 7.253 on 1 and 186 DF,  p-value: 0.007726

Q5.  What percentage of variance in happiness is explained by a model with both extraversion and diversity of life experience as predictors? 0.07
modeln2 <-lm(data1$happy ~ data1$extra + data1$diverse)
summary(modeln2)
Call:
lm(formula = data1$happy ~ data1$extra + data1$diverse)

Residuals:
     Min       1Q   Median       3Q      Max 
-2.22437 -0.56277 -0.06535  0.77563  1.94231 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)    1.88612    0.38780   4.864 2.46e-06 ***
data1$extra    0.22224    0.10315   2.155   0.0325 *  
data1$diverse  0.18680    0.07623   2.451   0.0152 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.9342 on 185 degrees of freedom
Multiple R-squared:  0.06779,	Adjusted R-squared:  0.05771 
F-statistic: 6.727 on 2 and 185 DF,  p-value: 0.001513
Q6.  What is the 95% confidence interval for the regression coefficient for extraversion when it is the only predictor of happiness?  .07 .48
confint(modeln1)
               2.5 %    97.5 %
(Intercept) 1.455681 2.9247358
data1$extra 0.073618 0.4768748

Q7.  What is the 95% confidence interval for the regression coefficient for extraversion 
when it and diversity of life experience are both predictors of happiness? .02 .43
confint(modeln2)
                   2.5 %    97.5 %
(Intercept)   1.12103622 2.6512115
data1$extra   0.01874514 0.4257446
data1$diverse 0.03641437 0.3371789

Q8.  What is the unstandardized regression estimate of the indirect effect? 0.05
model.all <- sobel(data1$extra, data1$diverse, data1$happy)
model.all
$`Mod1: Y~X`
             Estimate Std. Error  t value     Pr(>|t|)
(Intercept) 2.1902083  0.3723274 5.882479 1.844771e-08
pred        0.2752464  0.1022042 2.693103 7.725913e-03

$`Mod2: Y~X+M`
             Estimate Std. Error  t value     Pr(>|t|)
(Intercept) 1.8861239 0.38780440 4.863596 2.456085e-06
pred        0.2222449 0.10314908 2.154599 3.248615e-02
med         0.1867966 0.07622512 2.450591 1.519190e-02

$`Mod3: M~X`
             Estimate Std. Error  t value     Pr(>|t|)
(Intercept) 1.6278904 0.35343054 4.605970 7.598125e-06
pred        0.2837394 0.09701697 2.924636 3.877543e-03

$Indirect.Effect
[1] 0.05300156

$SE
[1] 0.02821695

$z.value
[1] 1.878359

$N
[1] 188

Q9.  What is the z-value of the Sobel test? 1.88

Q10.  Do these analyses suggest full mediation, partial mediation, or no mediation?  partial mediation
In model 2 above, the t-value and p-value are 2.450591 1.519190e-02.