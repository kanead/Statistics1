HW8
Welcome to week 8 assignment! We now return to our cognitive training example. In this week dataset, we compare the impact of three training conditions (Working Memory training, Physical Exercise, and Designed Sport) on Spatial Reasoning (SR), measured before (pre) and after (post) training.



install.packages("psych")
install.packages("car")  #Use Levene's Test of homogeneity of variance
install.packages("lsr")   #Use Learning statistics with R (lsr) for etaSquared

# Load packages
library(psych)
library(car)
library(lsr)
library(ggplot2)
library(reshape)

setwd("/Users/karenyang/Desktop/StatisticsOne12weeks")
cog = read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/Stats1.13.HW.02.txt", header = T)
head(cog)
  subject condition time SR
1       1        WM  pre 11
2       2        WM  pre 13
3       3        WM  pre 16
4       4        WM  pre 11
5       5        WM  pre  8
6       6        WM  pre 15

describeBy(cog, cog$condition)
group: DS
           var  n  mean   sd median trimmed  mad min max range  skew kurtosis   se
subject      1 32 40.50 4.68   40.5    40.5 5.93  33  48    15  0.00    -1.32 0.83
condition*   2 32  1.00 0.00    1.0     1.0 0.00   1   1     0   NaN      NaN 0.00
time*        3 32  1.50 0.51    1.5     1.5 0.74   1   2     1  0.00    -2.06 0.09
SR           4 32 13.47 2.84   13.5    13.5 2.22   6  19    13 -0.22    -0.05 0.50
------------------------------------------------------------------------------------- 
group: PE
           var  n  mean   sd median trimmed  mad min max range skew kurtosis   se
subject      1 32 24.50 4.68   24.5   24.50 5.93  17  32    15 0.00    -1.32 0.83
condition*   2 32  2.00 0.00    2.0    2.00 0.00   2   2     0  NaN      NaN 0.00
time*        3 32  1.50 0.51    1.5    1.50 0.74   1   2     1 0.00    -2.06 0.09
SR           4 32 12.09 2.41   12.0   12.08 2.97   7  17    10 0.03    -0.57 0.43
------------------------------------------------------------------------------------- 
group: WM
           var  n  mean   sd median trimmed  mad min max range  skew kurtosis   se
subject      1 32  8.50 4.68    8.5    8.50 5.93   1  16    15  0.00    -1.32 0.83
condition*   2 32  3.00 0.00    3.0    3.00 0.00   3   3     0   NaN      NaN 0.00
time*        3 32  1.50 0.51    1.5    1.50 0.74   1   2     1  0.00    -2.06 0.09
SR           4 32 12.41 2.26   13.0   12.46 2.97   8  17     9 -0.17    -0.67 0.40


Q1. Using a dependent t-test, is the difference between pre and post-test scores significant? Yes, Cohens D statistic shows 0.75 as the 
magnitude of size effect between the 2 groups, which is 3/4 of a 1 unit standard deviation. 
cog.pre = subset(cog, cog$time == "pre")
cog.post = subset(cog, cog$time == "post")

cog.pre.out = describe(cog.pre)
cog.pre.out
           var  n  mean    sd median trimmed   mad min max range  skew kurtosis   se
subject      1 48 24.50 14.00   24.5   24.50 17.79   1  48    47  0.00    -1.28 2.02
condition*   2 48  2.00  0.83    2.0    2.00  1.48   1   3     2  0.00    -1.56 0.12
time*        3 48  2.00  0.00    2.0    2.00  0.00   2   2     0   NaN      NaN 0.00
SR           4 48 12.02  2.53   12.0   12.03  2.22   6  18    12 -0.02    -0.10 0.37
cog.post.out = describe(cog.post)
cog.post.out
           var  n  mean    sd median trimmed   mad min max range skew kurtosis   se
subject      1 48 24.50 14.00   24.5   24.50 17.79   1  48    47 0.00    -1.28 2.02
condition*   2 48  2.00  0.83    2.0    2.00  1.48   1   3     2 0.00    -1.56 0.12
time*        3 48  1.00  0.00    1.0    1.00  0.00   1   1     0  NaN      NaN 0.00
SR           4 48 13.29  2.45   13.5   13.28  2.22   9  19    10 0.06    -0.49 0.35

cohensD(cog.pre$SR, cog.post$SR, method="paired")
[1] 0.7541032

Q2.  Create subsets for each training condition. Which group shows no difference between pre and post-test scores? PE group
cog.pre.WM = subset(cog.pre, cog.pre$condition == "WM")
cog.pre.PE = subset(cog.pre, cog.pre$condition == "PE")
cog.pre.DS = subset(cog.pre, cog.pre$condition == "DS")
cog.post.WM = subset(cog.post, cog.post$condition == "WM")
cog.post.PE = subset(cog.post, cog.post$condition == "PE")
cog.post.DS = subset(cog.post, cog.post$condition == "DS")

cohensD(cog.pre.WM$SR, cog.post.WM$SR, method="paired")
[1] 1.097888
cohensD(cog.pre.PE$SR, cog.post.PE$SR, method="paired")
[1] 0.05054202
cohensD(cog.pre.DS$SR, cog.post.DS$SR, method="paired")
[1] 1.424205

Q3.  Which training group shows the largest effect size for the difference pre-test to post-test?  DS group


Q4.  Reshape the data into a wide format, and create a new variable for gain score. Now subset the new dataframe based on the training conditions. Which comparison between training conditions does not show a significant difference?  None, all are statistically significant

library(reshape)
data.wide = cast(cog, subject + condition ~time)
data.wide$gain = data.wide$post - data.wide$pre
wm.wide = subset(data.wide, data.wide$condition=="WM")
pe.wide = subset(data.wide, data.wide$condition=="PE")
ds.wide = subset(data.wide, data.wide$condition=="DS")

t.test(wm.wide$gain, pe.wide$gain, var.equal=T)
	Two Sample t-test

data:  wm.wide$gain and pe.wide$gain
t = 2.907, df = 30, p-value = 0.006801
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.3718327 2.1281673
sample estimates:
mean of x mean of y 
   1.3125    0.0625
t.test(wm.wide$gain, ds.wide$gain, var.equal=T)
	Two Sample t-test

data:  wm.wide$gain and ds.wide$gain
t = -2.1555, df = 30, p-value = 0.03927
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -2.19089374 -0.05910626
sample estimates:
mean of x mean of y 
   1.3125    2.437
t.test(ds.wide$gain, pe.wide$gain, var.equal=T)

Q5.  To compare the gain scores across all groups, we now turn to ANOVA. Is the homogeneity of variance assumption violated? No
leveneTest(data.wide$gain, data.wide$condition)
Levenes Test for Homogeneity of Variance (center = median)
      Df F value Pr(>F)
group  2  0.5858 0.5609    #Cannot reject null hypothesis of equal variances, you want to retain the null hypothesis
      45  
Q6.  Run an ANOVA model on the gain scores as a function of training condition. Is the effect of condition significant?  Yes
aov.model = aov(data.wide$gain ~ data.wide$condition) 
summary(aov.model)                    
                                Df Sum Sq Mean Sq F value  Pr(>F)    
data.wide$condition  2  45.17  22.583   11.51 9.2e-05 ***
Residuals                 45  88.31   1.962                    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Q7.  What is the corresponding eta-squared value? (round to 2 decimal places) 0.34
need to use library(lsr)
etaSquared(aov.model, anova=T)

                                       eta.sq         eta.sq.part       SS df       MS        F            p
data.wide$condition 0.3383799   0.3383799 45.16667  2 22.58333 11.50743 9.198046e-05
Residuals                  0.6616201          NA     88.31250 45 1.96250       NA           NA

Q8.  Are the eta-squared and partial eta-squared value different in this case? No

Q9.  Let us now run post-hoc comparisons (Tukey HSD). Which two groups do not significantly differ from one another when considering gain scores? DS and WM
TukeyHSD(aov.model)
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = data.wide$gain ~ data.wide$condition)

$`data.wide$condition`
                  diff         lwr            upr               p adj
PE-DS -2.375 -3.57539235 -1.17460765 0.0000532
WM-DS -1.125 -2.32539235  0.07539235 0.0703312
WM-PE  1.250  0.04960765  2.45039235 0.0395985


Q10.  Based on these data, which training condition should you choose to target some improvements in spatial reasoning?  WM or DS
WM
PE
WM or DS
DS

