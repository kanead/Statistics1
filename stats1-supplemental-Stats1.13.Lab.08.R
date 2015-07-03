# Statistics One, 2013, Lab 8

# Lab goals
#   Conduct group comparisons
#     Dependent t-tests
#     Independent t-tests
#     Analysis of Variance (ANOVA)

# Example
#  Working memory training experiment (N = 120)
#  The dependent variable (DV) is number of items answered correctly on an intelligence test
#  There are three independent variables:
#    Time (2 levels): pre and post training
#    Training (2 levels): training (1) and control (0) (n.training = 80, n.control = 40)
#    Training sessions (4 levels): 8, 12, 17, 19 (for each, n = 20)

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("Users/aconway/Dropbox/STATS1-V2.0/Labs")
setwd("/Users/karenyang/Desktop/StatisticsOne12weeks")
# If necessary, install packages
install.packages("psych")
install.packages("car")
install.packages("lsr")
# Load packages
library(psych)
library(car)
library(lsr)
library(ggplot2)
library(reshape)

# Read data into a dataframe called wm
wm = read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.Lab.08.txt", header = T)

# If you want to view the data
# View(wm)
edit(wm)

# Summary statistics by all groups (control, 8 sessions, 12 sessions, 17 sessions, 19 sessions)
describeBy(wm, wm$cond)
group: control
      var  n  mean   sd median trimmed  mad min max range  skew kurtosis   se
cond*   1 40  1.00 0.00      1    1.00 0.00   1   1     0   NaN      NaN 0.00
pre     2 40  9.97 1.46     10    9.97 1.48   8  12     4  0.09    -1.41 0.23
post    3 40 11.95 2.06     12   12.00 2.22   7  17    10 -0.05    -0.18 0.33
gain    4 40  1.98 1.39      2    1.97 1.48  -1   5     6  0.10     0.03 0.22
train   5 40  0.00 0.00      0    0.00 0.00   0   0     0   NaN      NaN 0.00
------------------------------------------------------------------------------------- 
group: t08
      var  n  mean   sd median trimmed  mad min max range  skew kurtosis   se
cond*   1 20  2.00 0.00    2.0    2.00 0.00   2   2     0   NaN      NaN 0.00
pre     2 20 10.05 1.50   10.0   10.06 1.48   8  12     4  0.01    -1.53 0.34
post    3 20 11.40 2.14   11.5   11.50 2.22   7  15     8 -0.25    -0.84 0.48
gain    4 20  1.35 1.23    1.0    1.44 1.48  -1   3     4 -0.32    -0.82 0.27
train   5 20  1.00 0.00    1.0    1.00 0.00   1   1     0   NaN      NaN 0.00
------------------------------------------------------------------------------------- 
group: t12
      var  n mean   sd median trimmed  mad min max range skew kurtosis   se
cond*   1 20  3.0 0.00      3    3.00 0.00   3   3     0  NaN      NaN 0.00
pre     2 20  9.9 1.45     10    9.88 1.48   8  12     4 0.16    -1.43 0.32
post    3 20 12.5 1.88     12   12.38 2.22  10  17     7 0.48    -0.54 0.42
gain    4 20  2.6 1.27      2    2.50 0.00   0   5     5 0.44    -0.54 0.28
train   5 20  1.0 0.00      1    1.00 0.00   1   1     0  NaN      NaN 0.00
------------------------------------------------------------------------------------- 
group: t17
      var  n mean   sd median trimmed  mad min max range skew kurtosis   se
cond*   1 20  4.0 0.00      4    4.00 0.00   4   4     0  NaN      NaN 0.00
pre     2 20 10.0 1.34     10   10.00 1.48   8  12     4 0.25    -1.34 0.30
post    3 20 14.4 1.85     14   14.25 1.48  12  19     7 0.63    -0.27 0.41
gain    4 20  4.4 1.39      4    4.25 1.48   3   7     4 0.64    -1.12 0.31
train   5 20  1.0 0.00      1    1.00 0.00   1   1     0  NaN      NaN 0.00
------------------------------------------------------------------------------------- 
group: t19
      var  n  mean   sd median trimmed  mad min max range skew kurtosis   se
cond*   1 20  5.00 0.00    5.0    5.00 0.00   5   5     0  NaN      NaN 0.00
pre     2 20 10.15 1.27   10.0   10.19 1.48   8  12     4 0.03    -1.10 0.28
post    3 20 15.75 1.86   16.0   15.69 1.48  13  19     6 0.16    -1.03 0.42
gain    4 20  5.60 1.73    5.5    5.50 2.22   3   9     6 0.36    -0.76 0.39
train   5 20  1.00 0.00    1.0    1.00 0.00   1   1     0  NaN      NaN 0.00


# Create two subsets of data: One for the control group and another for the training groups
wm.c = subset(wm, wm$train == "0")
wm.t = subset(wm, wm$train == "1")

# Save summary statistics in tables to illustrate calculation of effect size
wm.c.out = describe(wm.c)
wm.c.out
      var  n  mean   sd median trimmed  mad min max range  skew kurtosis   se
cond*   1 40  1.00 0.00      1    1.00 0.00   1   1     0   NaN      NaN 0.00
pre     2 40  9.97 1.46     10    9.97 1.48   8  12     4  0.09    -1.41 0.23
post    3 40 11.95 2.06     12   12.00 2.22   7  17    10 -0.05    -0.18 0.33
gain    4 40  1.98 1.39      2    1.97 1.48  -1   5     6  0.10     0.03 0.22
train   5 40  0.00 0.00      0    0.00 0.00   0   0     0   NaN      NaN 0.00
wm.t.out = describe(wm.t)
wm.t.out
      var  n  mean   sd median trimmed  mad min max range skew kurtosis   se
cond*   1 80  3.50 1.13    3.5    3.50 1.48   2   5     3 0.00    -1.40 0.13
pre     2 80 10.03 1.37   10.0   10.03 1.48   8  12     4 0.10    -1.24 0.15
post    3 80 13.51 2.54   14.0   13.50 2.97   7  19    12 0.00    -0.24 0.28
gain    4 80  3.49 2.15    3.0    3.41 1.48  -1   9    10 0.34    -0.25 0.24
train   5 80  1.00 0.00    1.0    1.00 0.00   1   1     0  NaN      NaN 0.00

# Dependent t-tests

# First, compare pre and post scores in the control group
t.test(wm.c$post, wm.c$pre, paired = T)
	Paired t-test

data:  wm.c$post and wm.c$pre
t = 9.0089, df = 39, p-value = 4.511e-11
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 1.53157 2.41843
sample estimates:
mean of the differences 
                  1.975 

# Next, compare pre and post scores in the training groups
t.test(wm.t$post, wm.t$pre, paired = T)
	Paired t-test

data:  wm.t$post and wm.t$pre
t = 14.4924, df = 79, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 3.008511 3.966489
sample estimates:
mean of the differences 
                 3.4875 

# Cohen's d for dependent t-tests
# d = Mean of difference scores / Standard deviation of difference scores

d.c = (wm.c.out[4,3]) / (wm.c.out[4,4])
d.c
#or
cohensD(wm.c$post, wm.c$pre, method="paired")
[1] 1.42443
d.t = (wm.t.out[4,3]) / (wm.t.out[4,4])
d.t
#or
cohensD(wm.t$post, wm.t$pre, method="paired")
[1] 1.620297 
# Boxplot
long.wm <- melt(wm, id=c("cond", "train", "gain"))

ggplot(long.wm, aes(x=cond, y=value, color=variable)) + 
  geom_boxplot() +
  guides(fill=FALSE) 
  
# Independent t-test
# Compare the gain scores in the control and training groups 
t.test(wm$gain ~ wm$train, var.equal = T)
	Two Sample t-test

data:  wm$gain by wm$train
t = -4.0404, df = 118, p-value = 9.539e-05
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -2.2538109 -0.7711891
sample estimates:
mean in group 0 mean in group 1 
         1.9750          3.4875 
         
# Cohen's d for independent t-tests
# d = (M1 - M2) / Pooled Standard Deviation

pooled.sd = (79/118 * wm.t.out[4,4]) + (39/118 * wm.c.out[4,4])

d.ct = (wm.t.out[4,3] - wm.c.out[4,3]) / pooled.sd
d.ct
# or
cohensD(wm$gain ~ wm$train, method="pooled")
[1] 0.7824121
# Boxplot
ggplot(wm, aes(x=cond, y=gain, fill=cond)) + 
  geom_boxplot() +
  guides(fill=FALSE)
  
# To compare the gain scores across all groups, use ANOVA
# First, check the homogeneity of variance assumption
leveneTest(wm.t$gain, wm.t$cond, center="mean")
Levene's Test for Homogeneity of Variance (center = "mean")
      Df F value Pr(>F)
group  3  1.1269 0.3436
      76 
leveneTest(wm.t$gain, wm.t$cond)
Levene's Test for Homogeneity of Variance (center = median)
      Df F value Pr(>F)
group  3  1.3134 0.2763
      76 
aov.model = aov(wm.t$gain ~ wm.t$cond)
summary(aov.model)
            Df Sum Sq Mean Sq F value   Pr(>F)    
wm.t$cond    3  213.0   71.01   35.29 2.16e-14 ***
Residuals   76  152.9    2.01                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Save results in a table to illustrate calculation of effect size
aov.table = summary(aov.model)

# Effect size for ANOVA
ss = aov.table[[1]]$"Sum Sq"
eta.sq = ss[1] / (ss[1] + ss[2])
eta.sq
#or
etaSquared(aov.model, anova=T)
             eta.sq eta.sq.part       SS df      MS        F            p
wm.t$cond 0.5820896   0.5820896 213.0375  3 71.0125 35.28571 2.153833e-14
Residuals 0.4179104          NA 152.9500 76  2.0125       NA           NA
# Conduct post-hoc tests to evaluate all pairwise comparisons
TukeyHSD(aov.model)
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = wm.t$gain ~ wm.t$cond)

$`wm.t$cond`
        diff        lwr      upr     p adj
t12-t08 1.25 0.07159545 2.428405 0.0333212
t17-t08 3.05 1.87159545 4.228405 0.0000000
t19-t08 4.25 3.07159545 5.428405 0.0000000
t17-t12 1.80 0.62159545 2.978405 0.0007908
t19-t12 3.00 1.82159545 4.178405 0.0000000
t19-t17 1.20 0.02159545 2.378405 0.0443394