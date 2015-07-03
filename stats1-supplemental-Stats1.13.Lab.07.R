# Statistics One, 2013, Lab 7
# Lab goals
#   Conduct moderation and mediation analyses

# Segment 1
#   Moderation analysis
#     Example
#     An experimental research investigation of the effects of stereotype threat on intelligence testing 
#       Dependent variable (Y) is score on an intelligence test (IQ)
#       Independent variable (X) is the treatment condition (3 levels: control, explicit threat, implicit threat)
#       Moderator variable is score on a working memory task
#       Sample size of N = 150 (n = 50)

# Segment 2
#   Mediation analysis
#     Example
#       An experimental research investigation of the effects of stereotype threat on intelligence testing 
#         Dependent variable (Y) is score on an intelligence test (IQ)
#         Independent variable (X) is the treatment condition (2 levels: control, threat)
#         Mediator variable (M) is score on a working memory task
#         Sample size of N = 100 (n = 50)

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

# Segment 1

# Read data into a dataframe called MOD
MOD <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.Lab.07.txt", header = T)

# If you want to view the data
# View(MOD)
edit(MOD)
head(MOD)
  subject condition  IQ  WM WM.centered D1 D2
1       1   control 134  91       -8.08  0  0
2       2   control 121 145       45.92  0  0
3       3   control  86 118       18.92  0  0
4       4   control  74 105        5.92  0  0
5       5   control  80  96       -3.08  0  0
6       6   control 105 133       33.92  0  0
# Summary statistics
describeBy(MOD, MOD$condition) 
group: control
            var  n   mean    sd median trimmed   mad    min    max range skew kurtosis   se
subject       1 50  25.50 14.58  25.50   25.50 18.53   1.00  50.00    49 0.00    -1.27 2.06
condition*    2 50   1.00  0.00   1.00    1.00  0.00   1.00   1.00     0  NaN      NaN 0.00
IQ            3 50  97.88 20.93  99.50   97.47 25.20  46.00 141.00    95 0.04    -0.50 2.96
WM            4 50 102.18 18.79  99.50  100.55 20.02  71.00 159.00    88 0.73     0.41 2.66
WM.centered   5 50   3.10 18.79   0.42    1.47 20.02 -28.08  59.92    88 0.73     0.41 2.66
D1            6 50   0.00  0.00   0.00    0.00  0.00   0.00   0.00     0  NaN      NaN 0.00
D2            7 50   0.00  0.00   0.00    0.00  0.00   0.00   0.00     0  NaN      NaN 0.00
----------------------------------------------------------------------------------- 
group: threat1
            var  n   mean    sd median trimmed   mad    min    max range skew kurtosis   se
subject       1 50  75.50 14.58  75.50   75.50 18.53  51.00 100.00    49 0.00    -1.27 2.06
condition*    2 50   2.00  0.00   2.00    2.00  0.00   2.00   2.00     0  NaN      NaN 0.00
IQ            3 50  52.16 13.79  52.00   51.75 11.86  21.00  83.00    62 0.16    -0.25 1.95
WM            4 50 100.80 16.85  97.50   99.90 16.31  72.00 138.00    66 0.48    -0.56 2.38
WM.centered   5 50   1.72 16.85  -1.58    0.82 16.31 -27.08  38.92    66 0.48    -0.56 2.38
D1            6 50   1.00  0.00   1.00    1.00  0.00   1.00   1.00     0  NaN      NaN 0.00
D2            7 50   0.00  0.00   0.00    0.00  0.00   0.00   0.00     0  NaN      NaN 0.00
----------------------------------------------------------------------------------- 
group: threat2
            var  n   mean    sd median trimmed   mad    min    max range skew kurtosis   se
subject       1 50 125.50 14.58 125.50  125.50 18.53 101.00 150.00    49 0.00    -1.27 2.06
condition*    2 50   3.00  0.00   3.00    3.00  0.00   3.00   3.00     0  NaN      NaN 0.00
IQ            3 50  48.02 12.45  47.00   47.40 14.83  28.00  79.00    51 0.38    -0.56 1.76
WM            4 50  94.26 18.77  92.00   93.97 17.79  55.00 133.00    78 0.16    -0.71 2.65
WM.centered   5 50  -4.82 18.77  -7.08   -5.10 17.79 -44.08  33.92    78 0.16    -0.71 2.65
D1            6 50   0.00  0.00   0.00    0.00  0.00   0.00   0.00     0  NaN      NaN 0.00
D2            7 50   1.00  0.00   1.00    1.00  0.00   1.00   1.00     0  NaN      NaN 0.00
# First, is there an effect of stereotype threat?
model0 <- lm(MOD$IQ ~ MOD$D1 + MOD$D2)
summary(model0)
Call:
lm(formula = MOD$IQ ~ MOD$D1 + MOD$D2)

Residuals:
    Min      1Q  Median      3Q     Max 
-51.880 -11.125  -0.450   8.765  43.120 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   97.880      2.286   42.83   <2e-16 ***
MOD$D1       -45.720      3.232  -14.14   <2e-16 ***
MOD$D2       -49.860      3.232  -15.43   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 16.16 on 147 degrees of freedom
Multiple R-squared:  0.666,	Adjusted R-squared:  0.6615 
F-statistic: 146.6 on 2 and 147 DF,  p-value: < 2.2e-16
confint(model0)
                2.5 %    97.5 %
(Intercept)  93.36331 102.39669
MOD$D1      -52.10757 -39.33243
MOD$D2      -56.24757 -43.47243

# We could also use the aov function (for analysis of variance) followed by the TukeyHSD function (Tukey's test of pairwise comparisons, which adjusts the p value to prevent infaltion of Type I error rate)
model0a <- aov(MOD$IQ ~ MOD$condition)
summary(model0a)
               Df Sum Sq Mean Sq F value Pr(>F)    
MOD$condition   2  76558   38279   146.6 <2e-16 ***
Residuals     147  38393     261                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
TukeyHSD(model0a)
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = MOD$IQ ~ MOD$condition)

$`MOD$condition`
                  diff       lwr        upr     p adj
threat1-control -45.72 -53.37284 -38.067159 0.0000000
threat2-control -49.86 -57.51284 -42.207159 0.0000000
threat2-threat1  -4.14 -11.79284   3.512841 0.4082157

# Moderation analysis (uncentered): model1 tests for "first-order effects"; model2 tests for moderation
model1 <- lm(MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2)
summary(model1)
Call:
lm(formula = MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2)

Residuals:
    Min      1Q  Median      3Q     Max 
-47.339  -7.294   0.744   7.608  42.424 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  59.78635    7.14360   8.369 4.30e-14 ***
MOD$WM        0.37281    0.06688   5.575 1.16e-07 ***
MOD$D1      -45.20552    2.94638 -15.343  < 2e-16 ***
MOD$D2      -46.90735    2.99218 -15.677  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 14.72 on 146 degrees of freedom
Multiple R-squared:  0.7246,	Adjusted R-squared:  0.719 
F-statistic: 128.1 on 3 and 146 DF,  p-value: < 2.2e-16

ggplot(MOD, aes(x = WM, y = IQ)) + geom_smooth(method = "lm") + 
  geom_point() 

# Create new predictor variables
MOD$WM.D1 <- (MOD$WM * MOD$D1)
MOD$WM.D2 <- (MOD$WM * MOD$D2)

model2 <- lm(MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2 + MOD$WM.D1 + MOD$WM.D2)
summary(model2)
Call:
lm(formula = MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2 + MOD$WM.D1 + 
    MOD$WM.D2)

Residuals:
    Min      1Q  Median      3Q     Max 
-50.414  -7.181   0.420   8.196  40.864 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  85.5851    11.3576   7.535 4.95e-12 ***
MOD$WM        0.1203     0.1094   1.100  0.27303    
MOD$D1      -93.0952    16.8573  -5.523 1.52e-07 ***
MOD$D2      -79.8970    15.4772  -5.162 7.96e-07 ***
MOD$WM.D1     0.4716     0.1638   2.880  0.00459 ** 
MOD$WM.D2     0.3288     0.1547   2.125  0.03529 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 14.38 on 144 degrees of freedom
Multiple R-squared:  0.7409,	Adjusted R-squared:  0.7319 
F-statistic: 82.35 on 5 and 144 DF,  p-value: < 2.2e-16

anova(model1, model2)
Analysis of Variance Table

Model 1: MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2
Model 2: MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2 + MOD$WM.D1 + MOD$WM.D2
  Res.Df   RSS Df Sum of Sq      F  Pr(>F)  
1    146 31655                              
2    144 29784  2    1871.3 4.5238 0.01243 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Scatter plot by group and all in one
WM.control <- MOD$WM[1:50]
IQ.control <- MOD$IQ[1:50]
WM.threat1 <- MOD$WM[51:100]
IQ.threat1 <- MOD$IQ[51:100]
WM.threat2 <- MOD$WM[101:150]
IQ.threat2 <- MOD$IQ[101:150]

ggplot(MOD, aes(x = WM.control, y = IQ.control)) + geom_smooth(method = "lm") + geom_point()
ggplot(MOD, aes(x = WM.threat1, y = IQ.threat1)) + geom_smooth(method = "lm") + geom_point()
ggplot(MOD, aes(x = WM.threat2, y = IQ.threat2)) + geom_smooth(method = "lm") + geom_point()

color <- c("red","green","blue")
ggplot(MOD, aes(x = WM, y = IQ)) + stat_smooth(method="lm", se=F) +
  geom_point(aes(color=condition))
ggplot(MOD, aes(x = WM, y = IQ)) + 
  geom_smooth(aes(group=condition), method="lm", se=T, color="black", fullrange=T) +
  geom_point(aes(color=condition))
  
# Segment 2

# Read data into a dataframe called MED
MED <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.Lab.07b.txt", header = T)

# If you want to view the data
# View(MED)
edit(MED)
head(MED)
  subject condition  IQ WM
1       1   control  73 37
2       2   control 128 77
3       3   control  83 32
4       4   control  83 33
5       5   control  64 53
6       6   control  95 46

# Summary statistics
describeBy(MED, MED$condition) 
group: control
           var  n  mean    sd median trimmed   mad min max range  skew kurtosis   se
subject      1 50 25.50 14.58   25.5   25.50 18.53   1  50    49  0.00    -1.27 2.06
condition*   2 50  1.00  0.00    1.0    1.00  0.00   1   1     0   NaN      NaN 0.00
IQ           3 50 97.32 15.55   95.0   96.55 15.57  64 137    73  0.43     0.11 2.20
WM           4 50 54.92 13.87   56.0   55.27 14.08  23  81    58 -0.20    -0.64 1.96
----------------------------------------------------------------------------------- 
group: threat
           var  n  mean    sd median trimmed   mad min max range  skew kurtosis   se
subject      1 50 75.50 14.58   75.5   75.50 18.53  51 100    49  0.00    -1.27 2.06
condition*   2 50  2.00  0.00    2.0    2.00  0.00   2   2     0   NaN      NaN 0.00
IQ           3 50 86.32 13.67   85.0   86.50 13.34  51 117    66 -0.08    -0.22 1.93
WM           4 50 43.50 12.99   43.0   43.45 13.34  15  74    59  0.06    -0.58 1.84
# The function sobel in the multilevel package executes the entire mediation analysis in one step but first we will do it with 3 lm models
model.YX <- lm(MED$IQ ~ MED$condition)
model.YXM <- lm(MED$IQ ~ MED$condition + MED$WM)
model.MX <- lm(MED$WM ~ MED$condition)

summary(model.YX)
Call:
lm(formula = MED$IQ ~ MED$condition)

Residuals:
   Min     1Q Median     3Q    Max 
-35.32  -9.57  -1.82  10.68  39.68 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)           97.320      2.071  46.999  < 2e-16 ***
MED$conditionthreat  -11.000      2.928  -3.756 0.000293 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 14.64 on 98 degrees of freedom
Multiple R-squared:  0.1259,	Adjusted R-squared:  0.1169 
F-statistic: 14.11 on 1 and 98 DF,  p-value: 0.0002928
summary(model.YXM)
Call:
lm(formula = MED$IQ ~ MED$condition + MED$WM)

Residuals:
    Min      1Q  Median      3Q     Max 
-31.875  -7.897   0.932   6.993  27.581 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)          55.9977     4.6440  12.058  < 2e-16 ***
MED$conditionthreat  -2.4075     2.3164  -1.039    0.301    
MED$WM                0.7524     0.0800   9.406 2.58e-15 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 10.64 on 97 degrees of freedom
Multiple R-squared:  0.5428,	Adjusted R-squared:  0.5334 
F-statistic: 57.59 on 2 and 97 DF,  p-value: < 2.2e-16
summary(model.MX)
Call:
lm(formula = MED$WM ~ MED$condition)

Residuals:
   Min     1Q Median     3Q    Max 
-31.92  -7.75  -0.50  10.19  30.50 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)           54.920      1.901  28.895  < 2e-16 ***
MED$conditionthreat  -11.420      2.688  -4.249 4.91e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 13.44 on 98 degrees of freedom
Multiple R-squared:  0.1555,	Adjusted R-squared:  0.1469 
F-statistic: 18.05 on 1 and 98 DF,  p-value: 4.906e-05

# Compare the results to the output of the sobel function
model.ALL <- sobel(MED$condition, MED$WM, MED$IQ) 
model.ALL

$`Mod1: Y~X`
            Estimate Std. Error   t value     Pr(>|t|)
(Intercept)    97.32   2.070678 46.999106 4.965625e-69
predthreat    -11.00   2.928380 -3.756342 2.927777e-04

$`Mod2: Y~X+M`
             Estimate Std. Error   t value     Pr(>|t|)
(Intercept) 55.997696 4.64403367 12.057987 5.303590e-21
predthreat  -2.407489 2.31641713 -1.039316 3.012416e-01
med          0.752409 0.07999527  9.405669 2.576515e-15

$`Mod3: M~X`
            Estimate Std. Error   t value     Pr(>|t|)
(Intercept)    54.92   1.900708 28.894501 9.487423e-50
predthreat    -11.42   2.688007 -4.248501 4.906391e-05

$Indirect.Effect
[1] -8.592511

$SE
[1] 2.219233

$z.value
[1] -3.871839

$N
[1] 100
