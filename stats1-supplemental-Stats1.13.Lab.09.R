# Statistics One, 2013, Lab 9

# Lab Goals
#    Conduct a between groups factorial ANOVA
#    Example
#    A randomized controlled experiment designed to investigate teh effects of talking on a cell phone while driving
#      DV = Number of driving errors
#      Two IVs
#         (A) Conversation difficulty (3 levels): Control, Easy, Difficult
#         (B) Driving difficulty (2 levels): Easy, Difficult

# If necessary, install packages
install.packages("psych")
install.packages("car")
install.packages("lsr")

library(psych)
library(car)
library(lsr)

# Read data into a dataframe called AB
AB <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.Lab.09.txt", header = T)

# Let's look at the data 
edit(AB)
str(AB)
str(AB)
'data.frame':	120 obs. of  4 variables:
 $ subject     : int  1 2 3 4 5 6 7 8 9 10 ...
 $ conversation: Factor w/ 3 levels "High demand",..: 3 3 3 3 3 3 3 3 3 3 ...
 $ driving     : Factor w/ 2 levels "Difficult","Easy": 2 2 2 2 2 2 2 2 2 2 ...
 $ errors      : int  20 19 31 27 31 17 23 26 11 15 ...
 levels(AB$conversation)  
[1] "High demand" "Low demand"  "None" 
levels(AB$driving)
[1] "Difficult" "Easy" 

# Test the homogeneity of variance assumption
leveneTest(AB$errors ~ AB$driving * AB$conversation)
Levenes Test for Homogeneity of Variance (center = median)
       Df F value Pr(>F)
group   5  0.5206 0.7602
      114 
      
# Conduct the factorial ANOVA
AB.model <- aov(AB$errors ~ AB$driving * AB$conversation)
summary(AB.model)
                            Df Sum Sq Mean Sq F value   Pr(>F)    
AB$driving                   1   5782    5782   94.64  < 2e-16 ***
AB$conversation              2   4416    2208   36.14 6.98e-13 ***
AB$driving:AB$conversation   2   1639     820   13.41 5.86e-06 ***
Residuals                  114   6965      61                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Conduct simple effects analysis (of Conversation at each level of Driving)
AB1 <- subset(AB, AB$driving == "Easy")
head(AB1)
  subject conversation driving errors
1       1         None    Easy     20
2       2         None    Easy     19
3       3         None    Easy     31
4       4         None    Easy     27
5       5         None    Easy     31
6       6         None    Easy     17
AB2 <- subset(AB, AB$driving == "Difficult")
head(AB2)
   subject conversation   driving errors
21      21         None Difficult     10
22      22         None Difficult     23
23      23         None Difficult     31
24      24         None Difficult     32
25      25         None Difficult     24
26      26         None Difficult     26

aov.AB1 <- aov(AB1$errors ~ AB1$conversation)
summary(aov.AB1)
                 Df Sum Sq Mean Sq F value Pr(>F)  
AB1$conversation  2  504.7   252.3   4.928 0.0106 *
Residuals        57 2918.5    51.2                 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

aov.AB2 <- aov(AB2$errors ~ AB2$conversation)
summary(aov.AB2)
                 Df Sum Sq Mean Sq F value   Pr(>F)    
AB2$conversation  2   5551    2776   39.09 2.05e-11 ***
Residuals        57   4047      71                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Both simple effects are significant, so why is there an interaction? Let's look at effect sizes:
etaSquared(aov.AB1, anova = T)
                   eta.sq eta.sq.part      SS df        MS        F          p
AB1$conversation 0.147433    0.147433  504.70  2 252.35000 4.928458 0.01061116
Residuals        0.852567          NA 2918.55 57  51.20263       NA         NA

etaSquared(aov.AB2, anova = T)
                    eta.sq eta.sq.part       SS df         MS        F            p
AB2$conversation 0.5783571   0.5783571 5551.033  2 2775.51667 39.09275 2.046097e-11
Residuals        0.4216429          NA 4046.900 57   70.99825       NA           NA

# Finally, let's look at pairwise comparisons for the simple effects
TukeyHSD(aov.AB1)
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = AB1$errors ~ AB1$conversation)

$`AB1$conversation`
                        diff        lwr        upr     p adj
Low demand-High demand -6.05 -11.495243 -0.6047574 0.0260458
None-High demand       -6.25 -11.695243 -0.8047574 0.0207614
None-Low demand        -0.20  -5.645243  5.2452426 0.9957026

TukeyHSD(aov.AB2)
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = AB2$errors ~ AB2$conversation)

$`AB2$conversation`
                         diff       lwr        upr     p adj
Low demand-High demand  -9.75 -16.16202  -3.337979 0.0015849
None-High demand       -23.45 -29.86202 -17.037979 0.0000000
None-Low demand        -13.70 -20.11202  -7.287979 0.0000103








