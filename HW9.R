HW 9

This week, we turn to a classic study on helping behavior by Darley and Batson (1973). Simulated data are provided here. The study demonstrates that people’s likelihood of helping a person in distress depends largely on their level of haste—whether they were running early, on time, or late for an appointment—when they encounter him, rather than on whether they have been asked to reflect on a pro-helping message (the parable of the Good Samaritan) as opposed to a neutral message (occupational effectiveness). In this dataset, independent variables include Prime (1 = parable of the Good Samaritan; 2 = occupational effectiveness) and Haste (1 = early, 2 = on time, 3 = late). On their way to a nearby location, participants encounter a moaning individual in distress. The Helping variable provides a measure of how much they help, ranging from 0 to 6 with higher scores indicating greater helping.



# If necessary, install packages
install.packages("psych")
install.packages("car")
install.packages("lsr")

library(psych)
library(car)
library(lsr)

data1 <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.HW.09.txt", header = T)
str(data1)
'data.frame':	42 obs. of  4 variables:
 $ PID    : int  1 2 3 4 5 6 7 8 9 10 ...
 $ Prime  : int  1 1 1 1 1 1 1 1 1 1 ...
 $ Haste  : int  1 1 1 1 1 1 1 2 2 2 ...
 $ Helping: num  4.38 4.58 2.7 3.65 3.84 4.66 2.79 2.39 2.89 1.42 ...
 
 Question 1. What is the class of Haste and Prime in R? integer
 class(data1$Haste)
[1] "integer"
class(data1$Prime)
[1] "integer"
 
 Question 2. After converting Haste and Prime to factors, run an ANOVA with both Haste and Prime as independent variables. Is the effect of Haste significant? Yes
 Yes or No
 data1$Prime <- as.factor(data1$Prime)
 data1$Haste <- as.factor(data1$Haste)
 class(data1$Prime)
[1] "factor"
class(data1$Haste)
[1] "factor"
#Run ANOVA model
model1 <- aov(data1$Helping ~ data1$Haste * data1$Prime) 
summary(model1)
                                Df Sum Sq Mean Sq F value   Pr(>F)    
data1$Haste              2 20.237  10.118  11.893 0.000108 ***        #This is the main effect for Haste
data1$Prime              1 10.222  10.222  12.015 0.001383 **         #This is the main effect for Prime
data1$Haste:data1$Prime  2  6.913   3.457   4.063 0.025638 *    #This is the interaction effect
Residuals               36 30.627   0.851                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

 
 Question 3.  Is the effect of Prime significant? Yes
 
 Question 4.  Is the interaction significant? Yes
 Yes or No
 
 Question 5.  Save the ANOVA summary in a table and run Tukeys pairwise comparison on all group means. Do each level of Haste significantly differ from one another? No
 Yes or No
 
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = data1$Helping ~ data1$Haste * data1$Prime)

$`data1$Haste`
          diff             lwr              upr            p adj
2-1 -0.9000000 -1.752129 -0.04787061 0.0365564   #Reject the null hypothesis
3-1 -1.6992857 -2.551415 -0.84715632 0.0000643   #Reject the null hypothesis
3-2 -0.7992857 -1.651415  0.05284368 0.0697038   #Adjusted p-value is 0.07 so cannot reject null hypothesis of no difference

$`data1$Prime`
          diff       lwr        upr    p adj
2-1 -0.9866667 -1.563957 -0.4093767 0.001383

$`data1$Haste:data1$Prime`
                 diff       lwr        upr     p adj
2:1-1:1 -1.800000e+00 -3.283294 -0.3167056 0.0098729
3:1-1:1 -2.514286e+00 -3.997580 -1.0309913 0.0001518
1:2-1:1 -2.130000e+00 -3.613294 -0.6467056 0.0015238
2:2-1:1 -2.130000e+00 -3.613294 -0.6467056 0.0015238
3:2-1:1 -3.014286e+00 -4.497580 -1.5309913 0.0000070
3:1-2:1 -7.142857e-01 -2.197580  0.7690087 0.6977022
1:2-2:1 -3.300000e-01 -1.813294  1.1532944 0.9842223
2:2-2:1 -3.300000e-01 -1.813294  1.1532944 0.9842223
3:2-2:1 -1.214286e+00 -2.697580  0.2690087 0.1624280
1:2-3:1  3.842857e-01 -1.099009  1.8675801 0.9692787
2:2-3:1  3.842857e-01 -1.099009  1.8675801 0.9692787
3:2-3:1 -5.000000e-01 -1.983294  0.9832944 0.9101607
2:2-1:2 -4.440892e-16 -1.483294  1.4832944 1.0000000
3:2-1:2 -8.842857e-01 -2.367580  0.5990087 0.4823830
3:2-2:2 -8.842857e-01 -2.367580  0.5990087 0.4823830
 
 Question 6.  What is the partial eta-squared value for the effect of Haste? (round to 2 decimal places).  0.40
 etaSquared(model1, anova = T)
                                  			eta.sq    eta.sq.part   SS 		df         MS         F            p
data1$Haste             		0.2976029   0.3978615 20.236671  2 10.1183357 11.893452 0.0001082769
data1$Prime             		0.1503240   0.2502366 10.221867  1 10.2218667 12.015146 0.0013829622
data1$Haste:data1$Prime 	0.1016698   0.1841599  6.913433   2  3.4567167  4.063148 0.0256383754
Residuals               			0.4504034          NA 30.626943 	36  0.8507484        NA           NA
 
 Question 7.   What is the partial eta-squared value for the interaction? (round to 2 decimal places).   0.18
 
 Question 8.  Let us now run simple effects of Prime at each level of Haste. At which level of Haste is the effect of Prime significant? Early
 Late, Early, All of the above, On Time
 model2 <- subset(data1, data1$Haste=="1")
  model3 <- subset(data1, data1$Haste=="2") 
  model4 <- subset(data1, data1$Haste=="3")
  
  aov.model2 <- aov(model2$Helping ~ model2$Prime)   #Haste level is 1== early
  summary(aov.model2)
                         Df Sum Sq Mean Sq F value  Pr(>F)   
model2$Prime  1  15.88  15.879   17.54 0.00126 **    #statistically significant here
Residuals        12  10.87   0.906                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  aov.model3 <- aov(model3$Helping ~ model3$Prime)   #Haste level is 2==on time
  summary(aov.model3)
                        Df Sum Sq Mean Sq F value Pr(>F)
model3$Prime  1  0.381  0.3812    0.38  0.549
Residuals        12 12.048  1.0040 

  aov.model4 <-  aov(model4$Helping ~ model4$Prime)  #Haste level is 3== late
  summary(aov.model4)
                        Df Sum Sq Mean Sq F value Pr(>F)
model4$Prime  1  0.875  0.8750   1.361  0.266
Residuals         12  7.713  0.6427 
  
 Question 9.  What is the partial eta-squared value for the effect of Prime when people were early? (round to 2 decimal places).  0.59
 etaSquared(aov.model2, anova = T)
                                  eta.sq 	    eta.sq.part       SS          df         MS        F           p
model2$Prime 	0.5937119   0.5937119     15.87915  1 15.8791500 17.53569 0.001259446
Residuals    		0.4062881          NA         10.86640 12  0.9055333       NA          NA
 
 
 Question 10.  Which one of the following statements best illustrates the main finding of the study?    People are more likely to help others after being primed to do so if they are early
 People are less likely to help others if they are early
 People are more likely to be primed to help others if they are early
 People are more likely to help others after being primed to do so if they are early
 All of the above
 
 
 