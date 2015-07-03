

HW 11

Recent theories in the field of cognitive enhancement suggest that peoples belief about whether or not cognitive abilities can be improved influences the outcome of a training program. In this weeks assignment, we take a look at a dataset including two different kinds of feedback given to participants in a cognitive training program, either fixed (cognitive abilities are innate and cannot be improved) or malleable (cognitive abilities are largely driven by experiences). DVs include verbal, spatial, and intelligence measures,provided before and after training.

setwd("/Users/karenyang/Desktop/StatisticsOne12weeks")

install.packages("psych")
install.packages("car")

library(psych)
library(car)
library(lsr)
library(ggplot2)
library(reshape)

data1 <-read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.HW.11.txt", header = T)
dim(data1)
[1] 100   8
str(data1)
'data.frame':	100 obs. of  8 variables:
 $ id          : int  1 2 3 4 5 6 7 8 9 10 ...
 $ cond        : Factor w/ 2 levels "fixed","malleable": 1 1 1 1 1 1 1 1 1 1 ...
 $ verbal.pre  : int  21 15 18 17 15 17 15 17 20 17 ...
 $ verbal.post : int  25 21 23 18 16 16 16 28 22 14 ...
 $ spatial.pre : int  7 13 8 13 26 2 9 16 12 6 ...
 $ spatial.post: int  19 16 3 15 32 2 10 20 4 22 ...
 $ intel.pre   : int  1 11 2 11 15 4 11 10 11 6 ...
 $ intel.post  : int  4 14 3 14 20 7 11 13 12 7 ...
 

describeBy(data1, data1$cond)
group: fixed
             var  n  mean    sd median trimmed   mad min max range skew kurtosis   se
id             1 50 25.50 14.58   25.5   25.50 18.53   1  50    49 0.00    -1.27 2.06
cond*          2 50  1.00  0.00    1.0    1.00  0.00   1   1     0  NaN      NaN 0.00
verbal.pre     3 50 18.34  2.68   18.0   18.23  2.97  12  26    14 0.40     0.26 0.38
verbal.post    4 50 21.82  4.65   22.0   21.68  4.45  10  33    23 0.20    -0.11 0.66
spatial.pre    5 50 13.28  9.50   11.5   12.47  8.90   0  53    53 1.44     3.91 1.34
spatial.post   6 50 17.64 11.65   17.5   16.48 11.12   2  63    61 1.29     2.81 1.65
intel.pre      7 50 10.74  7.11   11.0   10.00  6.67   1  37    36 1.11     1.88 1.00
intel.post     8 50 13.74  7.46   13.0   12.97  4.45   2  38    36 1.06     1.23 1.05
------------------------------------------------------------------------------ 
group: malleable
             var  n  mean    sd median trimmed   mad min max range  skew kurtosis   se
id             1 50 75.50 14.58   75.5   75.50 18.53  51 100    49  0.00    -1.27 2.06
cond*          2 50  2.00  0.00    2.0    2.00  0.00   2   2     0   NaN      NaN 0.00
verbal.pre     3 50 17.90  2.88   18.0   17.90  2.97  11  25    14  0.04     0.00 0.41
verbal.post    4 50 24.82  3.75   25.0   24.77  4.45  14  33    19 -0.14    -0.08 0.53
spatial.pre    5 50 15.70 10.05   15.5   15.28 12.60   0  36    36  0.22    -1.03 1.42
spatial.post   6 50 19.62 12.70   17.5   19.20 16.31   0  43    43  0.26    -1.19 1.80
intel.pre      7 50 11.56  7.48   11.5   11.30  9.64   0  26    26  0.18    -1.14 1.06
intel.post     8 50 13.76  8.28   14.0   13.57 11.86   0  30    30  0.14    -1.16 1.17

Q1.  Using a t-test, compare verbal scores before and after training in the fixed condition. Is the difference pre-test to post-test significant? Yes
Yes or No

data1.fixed <- subset(data1, data1$cond=="fixed")
t.test(data1.fixed$verbal.pre, data1.fixed$verbal.post, paired = T)
	Paired t-test

data:  data1.fixed$verbal.pre and data1.fixed$verbal.post
t = -7.0766, df = 49, p-value = 5.048e-09
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -4.46823 -2.49177
sample estimates:
mean of the differences 
                  -3.48 
                  
Q2.  What are the degrees of freedom for the comparison between pre-test and post-test for the spatial scores? 49
49, 50, 94, 95

t.test(data1.fixed$spatial.pre, data1.fixed$spatial.post, paired = T)
	Paired t-test

data:  data1.fixed$spatial.pre and data1.fixed$spatial.post
t = -3.8701, df = 49, p-value = 0.0003218
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -6.623956 -2.096044
sample estimates:
mean of the differences 
                  -4.36 

Q3. Run a Wilcoxon test for the same comparison (pre-test to post-test on spatial scores, fixed condition). 
Which of the two tests gives the highest p-value for the comparison? Wilcoxon
t-test
Wilcoxon

wilcox.test(data1.fixed$spatial.pre, data1.fixed$spatial.post, paired = T)

	Wilcoxon signed rank test with continuity correction

data:  data1.fixed$spatial.pre and data1.fixed$spatial.post
V = 230.5, p-value = 0.0004211
alternative hypothesis: true location shift is not equal to 0


Q4. What is the effect size (Cohens d) for the difference between pre-test and post-test spatial scores for the fixed condition? (round to two decimal places) 0.55
cohensD(data1.fixed$spatial.pre, data1.fixed$spatial.post, method="paired")
[1] 0.5473156


Q5.  Which of the three tasks shows the largest improvements from pre-test to post-test, in the fixed condition? Verbal
Verbal, Spatial, Intel

leveneTest(data1.fixed$verbal.pre, data1.fixed$verbal.post, center="mean")
Levenes Test for Homogeneity of Variance (center = "mean")
      Df F value Pr(>F)
group 18  1.6757 0.1006
      31               
leveneTest(data1.fixed$spatial.pre, data1.fixed$spatial.post, center="mean")
Levenes Test for Homogeneity of Variance (center = "mean")
      Df F value  Pr(>F)  
group 27  1.7915 0.08322 .
      22                  
leveneTest(data1.fixed$intel.pre, data1.fixed$intel.post, center="mean")
Levenes Test for Homogeneity of Variance (center = "mean")
      Df F value Pr(>F)
group 22  0.7878 0.7137
      27 
      
cohensD(data1.fixed$verbal.pre, data1.fixed$verbal.post, method="paired")   [1] 1.000784     
cohensD(data1.fixed$spatial.pre, data1.fixed$spatial.post, method="paired")  [1] 0.5473156 
cohensD(data1.fixed$intel.pre, data1.fixed$intel.post, method="paired")          [1] 0.9506253

Q6. Which of the three tasks shows the largest improvements from pre-test to post-test, in the malleable condition? Verbal
Verbal, Spatial, Intel

leveneTest(data1.malleable$verbal.pre, data1.malleable$verbal.post, center="mean")
Levenes Test for Homogeneity of Variance (center = "mean")
      Df F value Pr(>F)
group 13  0.9403 0.5241
      36 
leveneTest(data1.malleable$spatial.pre, data1.malleable$spatial.post, center="mean")
Levenes Test for Homogeneity of Variance (center = "mean")
      Df F value    Pr(>F)    
group 31   23.47 2.102e-09 ***
      18 
leveneTest(data1.malleable$intel.pre, data1.malleable$intel.post, center="mean")
Levenes Test for Homogeneity of Variance (center = "mean")
      Df F value    Pr(>F)    
group 27  5.5717 5.737e-05 ***
      22
      
cohensD(data1.malleable$verbal.pre, data1.malleable$verbal.post, method="paired") [1] 2.254414      
cohensD(data1.malleable$spatial.pre, data1.malleable$spatial.post, method="paired") [1] 0.4485912
cohensD(data1.malleable$intel.pre, data1.malleable$intel.post, method="paired")   [1] 0.8279091 
      
Q7. Conduct Mann-Whitney comparisons between all tasks at pre-test. Which task(s) differ significantly from the other two in pre-test scores? All
Verbal, Spatial, Intel, ALL

wilcox.test(data1$spatial.pre, data1$verbal.pre, paired=F)
	Wilcoxon rank sum test with continuity correction

data:  data1$spatial.pre and data1$verbal.pre
W = 3554, p-value = 0.0004013
alternative hypothesis: true location shift is not equal to 0

wilcox.test(data1$spatial.pre, data1$intel.pre, paired=F)
	Wilcoxon rank sum test with continuity correction

data:  data1$spatial.pre and data1$intel.pre
W = 5949, p-value = 0.02036
alternative hypothesis: true location shift is not equal to 0

wilcox.test(data1$verbal.pre, data1$intel.pre, paired=F)
	Wilcoxon rank sum test with continuity correction

data:  data1$verbal.pre and data1$intel.pre
W = 8126.5, p-value = 1.968e-14
alternative hypothesis: true location shift is not equal to 0

Q8. Which feedback condition led to the largest improvements overall? malleable
fixed, malleable

pre.m <-  data1.malleable$verbal.pre + data1.malleable$spatial.pre + data1.malleable$intel.pre
post.m <- data1.malleable$verbal.post + data1.malleable$spatial.post + data1.malleable$intel.post
cohensD(pre.m, post.m, method="paired")  [1] 1.379879

pre.f <-  data1.fixed$verbal.pre + data1.fixed$spatial.pre + data1.fixed$intel.pre
post.f = data1.fixed$verbal.post + data1.fixed$spatial.post + data1.fixed$intel.post
cohensD(pre.f, post.f, method="paired")  [1] 1.062041

Q9.  Which task is largely driving this effect? Verbal
Verbal, Spatial, Intel, None in particular

cohensD(data1.malleable$verbal.pre, data1.malleable$verbal.post, method="paired")  [1] 2.254414
cohensD(data1.malleable$spatial.pre, data1.malleable$spatial.post, method="paired") [1] 0.4485912
cohensD(data1.malleable$intel.pre, data1.malleable$intel.post, method="paired")   [1] 0.8279091
cohensD(data1.fixed$verbal.pre, data1.fixed$verbal.post, method="paired")   [1] 1.000784 
cohensD(data1.fixed$spatial.pre, data1.fixed$spatial.post, method="paired")   [1] 0.5473156
cohensD(data1.fixed$intel.pre, data1.fixed$intel.post, method="paired")    [1] 0.9506253


Q10.  Based on the present data, are you convinced that malleable feedback is beneficial to performance when engaging in a cognitive training program? It depends on ability being trained
Yes, No, It depends on the ability being trained




