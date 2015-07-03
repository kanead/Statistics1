#HW3

setwd("/Users/karenyang/Desktop/StatisticsOne12weeks")
getwd()

#install packages
install.packages("psych")
install.packages("gclus")
install.packages("rgl")

#load libraries
library(psych)
library(gclus)
library(rgl)

#Read data
cogtrain <- read.table("Stats1.13.HW.03.txt", header = T)
View(cogtrain)
names(cogtrain)

#Summary statistics
describe(cogtrain)
        var   n   mean    sd median trimmed   mad min max range  skew kurtosis   se
id        1 200 100.50 57.88  100.5  100.50 74.13   1 200   199  0.00    -1.22 4.09
cond*     2 200   1.50  0.50    1.5    1.50  0.74   1   2     1  0.00    -2.01 0.04
S1.pre    3 200  18.16  2.80   18.0   18.13  2.97  11  26    15  0.08    -0.26 0.20
S2.pre    4 200  26.91  3.22   27.0   26.78  2.97  19  35    16  0.33    -0.36 0.23
S1.post   5 200  23.57  4.32   23.0   23.60  4.45  10  34    24 -0.09    -0.12 0.31
S2.post   6 200  32.11  4.82   32.0   32.08  4.45  22  44    22  0.07    -0.47 0.34
V1.pre    7 200  14.17  9.19   13.0   13.66 10.38   0  53    53  0.64     0.54 0.65
V2.pre    8 200  11.03  6.79   11.0   10.64  7.41   0  37    37  0.53    -0.05 0.48
V1.post   9 200  17.85 11.78   17.0   16.99 13.34   0  63    63  0.63     0.07 0.83
V2.post  10 200  13.47  7.59   13.0   13.08  8.90   0  38    38  0.45    -0.29 0.54

describeBy(cogtrain, cogtrain$cond)

#Q1.What is the correlation between S1 and S2 pre-training?
cor(cogtrain$S1.pre, cogtrain$S2.pre)  #0.49

#Q2. What is the correlation between V1 and V2 pre-training?
cor(cogtrain$V1.pre, cogtrain$V2.pre)  #0.90

#Q3.  With respect to the measurement of two distinct constructs, spatial reasoning and verbal reasoning, the #pattern of correlations pre-training reveals:
cogtrain$S.pre <- (cogtrain$S1.pre + cogtrain$S2.pre)/2
cogtrain$V.pre <- (cogtrain$V1.pre + cogtrain$V2.pre) / 2 
cor(cogtrain$S.pre, cogtrain$V.pre)   #0.1186354  Both convergent and divergent

#Q4. Correlations from the control group could be used to estimate test/retest reliability. If so, which test is most #reliable?  V2
#correlation of baseline measures
cor(cogtrain[3:10])

cor(cogtrain$S1.pre, cogtrain$S1.post) #0.57
cor(cogtrain$S2.pre, cogtrain$S2.post) #0.60
cor(cogtrain$V1.pre, cogtrain$V1.post) #0.73
cor(cogtrain$V2.pre, cogtrain$V2.post)  #0.92

#Q5.  Does there appear to be a correlation between spatial reasoning before training and the amount of improvement in spatial reasoning?
cogtrain$S.pre = (cogtrain$S1.pre + cogtrain$S2.pre) / 2 
cogtrain$V.pre = (cogtrain$V1.pre + cogtrain$V2.pre) / 2
cogtrain$S.post = (cogtrain$S1.post + cogtrain$S2.post) / 2
cogtrain$V.post = (cogtrain$V1.post + cogtrain$V2.post) / 2 
cogtrain$Sgain = cogtrain$S.post - cogtrain$S.pre
cogtrain$Vgain = cogtrain$V.post - cogtrain$V.pre
cor(cogtrain$S.pre, cogtrain$Sgain)  #-0.09280867 NO

#Q6.  Does there appear to be a correlation between verbal reasoning before training and the amount of #improvement in verbal reasoning?
cor(cogtrain$V.pre, cogtrain$Vgain)  #-0.05822132 NO

#Q7.  Which group exhibited more improvement in spatial reasoning? Create two subsets. Answer: des
des <- subset(cogtrain, cogtrain[, 2]=="des")
aer <- subset(cogtrain, cogtrain[, 2]=="aer")

round(cor(des[3:10]), 2)
round(cor(aer[3:10]), 2)

des$S1.group <- (des$S1.post - des$S1.pre)
des$S2.group <- (des$S2.post - des$S2.pre)
des$result <- (des$S1.group + des$S2.group) / 2
mean(des$result)  #7.06

aer$S1.group <- (aer$S1.post - aer$S1.pre)
aer$S2.group <- (aer$S2.post - aer$S2.pre)
aer$result <- (aer$S1.group + aer$S2.group) / 2
mean(aer$result)  #3.535

#Q8.  Create a color scatterplot matrix for all 4 measures at pre-test. Do the scatterplots suggest two reliable and #valid constructs?  YES

pairs(~cogtrain$S1.pre + cogtrain$S2.pre + cogtrain$V1.pre + cogtrain$V2.pre, cex.labels = 1.2)

base <- cogtrain[ ,c("S1.pre", "S2.pre","V1.pre","V2.pre") ]
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r)
cpairs(base, base.order, panel.colors= base.color, gap = 0.5, main = "Variables Ordered and Colored by Correlation")

#Q9. Create a color scatterplot matrix for all 4 measures at post-test. Do the scatterplots suggest two reliable and #valid constructs?  YES

base2 <- cogtrain[ ,c("S1.post", "S2.post","V1.post","V2.post") ]
base2.r <- abs(cor(base))
base2.color <- dmat.color(base.r)
base2.order <- order.single(base.r)
cpairs(base2, base2.order, panel.colors= base2.color, gap = 0.5, main = "Variables Ordered and Colored by Correlation")

#Q10.  What is the major change from pre-test to post-test visible on the color matrix?  Variance