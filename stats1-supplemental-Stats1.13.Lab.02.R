# Statistics One Lab 2 

# Lab goals
#   Read a datafile into R
#   Learn more about object types
#   Print summary statistics
#   Examine distributions using histograms (use hist function)

# Example
#   Investigating the effects of sports-related concussion
#   Simulated data are based on an online assessment tool called IMPACT (http://www.impacttest.com)
#   IMPACT provides 6 main measures, listed here:
#     Memory composite verbal
#     Memory composite visual
#     Visual motor speed composite
#     Reaction time composite
#     Impulse control composite
#     Total symptom score

#use command + 1 to go to the console, use command + 2 to go to the R editor
# Check your working directory (tells you what directory you have set, if any)
getwd()   
# If necessary, set your working directory
# setwd("/Users/karenyang/Desktop/StatisticsOne12weeks")
setwd("/Users/karenyang/Desktop/StatisticsOne12weeks")
[1] "/Users/karenyang/Desktop/StatisticsOne12weeks"
getwd()
[1] "/Users/karenyang/Desktop/StatisticsOne12weeks"

# If necessary, install packages
# install.packages("psych")
# install.packages("sm")

# Load packages
library(psych)
library(sm)
#to see what packages that you have, use search() or installed.packages()
search()
 [1] ".GlobalEnv"        "package:sm"        "package:psych"    
 [4] "tools:RGUI"        "package:stats"     "package:graphics" 
 [7] "package:grDevices" "package:utils"     "package:datasets" 
[10] "package:methods"   "Autoloads"         "package:base" 

# Read data into a dataframe called impact (two ways)
#impact <- read.table("stats1-datafiles-Stats1.13.Lab.02.txt", header = T)

#1 Do a "Save AS" from url website and put file in a directory, find filepath
impact <- read.table("/Users/karenyang/Desktop/StatisticsOne12weeks/stats1-datafiles-Stats1.13.Lab.02.txt", header = T) 

#2 Alternatively, to read directly from url, copy url address and remove the "s" from https
impact <- read.table("http://d396qusza40orc.cloudfront.net/stats1%2Fdatafiles%2FStats1.13.Lab.02.txt", header = T) 

# Get the dimensions of the dataframe
dim(impact)  #40 14
nrow(impact)
ncol(impact)

#to see your data and open up the R data editor to make changes
edit(impact)

# Object types
class(impact)   #"data.frame"
names(impact)   # list or names of variables 

class(impact$verbal_memory_baseline)      #"integer"
class(impact$reaction_time_baseline)      #"numeric"
class(impact$subject)                     #"integer"

impact$subject <- factor(impact$subject) # to change class to a factor variable
class(impact$subject)

# Summary statistics
mean(impact$verbal_memory_baseline)   #89.75
sd(impact$verbal_memory_baseline)     #6.444039

describe(impact)   #gives summary of all variables, trimmed mean removes extreme values
#mad = mean absolute deviation score, |x-M|
                            var  n  mean    sd median trimmed   mad   min   max
subject*                      1 40 20.50 11.69  20.50   20.50 14.83  1.00 40.00
condition*                    2 40  1.50  0.51   1.50    1.50  0.74  1.00  2.00

describeBy(impact, impact$condition)   #split by condition

# Subsetting
edit(impact)

control <- subset(impact, impact[, 2]=="control")   #use column 2
control

concussed <- subset(impact, impact[, 2]=="concussed")
concussed

# Histograms of control group at baseline
par(mfrow = c(2,3)) # To view 6 histograms on one page in 2 rows and 3 columns, fill by row
hist(control[, 3], xlab = "Verbal memory", main = "")   #par means parameter
hist(control[, 4], xlab = "Visual memory")             #mfrow means matrix formation by row
hist(control[, 5], xlab = "Visual motor speed")
hist(control[, 6], xlab = "Reaction time")
hist(control[, 7], xlab = "Impulse control")
hist(control[, 8], xlab = "Total symptom score")

# To demonstrate that there is more than one way to access a variable
par(mfrow = c(1,2)) # To view 2 histograms on one page 
hist(control[, 3], xlab = "Verbal memory", main = "") 
hist(control$verbal_memory_baseline, xlab = "Verbal memory", main = "") 

# Histograms of concussed group at baseline
par(mfrow = c(2,3))
hist(concussed[, 3], xlab = "Verbal memory", main = "")
hist(concussed[, 4], xlab = "Visual memory", main = "")
hist(concussed[, 5], xlab = "Visual motor speed", main = "")
hist(concussed[, 6], xlab = "Reaction time", main = "")
hist(concussed[, 7], xlab = "Impulse control", main = "")
hist(concussed[, 8], xlab = "Total symptom score", main = "")

# Histograms of control group at retest
par(mfrow = c(2,3))
hist(control[, 9], xlab = "Verbal memory", main = "") 
hist(control[, 10], xlab = "Visual memory", main = "")
hist(control[, 11], xlab = "Visual motor speed", main = "")
hist(control[, 12], xlab = "Reaction time", main = "")
hist(control[, 13], xlab = "Impulse control", main = "")
hist(control[, 14], xlab = "Total symptom score", main = "")

# Histograms of concussed group at retest
par(mfrow = c(2,3))
hist(concussed[, 9], xlab = "Verbal memory", main = "")
hist(concussed[, 10], xlab = "Visual memory", main = "")
hist(concussed[, 11], xlab = "Visual motor speed", main = "")
hist(concussed[, 12], xlab = "Reaction time", main = "")
hist(concussed[, 13], xlab = "Impulse control", main = "")
hist(concussed[, 14], xlab = "Total symptom score", main = "")

# Density plots  (Use sm package to obtain smoothed histogram)
par(mfrow = c(1,2))
hist(concussed[, 14], xlab = "Total symptom score", main = "")
plot(density(concussed[, 14]), xlab = "Total sympton score", main = "")
#the previous line smooths the histogram

# Compare density plots  
par(mfrow = c(1,1))
sm.density.compare(impact$total_symptom_retest, impact$condition, xlab = "Total symptom score")               



