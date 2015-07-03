### Stats1.13.Lab.01.R ###

# Basic mathematical operations
3 + 4
[1] 7
5 * 5
[1] 25
12 / 3
[1] 4
5^5
[1] 3125
# R objects

# Vector 
## Most basic object in R 
## Contains elements of the same class
## Can be: character, numeric, integer, complex, logical(True/False))

# Create a vector
v=c(1,3,5,7)
v
[1] 1 3 5 7

# List 
## (Vector with different class of objects) 
l=c("Blue", 2, 5, "Red")
l
[1] "Blue" "2"    "5"    "Red"

# Create a matrix
m=matrix(1:6,2,3)
m
     [,1] [,2] [,3]
[1,]    1    3    5
[2,]    2    4    6

## Matrix creation is column-wise

# Create a matrix from a vector
m2=matrix(1:6)
     [,1]
[1,]    1
[2,]    2
[3,]    3
[4,]    4
[5,]    5
[6,]    6
# Then add dimensionality
dim(m2)=c(2,3)
m2
     [,1] [,2] [,3]
[1,]    1    3    5
[2,]    2    4    6

# Create a matrix by binding columns or rows
x=1:6
y=5:10
cbind(x,y) # by column
     x  y
[1,] 1  5
[2,] 2  6
[3,] 3  7
[4,] 4  8
[5,] 5  9
[6,] 6 10
rbind(x,y) # by row
  [,1] [,2] [,3] [,4] [,5] [,6]
x    1    2    3    4    5    6
y    5    6    7    8    9   10

# Check the attributes
attributes(m)
$dim
[1] 2 3
# Call a particular cell in a matrix
m
     [,1] [,2] [,3]
[1,]    1    3    5
[2,]    2    4    6
m[1,2]
[1] 3

# Dataframes
## Different than matrices => can store different classes of objects
## Usually called with read.table()

# Create a dataframe
d=data.frame(subjectID=1:5,gender=c("M","F","F","M","F"),score=c(8,3,6,5,5))
d
  subjectID gender score
1         1      M     8
2         2      F     3
3         3      F     6
4         4      M     5
5         5      F     5
# Number of rows
nrow(d)
[1] 5
# Number of columns
ncol(d)
[1] 3

# Check the attributes
attributes(d)
$names
[1] "subjectID" "gender"    "score"    

$row.names
[1] 1 2 3 4 5

$class
[1] "data.frame"

# Call a particular cell in a dataframe (use dataframe earlier)
d[2,1]
[1] 2
d[1,2]
[1] M
Levels: F M

# Display dataframe (pulls up the spreadsheet)
View(d)
# Edit dataframe
edit(d)

# Getting help on a function
?functionname

# Download and install packages
install.packages("psych") ## Need to specify CRAN the 1st time

# Load package
library(psych)
search()  ##to see what you have loaded
