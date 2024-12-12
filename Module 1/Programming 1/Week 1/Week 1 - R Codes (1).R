#Week 1

########### Goal 3 ###########
# Variable named x.numeric contains the numbers 7 and 10
x.numeric <- c(7, 10)
x.numeric

# Variable named x.logic is true/false
# Based on condition in parentheses
x.logical <- (x.numeric > 8)
x.logical

# Notice double equal sign in condition
x.logical2 <- (x.numeric == 7)
x.logical2

# Variable named x.string has words
x.string <- c("Hello", "World", "how are you doing?", 7)
x.string

#If you want to know what type of data a variable is use class()
x.numeric
class(x.numeric)

x.string
class(x.string)


# Create a a new variable. NA is specific term for missing data in R
x.new = c(7, 10, NA, 3, 1)
x.new
class(x.new)

# What happens here?
x.dot = c(7, 10, ".", 3, 1)
x.dot
class(x.dot)


Missing.indicator = is.na(x.new)
Missing.indicator

#What data type is the variable Missing.indicator?
class(Missing.indicator)


########### Goal 4 ###########
# Create a vector called x1
x1 <- c(1, 4, -1, 4, 1, 5)
x1

# x2 is simply 2 x x1 for each element
x2 <- 2 * x1
x2

x2[3] = NA
x2

# Set all elements of x1 that are < 2 to equal 200
x1[x1 < 2] = 200
x1

# the is.na() command indicates if an element is NA
is.na(x.new)

# Show observations that are not missing
# Equivalent ways to represent the same information
x.new[is.na(x.new) == FALSE]

x.new[is.na(x.new) == 0]

x.new[is.na(x.new) != 1]


x.no.missing = x.new[is.na(x.new) == 0]
x.no.missing

#What is length of x.no.missing?
length(x.no.missing)


Matrix.1 = cbind(x1, x2)
Matrix.1

#Check number of rows and columns with dim()
dim(Matrix.1)

# First column
Matrix.1[, 1]

# Third row
Matrix.1[3, ]

# Rows of Matrix.1 where first column is greater than 10
Matrix.1[Matrix.1[,1] > 10, ]


df <- data.frame(st = c("UT", "NV", "OR","TX", "NY", NA),
                 Wages = x1,
                 Spend = x2)
df
names(df)

# 2nd column
df[, 2]

# 4th row
df[4, ]

#Dollar sign notation
df$Wages

# Rows of df with wages < 10
df[df$Wages < 10, ]


#It is often useful/necessary to figure out the data structure of an object
str(x1)
str(Matrix.1)
str(df)
class(x1)


########### Goal 5 ###########
#List the data objects in memory
objects()


# mean() is basic function -- as are sum(), min() etc
mean(x1)

# See unique values
unique(x1)

# table() provides the frequency distribution of each value
table(x1)

# summary() provides basic descriptive stats
summary(x1)


#We often need to deal with missing data
x2
mean(x2)

#For many functions, remove NA observations with ", na.rm=TRUE"
mean(x2, na.rm = TRUE)
median(x1, na.rm = TRUE)
max(x.new, na.rm = TRUE)


# For matrix or data frame - need to specify what mean we're looking for
Matrix.1

# Mean of all data in matrix (seldom useful)
mean(Matrix.1, na.rm = TRUE)

# Mean of column 1
mean(Matrix.1[,1])


#apply() function applies a function to specified dimension of a matrix
Matrix.1

# Means of rows
apply(Matrix.1, 1, mean)

# Means of columns
apply(Matrix.1, 2, mean)

# Can add additional information for the function (such as dealing with NA)
apply(Matrix.1, 2, mean, na.rm=TRUE)

