#Live session codes - Week 1: Thursday 08/29

taxRate = c(1,5,7.5,10,12.5) #creating a (numerical) vector  and assigning it to a object named tax rate
taxRate #returning the values of the vector
class(taxRate) #checking the type of the vector
str(taxRate)
length(taxRate) #getting the length/size of the vector


logicalVec = (taxRate>10) #creating a (logical vector) checking which tax rates are above 10 (will return a true/false value)
logicalVec #returning the values of the vector
class(logicalVec)


income = c(80,10,NA,180,NA) #creating an income vector
anyNA(income) #check if there are any missing values in the vector (returns a single TRUE or FALSE)
is.na(income) #check each element to see if it's a missing value or not
sum(is.na(income)) #count the number of missing values

income[2]=100 #setting the value of second income equal to 100
# [] indicate cell references

missingIndex = is.na(income) #returns 
income[missingIndex] = 250
missingIndex

income>150 #check to see which income are greater than 150
sum(income>150) #count the number of incomes which are greater than 150


initials = c("BZ","RR","SD","LV",NA) #a character vector: three strings, each containing different characters
class(initials)

mat = cbind(initials,taxRate,income) #combining the vectors into a new vector
mat
str(mat)

df = data.frame(initials,taxRate,income) #combining the vectors into a data frame
df
str(df)

df$tax = df$taxRate/100 * df$income #creating a new variable for actual tax amount of each income

apply(df,1,anyNA) #check which rows have missing values



########### Exercise ###########

#Read the dataset and check some of its basic information:
WV = read.csv("WorldValues_W1Live.csv")
dim(WV) #Dimension of the dataset
head(WV) #First few rows of the dataset
class(WV) #Class type of the data object
str(WV) #Variable types


#1: Does this dataset have missing values? If so, how many missing values does it have?




#2: Create an object that contains unique birth years. How many unique birth years the dataset have?




#3: Create a new variable that stores the age of each respondent at the time of the survey. 
# (add this variable to the dataset)



#4: What is the average age of survey respondents?




#5: How many responses are from country 12? The variable 'Code' captures country code for responses.



#6: What is the average age of respondents from country 12?

