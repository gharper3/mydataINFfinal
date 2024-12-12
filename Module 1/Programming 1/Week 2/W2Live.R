#Live session codes - Week 2: Thursday 09/05

# Create and load some objects
string.vec = c("Comp1", "Comp2", "Comp3", "Comp4", "Comp5")  # A character vector
logical.vec = c(TRUE, FALSE, TRUE)  # A logical vector (TRUE/FALSE)
 
W2Data = read.csv("W2Live-Data.csv") #load a data frame from a CSV file

#Create a list called testList, which combines multiple objects into one structure
#Lists in R can store different types of objects together (e.g., vectors, data frames)
testList = list(strVec = string.vec, logVec = logical.vec, auctionData = W2Data)   
testList  #Print the full list to see all elements

#Accessing elements of a list:
#First object in the testList (the string vector)
testList[[1]]  #Using double brackets to extract the first object (strVec)

#Accessing the third element of the first object (strVec) in testList
testList[[1]][3]  #Double brackets for the object, single brackets for the element
testList[[1]][seq(1,5,2)]
#Alternative ways to access the third element of strVec:
testList[["strVec"]][3]  #By name using double brackets
testList$strVec[3]  #Using the dollar sign to access list elements by name

#Accessing elements in the third object of testList (auctionData, which is a data frame)
testList[[3]][2]  #Access the second element of the third object (returns a column)
testList[[3]][,2]  #Access the second column of the data frame (auctionData)


#Loops:

#This is a simple loop that iterates through numbers 1 to 4. 
#For each number, it prints out the current value of 'i' (the counter).
for (i in 1:4){
  cat("The value of the counter is", i, "\n")
}

#A loop to go over the dataset and print the category of each auction using a prompt
for (i in 1:nrow(W2Data)){
  cat("The category of this auction is", W2Data$Category[i], "\n")
}


#A loop to go over the first ten auctions and save their duration to another vector
# Note: We first create an empty/null vector of size 10 using rep() function
tempVec = c()  # Initialize an empty vector
for (i in 1:10){
  tempVec[i] = W2Data$Duration[i]
}
tempVec


#If statements and logical tests
x=5 #set the value of x to an arbitrary value for the exercise

#Traditional if-else statement:
if (x > 10) {
  "x is greater than 10"
} else {
  "x is not greater than 10"
}

#Now simplified using ifelse:
ifelse(x > 10, "x is greater than 10", "x is not greater than 10")


ifelse(W2Data$endDay=="Mon",T,F) #Check all endDays to see if they're Monday or not
ifelse(W2Data$endDay[1]=="Mon",T,F) #Check the first endDay to see if it's Monday or not


#Check if Art is an auction category
ifelse("Art" %in% W2Data$Category,"Yes it is!","No it's not!") 

#Check if the first auction's category is computer or books:
ifelse(W2Data$Category[1] %in% c("Computer","Books"),T,F) 

#Create a variable that shows if each auction's end day is Monday or not
W2Data$Monday = ifelse(W2Data$endDay=="Mon",TRUE,FALSE)


#Combining if statements and loops ("if" inside a loop)

#Loop through the first 10 auctions. If the auction's duration is greater than 50,
#it is divided by 10 and updated. Otherwise, the value is left unchanged.
for (i in 1:10){
  if (W2Data$Duration[i]>50){
    W2Data$Duration[i] = W2Data$Duration[i]/10
  }
}



#Functions:

#This custom function takes two arguments (x and y) and calculates x raised to the power of y.
#The 'return' ensures that the outcome is available to the user when the function is called.
powerFunction = function(x,y){
  outcome=x^y
  return(outcome) #this makes sure results are returned
}
powerFunction(2,3) #example: take 2 to the power of 3


#Create a custom function to filter data by a specific seller rating
ratingFilter = function(x){
  W2Data[W2Data$sellerRating==x,]
}

ratingFilter(10) #example: only show data for sellers with rating 10



########### Exercise ###########
#In this exercise, you will practice creating custom functions, 
# working with loops, and manipulating datasets in R.


#Read the data and save it as W2Data
W2Data = read.csv("W2Live-Data.csv")

#1: Write a function called AVG that calculates the average of a given vector. 
#   Do NOT use the base R mean() function!
AVG <- function(vec) {
  sum_vec <- sum(vec)
  length_vec <- length(vec)
  avg <- sum_vec / length_vec
  return(avg)
}



#2: Use the AVG to calculate the average seller rating of all auctions.
#   Save this value as avgRating.
#   Does the base R mean() function returns the same value?
avgRating <- AVG(W2Data$sellerRating)

# Calculate average seller rating using base R mean() function
base_avgRating <- mean(W2Data$sellerRating)

# Print the results
cat("Average seller rating using AVG function: ", avgRating, "\n")
cat("Average seller rating using base R mean() function: ", base_avgRating, "\n")




#3: Create a loop that goes over the dataset and prints a prompt along with 
#   the seller rating of each auction.

for (i in 1:nrow(W2Data)) {
  cat("Seller rating for auction", i, "is", W2Data$sellerrating[i], "\n")
}


#4: Write a code that loops over the dataset, checks the category of each auction,
#   and if it is Music/Movie/Game category, change it to Entertainment.

# Sample dataset with categories


# Loop over the dataset and change category
for (i in 1:nrow(W2Data)) {
  if (W2Data$category[i] == "Music/Movie/Game") {
    W2Data$category[i] <- "Entertainment"
  }
}

# Print the updated dataset
print(auctions)
