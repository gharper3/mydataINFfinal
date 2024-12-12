#Live session codes - Week 3: Thursday 09/12



########### Exercise ###########

#In all the exercises, use tidyverse functions, preferably with the piping operator.
library(tidyverse)
#Load and save "AuctionInfo_W3Live.csv" as auctionData and "SellerInfo_W3Live.csv" as sellerData.
auctionData = read.csv("AuctionInfo_W3Live.csv")
sellerData = read.csv("SellerInfo_W3Live.csv")


#1: Update auctionData by removing/dropping the OpenPrice variable.

dplyr::select(auctionData, -OpenPrice)

#2: Merge the seller ratings from sellerData into auctionData. 
#   Make sure correct variables (AuctionID and ID) are used to perform the join.

left_join(auctionData, sellerData, by=c("AuctionID"="ID"))

#3: Using pipes, group the auctionData by Category
#   and calculate the average duration and seller rating for each category. 
#  Then, display the 3 categories with the shortest average duration.



#4: Using pipes, group the auctionData by both Category and endDay 
#   to calculate the average duration for each group. 
#   Afterward, modify the code to only display results where the auction's endDay is Friday. 



#Updated code to filter results to auctions with end day of Friday.



#5: Write a function using pipes that takes a category as input and calculates 
#   the average and standard deviation of the closing price for each endDay. 
#   Test your function on the Books category. 




