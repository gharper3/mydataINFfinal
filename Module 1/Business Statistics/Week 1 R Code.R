# Sampling using R
# Generate a Random Sample of Size 100
# Draw a 100 random numbers from a normal distributions with mean of 15 and standard #
#deviation of 6.
rnorm(n=100, mean = 15, sd= 6)
library(tidyverse)
# Simple Random Sampling (SRS)
# Create a vector of 100 random numbers drawn from a normal distribution with mean of 0 and
# standard deviation of 1
nd <- rnorm(100)
#Sample 10 of those numbers at random (SRS)
#Use the sample function is used to generate a random sample from a given population
sample(nd, 10)
# Ames Housing Data
#Read your dataset
ames <-read.csv("C:/Users/.../AmesHousing2.csv")
#Preview of dataset and its structure (look at first 6 obs)
head(ames)
#First 10 observations
head(ames, 10)
#Selecting ramdom sample from a data frame (AMES DATA)
#Two step process; sample all the row vectors and select 10 randomly
index <-sample(1:nrow(ames), 10)
#Subset the dataset using this index to get random sample
ames[index, ]
# Stratified Random Sample
# In package dplyr, use group_by function for the strata and sample_n for no. of samples in
each # group
# install.packages("dplyr") if you have not already installed it.
library(dplyr)
ames%>%
  group_by(Bldg.Type)%>%
  sample_n(5)
# Systematic Sampling
# Write a function indicating the step k that has to be taken in the dataset
# N is the total rows, n is every nth number, k = N/n the step
# seq function ties the arguments to generate sequence of numbers
# sample randomly reorders the elements and picks one
obtain_sys = function(N,n){
  k=ceiling(N/n)
  r=sample(1:k, 1)
  seq(r, r+k*(n-1), k)
}
sys.sample = ames[obtain_sys(nrow(ames), 10), ]
head(sys.sample)
