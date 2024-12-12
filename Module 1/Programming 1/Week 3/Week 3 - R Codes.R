#Week 3

########### Goal 2 ###########

#The `tidyverse` is a housing package that holds the following packages:
#- [readr](http://readr.tidyverse.org/) - for reading data in
#- [tibble](https://tibble.tidyverse.org/) - for "tidy" data structures
#- [dplyr](http://dplyr.tidyverse.org/) - for data manipulation
#- [ggplot2](http://ggplot2.tidyverse.org/) - for data visualization
#- [tidyr](http://tidyr.tidyverse.org/) - for cleaning
#- [purrr](http://purrr.tidyverse.org/) - functional programming toolkit



# Install tidyverse package
install.packages('tidyverse')

# Load the package 
require(tidyverse)



########### Goal 3 ###########

## 6 main `dplyr` verbs:

# select(): Pick variables by their names.
# filter(): Pick observations by their values
# arrange(): Reorder the rows.
# mutate(): Create new variables with functions of existing variables.
# summarize(): Collapse many values down to a single summary.
# group_by(): changes the scope of each function from operating on the entire dataset to operating on it group-by-group. 


stk = read_csv("Stocks_W3.csv")

head(stk)

#The following will provide all variables in-between date and AMZN (notice order in original).
dplyr::select(stk, date:AMZN)[1:4,]

# Show first 4 rows
dplyr::select(stk, K, NFLX, date)[1:4,]

# Rename variables
dplyr::select(stk, date, Kellog=K,
              Tesla = TSLA,
              Netflix = NFLX)[1:4,]

# Drop variables with a negative sign in from of variable name (here we drop the K (Kellog) variable)
dplyr::select(stk,-K)

# Create an object with only columns we need
stk2 =  stk %>% dplyr::select(date, DJI, AAPL, TSLA)

# Filter this object by selected date
filter(stk2, date=="2020-01-14")

# Use | ("or")
filter(stk2, date == "2020-01-14" |
             date == "2020-01-15")

# Use & ("and")
filter(stk2, AAPL>75 & TSLA<100)

# Sorting the data
arrange(stk2, TSLA)[1:10, ]

# Sorting in a descending order
arrange(stk2, desc(TSLA))[1:10, ]

# DJI29plus is a new variable that indicates if Dow Jones is above 29k
mutate(stk, DJI29plus = (DJI > 29000))

# Create month and day variables from string date variable
# subset(x, a, b) ("substring") pulls characters from a to b in object x
stk = mutate(stk,
             Month = as.numeric(substr(date, 6, 7)),
             Day = as.numeric(substr(date, 9, 10)))
head(stk)

# Creating new objects that contains summary information
summarize(stk,
          AppleAvg = mean(AAPL),
          AppleMax = max(AAPL),
          AppleMin = min(AAPL))
#control shift m is pipe shortcut

# Count the number of observations and the number of distinct entries
summarize(stk,
          N=n(),
          N_Kellog_prices = n_distinct(K))

# group by month
x <- group_by(stk, Month)
summarize(x, AppleMax = max(AAPL))

# group by month and a Covid indicator
x = mutate(stk, Covid = (Month==4) | (Month==3 & Day>12))
xGroup <- group_by(x, Month, Covid)
summarize(xGroup, AppleMax = max(AAPL))

# Seeing how many observations we have by group
x <- group_by(stk, Month)
count(x)


########### Goal 4 ###########

# Calculate the ratio of stock prices for days in March

# Option 1: Perform each manipulation individually and save each entry as a new object
x <- filter(stk, Month==3)
x <- mutate(x, AppleTeslaRatio = AAPL/TSLA)
x <- arrange(x, AppleTeslaRatio)
x

# Option 2: nest functions within each other
arrange(
  mutate(
    filter(stk, Month == 3),
  AppleTeslaRatio = AAPL/TSLA),
AppleTeslaRatio)

# A better option is to use the pipe function (from dplyr package)

# Task: Rank days in March 2020 ration of Apple prices to Dow Jones average
stk %>%
  filter(Month == 3) %>%
  mutate(AppleDJIPct = 100*AAPL/DJI) %>%
  arrange(desc(AppleDJIPct)) %>%
  select(c(Month,Day,AAPL,DJI,AppleDJIPct))


# Task: To create a list with price by month for each stock
# (Note .groups command in summarize suppresses harmless error message)
stk %>%
  group_by(Month) %>%
  summarize(AppleAvg = mean(AAPL),
            AmazonAvg = mean(AMZN),
            numObs = n(),
            .groups = 'drop') %>%
  arrange(desc(Month))


########### Goal 5 ###########

# Consider the following two example datasets...

data_A = data.frame("country"=c("Nigeria","England","Botswana"),
                    "Var1"=c(4,3,6),stringsAsFactors = F)
data_B = data.frame("country"=c("Nigeria","United States","Botswana"),
                    "Var2"=c("Low","High","Medium"),stringsAsFactors = F)

data_A
data_B


# Left join
left_join(data_A,data_B,by="country")

# Right join
right_join(data_A,data_B,by="country")

# Inner join
inner_join(data_A,data_B,by="country")

# Full join
full_join(data_A,data_B,by="country")

# Anti join
anti_join(data_A,data_B,by="country")


# Bind rows
bind_rows(data_A,data_B)

# Bind cols
bind_cols(data_A,data_B)


# Consider updated data_A and data_B defined as the following:

data_A = data.frame("country"=c("Nigeria","England","Botswana"),
                    "year"=c(1999,2001,2000),"Var1"=c(4,3,6),stringsAsFactors = F)
data_B = data.frame("country_name"=c("Nigeria","United States","Botswana"),
                    "year"=c(1999,2004,2003),"Var2"=c("Low","High","Medium"),stringsAsFactors = F)

# Disparate column names
full_join(data_A,data_B,
          by=c('country'='country_name',
               'year'))

