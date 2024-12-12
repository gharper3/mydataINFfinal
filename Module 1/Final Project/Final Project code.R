#Programming 1 - Final Project
#Your name(s): Andrew Harper, Andrew Singh


listingdata = read.csv("Listings.csv")
reviewdata = read.csv("Reviews.csv")


complete_data <- left_join(listingdata,reviewdata, by=c("id"="listing_id"))

# Assuming complete_data is your dataframe
# Count total number of missing values in the entire dataset
total_missing <- sum(is.na(complete_data))

# Calculate the mean of the host_acceptance_rate, excluding NA values
mean_acceptance_rate <- mean(complete_data$host_acceptance_rate, na.rm = TRUE)

# Replace NA values in host_acceptance_rate with the calculated mean
complete_data_draft <- complete_data %>%
  mutate(host_acceptance_rate = ifelse(is.na(host_acceptance_rate), mean_acceptance_rate, host_acceptance_rate))

# Replace blank cells in room_type with "Entire home/apt"
complete_data_draft <- complete_data_draft %>%
  mutate(room_type = case_when(
    is.na(room_type) & grepl("private", bathrooms, ignore.case = TRUE) ~ "Private room",
    is.na(room_type) & grepl("shared", bathrooms, ignore.case = TRUE) ~ "Shared room",
    is.na(room_type) ~ "Entire home/apt",
    TRUE ~ room_type
  ))

# Calculate the mean of the bedrooms, excluding NA values
mean_bedrooms <- ceiling(mean(complete_data_draft$bedrooms, na.rm = TRUE))


# Replace blank values in bedrooms based on the number of beds
complete_data_draft <- complete_data_draft %>%
  mutate(bedrooms = case_when(
    bedrooms == "" & beds > 1 ~ mean_bedrooms,
    bedrooms == "" & beds == 1 ~ 1,
    TRUE ~ bedrooms
  ))

# Print the updated dataframe
print(complete_data_draft)
