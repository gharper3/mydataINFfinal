# Load required libraries
library(dplyr)
library(caret)

# Load the dataset
df <- read.csv("C:/Users/harpe/Documents/Georgetown MSBA/Machine Learning 1/OPAN-6602/Employee_Data_Project.csv")

# Exploratory Data Analysis
str(df)
summary(df)

# Check for missing values in each column
missing_per_column <- colSums(is.na(df))
cat("Missing values per column:\n")
print(missing_per_column)

# Replace missing values in specific columns with the rounded average
columns_to_impute <- c("NumCompaniesWorked", "TotalWorkingYears", 
                       "EnvironmentSatisfaction", "JobSatisfaction")

for (col in columns_to_impute) {
  mean_value <- mean(df[[col]], na.rm = TRUE)
  df[[col]][is.na(df[[col]])] <- round(mean_value)
}

# Check missing values after imputation
missing_per_column_after <- colSums(is.na(df))
cat("Missing values per column after imputation:\n")
print(missing_per_column_after)

# Convert relevant variables into factors
df <- df %>%
  mutate(
    Attrition = factor(Attrition, levels = c("No", "Yes")),
    BusinessTravel = factor(BusinessTravel),
    Education = factor(Education, levels = c(1, 2, 3, 4, 5), 
                       labels = c("Below College", "College", "Bachelor", "Master", "Doctor")),
    Gender = factor(Gender, levels = c("Female", "Male")),
    JobLevel = factor(JobLevel, levels = c(1, 2, 3, 4, 5)),
    MaritalStatus = factor(MaritalStatus, levels = c("Single", "Married", "Divorced")),
    EnvironmentSatisfaction = factor(EnvironmentSatisfaction, levels = c(1, 2, 3, 4), 
                                     labels = c("Low", "Medium", "High", "Very High")),
    JobSatisfaction = factor(JobSatisfaction, levels = c(1, 2, 3, 4), 
                             labels = c("Low", "Medium", "High", "Very High"))
  )

# Detect and handle outliers
df <- df %>%
  mutate(across(where(is.numeric), ~ ifelse(. > quantile(., 0.95, na.rm = TRUE),
                                            quantile(., 0.95, na.rm = TRUE),
                                            ifelse(. < quantile(., 0.05, na.rm = TRUE),
                                                   quantile(., 0.05, na.rm = TRUE), .))))

# Normalize numeric variables
df <- df %>%
  mutate(across(where(is.numeric), ~ scale(.) %>% as.vector()))

# Split into 80-20 train-test split
set.seed(123)  # For reproducibility
train_index <- createDataPartition(df$Attrition, p = 0.8, list = FALSE)
train_set <- df[train_index, ]
test_set <- df[-train_index, ]

cat("Training set size before SMOTE:", nrow(train_set), "\n")
cat("Testing set size:", nrow(test_set), "\n")

# Check class distribution in training data
cat("Class distribution in training set before SMOTE:\n")
print(table(train_set$Attrition))

# Apply SMOTE using caret
set.seed(123)  # For reproducibility
smote_train <- upSample(x = train_set[, -which(names(train_set) == "Attrition")],
                        y = train_set$Attrition, 
                        yname = "Attrition")

# Check class distribution after SMOTE
cat("Class distribution in training set after SMOTE:\n")
print(table(smote_train$Attrition))

# Verify the new training set size
cat("Training set size after SMOTE:", nrow(smote_train), "\n")




