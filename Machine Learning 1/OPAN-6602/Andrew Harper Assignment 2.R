# Load necessary libraries
install.packages("ResourceSelection")
install.packages("glmnet")
install.packages("gridExtra")
library(tidyverse)
library(caret)
library(GGally)
library(corrplot)
library(car)
library(pROC)
library(lmtest)
library(aod)
library(gains)
library(reshape2)
library(dplyr)
library(ResourceSelection)
library(glmnet)
library(splines)
library(gridExtra)
library(grid)

# Load the dataset (Q1a, Q1b)
df <- read.csv("C:/Users/harpe/Documents/Georgetown MSBA/Machine Learning 1/OPAN-6602/Employee_Data_Project.csv")

# Exploratory Data Analysis (EDA) (Q1a, Q3a, Q4a)
str(df)
summary(df)

# Check Attrition distribution (Q3a: Marginal distribution of Attrition)
cat("Attrition Distribution:\n")
print(table(df$Attrition))
print(prop.table(table(df$Attrition)))

# Check for missing values and clean the dataset (Q1a)
missing_values <- sum(is.na(df))
df_cleaned <- df[complete.cases(df), ]
cat("Missing values after cleaning:", sum(is.na(df_cleaned)), "\n")

# Detect and handle outliers (Q1a)
numeric_vars <- df_cleaned %>% select(where(is.numeric))
df_cleaned <- df_cleaned %>%
  mutate(across(where(is.numeric), ~ ifelse(. > quantile(., 0.95, na.rm = TRUE),
                                            quantile(., 0.95, na.rm = TRUE),
                                            ifelse(. < quantile(., 0.05, na.rm = TRUE),
                                                   quantile(., 0.05, na.rm = TRUE), .))))
# Visualizations
# Calculate summary statistics from df dataset
df_summary <- df_cleaned %>%
  group_by(Attrition) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Std_Deviation = sd(Age, na.rm = TRUE),
    Min_Age = min(Age, na.rm = TRUE),
    Max_Age = max(Age, na.rm = TRUE),
    Count = n()
  ) %>%
  ungroup()

# Customize the table theme
custom_theme <- ttheme_default(
  core = list(
    fg_params = list(fontsize = 12),
    bg_params = list(fill = c(rep(c("#f9f9f9", "#ffffff"), length.out = nrow(df_summary))))
  ),
  colhead = list(
    fg_params = list(fontsize = 14, fontface = "bold", col = "white"),
    bg_params = list(fill = "#003f5c")  # Navy blue header
  )
)

# Create the table
table_plot <- tableGrob(df_summary, rows = NULL, theme = custom_theme)

# Add a title to the table
title <- textGrob("Summary of Age Statistics by Attrition", gp = gpar(fontsize = 16, fontface = "bold"))

# Combine title and table
grid.arrange(table_plot, top = title)

# Boxplot for Job Satisfaction by Attrition
ggplot(df, aes(x = Attrition, y = JobSatisfaction, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Job Satisfaction by Attrition Status",
       x = "Attrition",
       y = "Job Satisfaction Level") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Histogram for Age distribution by Attrition
ggplot(df, aes(x = Age, fill = Attrition)) +
  geom_histogram(binwidth = 3, position = "identity", alpha = 0.6) +
  labs(title = "Age Distribution by Attrition Status",
       x = "Age",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red"))

# Bar plot for Gender proportions in Attrition
ggplot(df, aes(x = Gender, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(title = "Gender Proportion by Attrition Status",
       x = "Gender",
       y = "Proportion") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Scatterplot for Income vs. Total Working Years by Attrition
ggplot(df, aes(x = TotalWorkingYears, y = Income, color = Attrition)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Income vs. Total Working Years by Attrition Status",
       x = "Total Working Years",
       y = "Income") +
  theme_minimal() +
  scale_color_manual(values = c("No" = "blue", "Yes" = "red"))
# Standardize numeric variables (Q1a)
df_cleaned <- df_cleaned %>%
  mutate(Age = c(scale(Age)),
         Income = c(scale(Income)),
         TotalWorkingYears = c(scale(TotalWorkingYears)),
         DistanceFromHome = c(scale(DistanceFromHome)))

# Convert categorical variables to factors (Q1b)
df_cleaned <- df_cleaned %>%
  mutate(Attrition = factor(Attrition, levels = c("No", "Yes")),
         Gender = factor(Gender),
         BusinessTravel = factor(BusinessTravel),
         MaritalStatus = factor(MaritalStatus),
         Education = factor(Education),
         JobLevel = factor(JobLevel),
         EnvironmentSatisfaction = factor(EnvironmentSatisfaction),
         JobSatisfaction = factor(JobSatisfaction))

# Split the data into training and testing sets (Q6a)
set.seed(123)
train_index <- createDataPartition(df_cleaned$Attrition, p = 0.7, list = FALSE)
train <- df_cleaned[train_index, ]
test <- df_cleaned[-train_index, ]

# Logistic Regression Models (Q2a, Q5a-d)
model_1 <- glm(Attrition ~ Age, data = train, family = binomial)  # Model 1
model_2 <- glm(Attrition ~ Age + Gender, data = train, family = binomial)  # Model 2
model_3 <- glm(Attrition ~ Age + Gender + JobSatisfaction, data = train, family = binomial)  # Model 3
model_4 <- glm(Attrition ~ Age * Gender + JobSatisfaction + Income + Gender:Income, data = train, family = binomial)  # Model 4

# Check Assumptions (Q2a)
# 1. Multicollinearity (using VIF)
vif_values <- vif(model_4)
cat("VIF Values for Model 4 (Multicollinearity Check):\n")
print(vif_values)

# 2. Independence of Errors (using Durbin-Watson test)
dw_test <- dwtest(model_4)
cat("Durbin-Watson Test (Independence of Errors):\n")
print(dw_test)

# 3. Linearity of Logit Assumption
# Create logit for observed probabilities
train$logit <- log(predict(model_4, type = "response") / (1 - predict(model_4, type = "response")))
numeric_vars_train <- train %>% select(where(is.numeric)) %>% select(-logit)

# Test linearity by adding interaction terms with logit
linearity_test <- glm(Attrition ~ logit * (Age + Income + TotalWorkingYears), data = train, family = binomial)
summary(linearity_test)
cat("Linearity of Logit Test Results:\n")
print(summary(linearity_test))


# Summarize models (Q2b, Q2c)
summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)

# Addressing violations of linearity of the logit assumption
train <- train %>%
  mutate(
    Age_ns = predict(ns(Age, df = 3), Age),
    Income_ns = predict(ns(Income, df = 3), Income),
    TotalWorkingYears_ns = predict(ns(TotalWorkingYears, df = 3), TotalWorkingYears)
  )

test <- test %>%
  mutate(
    Age_ns = predict(ns(train$Age, df = 3), Age),
    Income_ns = predict(ns(train$Income, df = 3), Income),
    TotalWorkingYears_ns = predict(ns(train$TotalWorkingYears, df = 3), TotalWorkingYears)
  )

# Fit logistic regression models
model_1 <- glm(Attrition ~ Age_ns, data = train, family = binomial)
model_2 <- glm(Attrition ~ Age_ns + Gender, data = train, family = binomial)
model_3 <- glm(Attrition ~ Age_ns + Gender + JobSatisfaction, data = train, family = binomial)
model_4 <- glm(Attrition ~ Age_ns * Gender + JobSatisfaction + Income_ns + TotalWorkingYears_ns, 
               data = train, family = binomial)

summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)
# Check assumptions
# Multicollinearity
cat("VIF Values for Model 4:\n")
print(vif(model_4))

# Independence of Errors
dw_test <- dwtest(model_4)
cat("Durbin-Watson Test:\n")
print(dw_test)

# Evaluate models
models <- list(Model1 = model_1, Model2 = model_2, Model3 = model_3, Model4 = model_4)
performance_metrics <- data.frame(Model = character(), AIC = numeric(), AUC = numeric(),
                                  Precision = numeric(), Recall = numeric(), F1_Score = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(models)) {
  model <- models[[i]]
  predicted_probs <- predict(model, newdata = test, type = "response")
  predicted_classes <- ifelse(predicted_probs > 0.5, "Yes", "No")
  conf_matrix <- confusionMatrix(as.factor(predicted_classes), test$Attrition, positive = "Yes")
  auc_value <- auc(roc(test$Attrition, predicted_probs))
  precision <- conf_matrix$byClass["Pos Pred Value"]
  recall <- conf_matrix$byClass["Sensitivity"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  performance_metrics <- rbind(performance_metrics, data.frame(
    Model = names(models)[i], AIC = AIC(model), AUC = auc_value, Precision = precision,
    Recall = recall, F1_Score = f1_score))
}
print(performance_metrics)

# Visualize model performance
performance_long <- melt(performance_metrics, id.vars = "Model", variable.name = "Metric", value.name = "Value")
ggplot(performance_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Performance Metrics", x = "Model", y = "Metric Value") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

# ROC Curves for models
roc_curves <- lapply(models, function(model) roc(test$Attrition, predict(model, newdata = test, type = "response")))
plot(roc_curves[[1]], col = "red", main = "ROC Curves")
for (i in 2:length(roc_curves)) {
  plot(roc_curves[[i]], add = TRUE, col = i + 1)
}
legend("bottomright", legend = names(models), col = seq_along(models) + 1, lty = 1)

# Predict probabilities for younger and older employees
# Precompute spline basis for the training dataset
# Precompute splines for the training data
# Precompute spline basis for the training dataset **revisit**
age_spline_basis <- ns(train$Age, df = 3)  # Spline for Age
income_spline_basis <- ns(train$Income, df = 3)  # Spline for Income
working_years_spline_basis <- ns(train$TotalWorkingYears, df = 3)  # Spline for TotalWorkingYears

# Add transformed spline variables to the training data
train <- train %>%
  mutate(
    Age_ns = as.matrix(age_spline_basis),
    Income_ns = as.matrix(income_spline_basis),
    TotalWorkingYears_ns = as.matrix(working_years_spline_basis)
  )

# Create younger_data and older_data with transformed variables
younger_data <- data.frame(
  Age_ns = as.matrix(predict(age_spline_basis, newdata = data.frame(Age = -1))),  # Transform Age = -1
  Gender = factor("Male", levels = levels(train$Gender)),  # Match levels of Gender
  JobSatisfaction = factor(3, levels = levels(train$JobSatisfaction)),  # Match levels of JobSatisfaction
  Income_ns = as.matrix(predict(income_spline_basis, newdata = data.frame(Income = 0))),  # Transform Income = 0
  TotalWorkingYears_ns = as.matrix(predict(working_years_spline_basis, newdata = data.frame(TotalWorkingYears = 5)))  # Transform TotalWorkingYears = 5
)

older_data <- data.frame(
  Age_ns = as.matrix(predict(age_spline_basis, newdata = data.frame(Age = 1))),  # Transform Age = 1
  Gender = factor("Male", levels = levels(train$Gender)),  # Match levels of Gender
  JobSatisfaction = factor(3, levels = levels(train$JobSatisfaction)),  # Match levels of JobSatisfaction
  Income_ns = as.matrix(predict(income_spline_basis, newdata = data.frame(Income = 0))),  # Transform Income = 0
  TotalWorkingYears_ns = as.matrix(predict(working_years_spline_basis, newdata = data.frame(TotalWorkingYears = 5)))  # Transform TotalWorkingYears = 5
)

# Confirm younger_data structure matches the model
str(younger_data)
str(model_1)
# Predict probabilities for younger and older employees
younger_prob <- predict(model_4, newdata = younger_data, type = "response")
older_prob <- predict(model_4, newdata = older_data, type = "response")

# Output predicted probabilities
cat("Predicted probability for younger employee:", younger_prob, "\n")
cat("Predicted probability for older employee:", older_prob, "\n")


coefficients <- list(
  Intercept = 0.37375,
  Age_ns1 = -1.13746,
  Age_ns2 = -0.96044,
  Age_ns3 = 0.07086
)

# Generate a sequence of ages
age_range <- seq(20, 60, by = 1)

# Calculate natural spline basis functions for Age
age_ns <- ns(age_range, df = 3)

# Compute log-odds (logit) using the model coefficients
logit <- coefficients$Intercept +
  coefficients$Age_ns1 * age_ns[, 1] +
  coefficients$Age_ns2 * age_ns[, 2] +
  coefficients$Age_ns3 * age_ns[, 3]

# Convert logit to probabilities using the logistic function
probability <- exp(logit) / (1 + exp(logit))

# Create a data frame for plotting
data <- data.frame(
  Age = age_range,
  Probability = probability
)

# Plot the relationship between Age and Attrition Probability
ggplot(data, aes(x = Age, y = Probability)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", size = 1) +  # 50% threshold
  geom_rect(aes(xmin = 20, xmax = 30, ymin = 0, ymax = 1), fill = "lightcoral", alpha = 0.1, inherit.aes = FALSE, show.legend = FALSE) +  # High-risk zone
  geom_rect(aes(xmin = 50, xmax = 60, ymin = 0, ymax = 1), fill = "lightcoral", alpha = 0.1, inherit.aes = FALSE, show.legend = FALSE) +  # Low-risk zone
  annotate("text", x = 25, y = 0.75, label = "High Attrition Risk", color = "darkred", size = 5, fontface = "bold") +
  annotate("text", x = 55, y = 0.15, label = "High Attrition Risk", color = "darkred", size = 5, fontface = "bold") +
  geom_line(color = "darkblue", size = 1.2) +  # Attrition probability curve
  labs(
    title = "Age and Predicted Probability of Attrition (Model 4)",
    x = "Age (Years)",
    y = "Probability of Attrition"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  )

