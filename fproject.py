#!/usr/bin/env python
# coding: utf-8

# # Final Project (Individual)
# ## Andrew Harper
# ### December 7th, 2024

# ***

# #### 1. Read in the data, call the dataframe "s"  and check the dimensions of the dataframe

# In[40]:

import streamlit as st
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, confusion_matrix, classification_report
import matplotlib.pyplot as plt
import seaborn as sns

# Streamlit app setup
st.title("LinkedIn User Prediction App")
st.markdown("""This app predicts whether a person is likely to be a LinkedIn user based on demographic and personal information.
Please enter the required details below.""")

# Mapping dictionaries for labels
income_labels = {
    1: "Less than $10,000",
    2: "10 to under $20,000",
    3: "20 to under $30,000",
    4: "30 to under $40,000",
    5: "40 to under $50,000",
    6: "50 to under $75,000",
    7: "75 to under $100,000",
    8: "100 to under $150,000",
    9: "$150,000 or more",
}

education_labels = {
    1: "Less than high school (Grades 1-8 or no formal schooling)",
    2: "High school incomplete (Grades 9-11 or Grade 12 with NO diploma)",
    3: "High school graduate (Grade 12 with diploma or GED certificate)",
    4: "Some college, no degree (includes some community college)",
    5: "Two-year associate degree from a college or university",
    6: "Four-year college or university degree/Bachelor’s degree (e.g., BS, BA, AB)",
    7: "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)",
    8: "Postgraduate or professional degree, including master’s, doctorate, medical or law degree (e.g., MA, MS, PhD, MD, JD)",
}

marital_labels = {
    1: "Married",
    2: "Living with a partner",
    3: "Divorced",
    4: "Separated",
    5: "Widowed",
    6: "Never been married",
}

# Input fields
income = st.selectbox("Household Income", options=list(income_labels.keys()), format_func=lambda x: income_labels[x])
education = st.selectbox("Highest Level of Education", options=list(education_labels.keys()), format_func=lambda x: education_labels[x])
marital = st.selectbox("Current Marital Status", options=list(marital_labels.keys()), format_func=lambda x: marital_labels[x])
parent = st.radio("Are you a parent?", options=[1, 0], format_func=lambda x: "Yes" if x == 1 else "No")
female = st.radio("Gender", options=[1, 0], format_func=lambda x: "Female" if x == 1 else "Male")
age = st.slider("Age (up to 98)", min_value=1, max_value=98, value=30, step=1)

# Convert marital status into binary (1 = married, 0 = not married) for the model
is_married = 1 if marital == 1 else 0

# Collect inputs into a list
input_data = [income, education, parent, marital, female, age]

# Load the data
s = pd.read_csv('https://raw.githubusercontent.com/gharper3/fproject/refs/heads/main/social_media_usage.csv')
print("Dimensions of the dataframe:", s.shape)


# ***

# #### 2. Define a function called clean_sm that takes one input, x, and uses `np.where` to check whether x is equal to 1. If it is, make the value of x = 1, otherwise make it 0. Return x. Create a toy dataframe with three rows and two columns and test your function to make sure it works as expected

# In[36]:


def clean_sm(x):
    return np.where(x == 1, 1, 0)

# Toy dataframe
toy_df = pd.DataFrame({'col1': [1, 2, 0], 'col2': [0, 1, 2]})
toy_df = toy_df.applymap(clean_sm)
print("Cleaned toy dataframe:\n", toy_df)


# ***

# ### 3. Create a new dataframe called "ss". The new dataframe should contain a target column called sm_li which should be a binary variable ( that takes the value of 1 if it is 1 and 0 otherwise (use clean_sm to create this) which indicates whether or not the individual uses LinkedIn, and the following features: income (ordered numeric from 1 to 9, above 9 considered missing), education (ordered numeric from 1 to 8, above 8 considered missing), parent (binary), married (binary), female (binary), and age (numeric, above 98 considered missing). Drop any missing values. Perform exploratory analysis to examine how the features are related to the target.

# In[38]:


# Clean and create the target variable `sm_li`
s['sm_li'] = clean_sm(s['web1h'])

# Clean features
s['income'] = np.where(s['income'] <= 9, s['income'], np.nan)
s['education'] = np.where(s['educ2'] <= 8, s['educ2'], np.nan)
s['parent'] = clean_sm(s['par'])
s['married'] = clean_sm(s['marital'] == 1)
s['female'] = clean_sm(s['gender'] == 2)
s['age'] = np.where(s['age'] <= 98, s['age'], np.nan)

# Create new dataframe and drop missing values
features = ['sm_li', 'income', 'education', 'parent', 'married', 'female', 'age']
ss = s[features].dropna()


# In[42]:


# Exploratory analysis
print("Feature summary statistics:\n", ss.describe())
print("Correlation with target (sm_li):\n", ss.corr()['sm_li'])


# In[ ]:


# Relationship between age and LinkedIn usage
plt.figure(figsize=(8, 6))
sns.boxplot(x='sm_li', y='age', data=ss)
plt.title('Age Distribution by LinkedIn Usage')
plt.xlabel('LinkedIn User (0 = No, 1 = Yes)')
plt.ylabel('Age')
plt.show()


# In[ ]:


# Distribution of income levels by LinkedIn usage
plt.figure(figsize=(8, 6))
sns.countplot(x='income', hue='sm_li', data=ss, palette='viridis')
plt.title('Income Levels by LinkedIn Usage')
plt.xlabel('Income Level')
plt.ylabel('Count')
plt.legend(title='LinkedIn User', labels=['No', 'Yes'])
plt.show()


# In[ ]:


# Education levels and LinkedIn usage
plt.figure(figsize=(8, 6))
sns.countplot(x='education', hue='sm_li', data=ss, palette='coolwarm')
plt.title('Education Levels by LinkedIn Usage')
plt.xlabel('Education Level')
plt.ylabel('Count')
plt.legend(title='LinkedIn User', labels=['No', 'Yes'])
plt.show()


# In[ ]:


# Correlation heatmap of features
plt.figure(figsize=(10, 8))
sns.heatmap(ss.corr(), annot=True, cmap='coolwarm', fmt=".2f", cbar=True)
plt.title('Correlation Heatmap')
plt.show()


# In[ ]:


# Age vs Income with LinkedIn usage as hue
plt.figure(figsize=(8, 6))
sns.scatterplot(x='age', y='income', hue='sm_li', data=ss, palette='tab10', alpha=0.8)
plt.title('Age vs Income by LinkedIn Usage')
plt.xlabel('Age')
plt.ylabel('Income Level')
plt.legend(title='LinkedIn User', labels=['No', 'Yes'])
plt.show()


# ***

# #### 4. Create a target vector (y) and feature set (X)

# In[44]:


X = ss[['income', 'education', 'parent', 'married', 'female', 'age']]
y = ss['sm_li']


# ***

# #### 5. Split the data into training and test sets. Hold out 20% of the data for testing. Explain what each new object contains and how it is used in machine learning

# In[46]:


# Split data
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

print("X_train shape:", X_train.shape)
print("X_test shape:", X_test.shape)
print("y_train shape:", y_train.shape)
print("y_test shape:", y_test.shape)


# ***

# #### 6. Instantiate a logistic regression model and set class_weight to balanced. Fit the model with the training data.

# In[48]:


# Logistic Regression model
model = LogisticRegression(class_weight='balanced')
model.fit(X_train, y_train)


# ***

# #### 7. Evaluate the model using the testing data. What is the model accuracy for the model? Use the model to make predictions and then generate a confusion matrix from the model. Interpret the confusion matrix and explain what each number means.
# 

# In[50]:


# Predict and evaluate
y_pred = model.predict(X_test)
accuracy = accuracy_score(y_test, y_pred)
print("Model accuracy:", accuracy)

# Confusion Matrix
conf_matrix = confusion_matrix(y_test, y_pred)
print("Confusion matrix:\n", conf_matrix)


# In[54]:


# Generate confusion matrix
conf_matrix = confusion_matrix(y_test, y_pred)

# Create labeled confusion matrix dataframe
conf_df = pd.DataFrame(
    conf_matrix,
    columns=['Predicted Non-User', 'Predicted User'],
    index=['Actual Non-User', 'Actual User']
)

# Display the labeled confusion matrix
plt.figure(figsize=(6, 5))
sns.heatmap(conf_df, annot=True, fmt="d", cmap="Blues", cbar=False)
plt.title("Confusion Matrix")
plt.ylabel("Actual")
plt.xlabel("Predicted")
plt.show()


# The accuracy of the model is 0.6587301587301587
# 
# Top-left: True Negatives (TN): These are the cases where the model correctly predicted "Non-User," and the actual label was also "Non-User."
# There are 99 correct predictions for non-users.
# 
# Top-right: False Positives (FP): These are the cases where the model predicted "User," but the actual label was "Non-User." The model incorrectly classified 62 non-users as users.
# 
# Bottom-left: False Negatives (FN): These are the cases where the model predicted "Non-User," but the actual label was "User."
# The model missed 24 actual users and classified them as non-users.
# 
# Bottom-right: True Positives (TP):These are the cases where the model correctly predicted "User," and the actual label was also "User."
# There are 67 correct predictions for users.

# ***

# #### 8. Create the confusion matrix as a dataframe and add informative column names and index names that indicate what each quadrant represents

# In[58]:


conf_df = pd.DataFrame(
    conf_matrix,
    columns=['Predicted Non-User', 'Predicted User'],
    index=['Actual Non-User', 'Actual User']
)
print("Labeled Confusion Matrix:\n", conf_df)


# ***

# #### 9. Aside from accuracy, there are three other metrics used to evaluate model performance: precision, recall, and F1 score. Use the results in the confusion matrix to calculate each of these metrics by hand. Discuss each metric and give an actual example of when it might be the preferred metric of evaluation. After calculating the metrics by hand, create a classification_report using sklearn and check to ensure your metrics match those of the classification_report.

# In[60]:


# Metrics calculation
TP = conf_matrix[1, 1]
TN = conf_matrix[0, 0]
FP = conf_matrix[0, 1]
FN = conf_matrix[1, 0]

precision = TP / (TP + FP)
recall = TP / (TP + FN)
f1_score = 2 * (precision * recall) / (precision + recall)

print(f"Precision: {precision}")
print(f"Recall: {recall}")
print(f"F1 Score: {f1_score}")

# Sklearn classification report
print("Classification report:\n", classification_report(y_test, y_pred))


# Precision:
# Precision focuses on the proportion of true positive predictions among all predictions made as positive. In this case, the precision for class 1 (LinkedIn users) is 0.52, which means that when the model predicts someone is a LinkedIn user, it is correct 52% of the time. This metric is particularly important in scenarios where false positives carry a significant cost. For example, in targeted marketing, sending ads to people predicted as LinkedIn users who actually are not could waste resources and irritate the audience. In this case, a higher precision would ensure that marketing efforts are directed only toward actual users, maximizing effectiveness.
# 
# Recall:
# Recall measures the proportion of actual positives correctly identified by the model. Here, the recall for class 1 (LinkedIn users) is 0.74, meaning the model identifies 74% of actual LinkedIn users. Recall is crucial when missing true positives is more costly than including false positives. For instance, if the goal is to ensure all potential LinkedIn users are reached in a campaign, maximizing recall would help minimize missed opportunities, even at the cost of some false positives.
# 
# F1-Score:
# The F1-score is the harmonic mean of precision and recall, balancing the two metrics. In this case, the F1-score for class 1 is 0.61, indicating a moderate balance between the ability to correctly identify LinkedIn users and avoiding false positives. The F1-score is most useful when there is a tradeoff between precision and recall, and both are equally important. For example, in a case where both wasted marketing efforts (false positives) and missed users (false negatives) are costly, the F1-score helps evaluate the overall performance.

# ***

# #### 10. Use the model to make predictions. For instance, what is the probability that a high income (e.g. income=8), with a high level of education (e.g. 7), non-parent who is married female and 42 years old uses LinkedIn? How does the probability change if another person is 82 years old, but otherwise the same?

# In[69]:


# Input example 1: 42 years old, high income, high education, non-parent, married female
example_1 = [[8, 7, 0, 1, 1, 42]]
prob_1 = model.predict_proba(example_1)[:, 1][0]

# Input example 2: 82 years old, same attributes
example_2 = [[8, 7, 0, 1, 1, 82]]
prob_2 = model.predict_proba(example_2)[:, 1][0]

print(f"Probability for example 1 (42 years old): {prob_1}")
print(f"Probability for example 2 (82 years old): {prob_2}")

# Function to classify and return LinkedIn user status and probability
def classify_linkedin_user(model, input_data):
    """
    Predicts LinkedIn user status and probability based on input data.
    """
    prob = model.predict_proba([input_data])[:, 1][0]
    is_user = int(prob > 0.5)  # Classification threshold
    return {'is_user': is_user, 'probability': prob}

# Button to predict
if st.button("Predict LinkedIn Usage"):
    result = classify_linkedin_user(model, input_data)
    user_status = "LinkedIn User" if result['is_user'] == 1 else "Non-LinkedIn User"
    probability = round(result['probability'] * 100, 2)
    
    # Display results
    st.subheader("Prediction Results")
    st.write(f"**User Status**: {user_status}")
    st.write(f"**Probability**: {probability}%")

# ***
