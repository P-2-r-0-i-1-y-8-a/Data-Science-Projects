library(corrplot)
library(ggplot2)
library(tidyr)
library(dplyr)

dataset <- read.csv("C:/Users/priya_/Desktop/Practicum/Project-1/Employee1.csv", sep = ",")


####################### DATA PRE-PROCESSING #####################################

str(dataset)

# Summary statistics
summary(dataset)

#head(dataset)

#names(dataset)

numeric_columns <- select_if(dataset, is.numeric)

names(numeric_columns)

categorical_columns <- select_if(dataset, function(col) is.factor(col) | is.character(col))

names(categorical_columns)


# Check for missing values in the entire dataset
missing_values <- sum(is.na(dataset))
cat("Number of missing values:", missing_values, "\n")

# Check for zero values in each column
zero_values_by_column <- colSums(dataset == 0)
print(zero_values_by_column)

######################## Correlation Matrix ###################################

correlation_matrix <- cor(numeric_columns)

# Create a heatmap using corrplot
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.cex = 0.7, addCoef.col = "black")


# Check the structure of the modified dataset
str(dataset)

###################### Convert Categorical to Factor Variables #########################

dataset$salary <- as.factor(dataset$salary)
dataset$sales <- as.factor(dataset$sales)
str(dataset)


############################# Uni and Bi Variate Analysis ################################

# Plot 1: Promotion in Last 5 Years vs. Left
ggplot(dataset, aes(x = as.factor(promotion_last_5years), fill = as.factor(left))) +
  geom_bar(position = "dodge") +
  labs(title = "Promotion in Last 5 Years vs. Left",
       x = "Promotion in Last 5 Years",
       y = "Count",
       fill = "Left") +
  scale_fill_manual(values = c("1" = "darkblue", "0" = "lightblue")) +
  theme_minimal()

# Plot 2: Department (Sales) vs. Left
ggplot(dataset, aes(x = sales, fill = as.factor(left))) +
  geom_bar(position = "dodge") +
  labs(title = "Department vs. Left",
       x = "Department",
       y = "Count",
       fill = "Left") +
  scale_fill_manual(values =c("1" = "darkblue", "0" = "lightblue")) +
  theme_minimal()


# Calculate the counts and percentages within each 'left' group by department
dataset_percent <- dataset %>%
  group_by(sales, left) %>%
  summarise(Count = n()) %>%
  group_by(sales) %>%
  mutate(Percent = Count / sum(Count) * 100)


# Stacked bar chart for 'left' vs 'promotion_last_5years'
ggplot(dataset, aes(x = as.factor(left), fill = as.factor(promotion_last_5years))) +
  geom_bar(position = "fill") +  # Stacked bar chart with normalized proportions
  scale_x_discrete(labels = c("0" = "Stayed", "1" = "Left")) +
  scale_fill_manual(values = c("lightblue", "darkblue"), labels = c("No Promotion", "Promotion")) +
  labs(title = "Employee Status vs. Promotion in Last 5 Years",
       x = "Employee Left",
       y = "Proportion",
       fill = "Promotion Last 5 Years") +
  theme_minimal()


# Stacked bar chart for 'left' vs 'Work_accident'
ggplot(dataset, aes(x = as.factor(left), fill = as.factor(Work_accident))) +
  geom_bar(position = "fill") +  # Stacked bar chart with normalized proportions
  scale_x_discrete(labels = c("0" = "Stayed", "1" = "Left")) +
  scale_fill_manual(values = c("lightblue", "darkblue"), labels = c("No Accident", "Accident")) +
  labs(title = "Employee Status vs. Work Accident",
       x = "Employee Left",
       y = "Proportion",
       fill = "Work Accident") +
  theme_minimal()


# Satisfaction Level vs Left
ggplot(dataset, aes(x = factor(left), y = satisfaction_level)) +
  geom_boxplot() +
  labs(title = "Satisfaction Level vs Left", x = "Left", y = "Satisfaction Level")

# Last Evaluation vs Left
ggplot(dataset, aes(x = factor(left), y = last_evaluation)) +
  geom_boxplot() +
  labs(title = "Last Evaluation vs Left", x = "Left", y = "Last Evaluation")

# Number of Projects vs Left
ggplot(dataset, aes(x = factor(left), y = number_project)) +
  geom_boxplot() +
  labs(title = "Number of Projects vs Left", x = "Left", y = "Number of Projects")

# Average Monthly Hours vs Left
ggplot(dataset, aes(x = factor(left), y = average_montly_hours)) +
  geom_boxplot() +
  labs(title = "Average Monthly Hours vs Left", x = "Left", y = "Average Monthly Hours")

# Time Spend at Company vs Left
ggplot(dataset, aes(x = factor(left), y = time_spend_company)) +
  geom_boxplot() +
  labs(title = "Time Spend at Company vs Left", x = "Left", y = "Time Spend at Company")


dataset_bins <- dataset

# Create 3 bins with the actual ranges as labels
dataset_bins$evaluation_bins <- cut(dataset_bins$last_evaluation, breaks = 3)

# Optionally, to see the distribution of the bins
table(dataset_bins$evaluation_bins)


# Plot: Left vs. Evaluation Bins
ggplot(dataset_bins, aes(x = evaluation_bins, fill = as.factor(left))) +
  geom_bar(position = "dodge") +
  labs(title = "Left vs. Evaluation Bins",
       x = "Evaluation Bins",
       y = "Count",
       fill = "Left") +
  scale_fill_manual(values = c("1" = "darkblue", "0" = "lightblue")) +
  theme_minimal()

######################## Creating Buckets ###############################



dataset_bins$time_spend_company_bucket <- cut(
  dataset_bins$time_spend_company,
  breaks = c(2, 3, 4.5, 6, Inf),  # 'Inf' captures all values greater than 6
  labels = c("2-3 years", "3.1-4.5 years", "4.6-6 years", ">6 years"),
  right = TRUE,  # Intervals include the right boundary
  include.lowest = TRUE  # Includes the lowest value in the first interval
)


# View the new column to check the distribution
table(dataset_bins$time_spend_company_bucket)


# Create a new column 'number_project_bucket' with 2 buckets
dataset_bins$number_project_bucket <- cut(
  dataset_bins$number_project,
  breaks = c(2, 4, 7), 
  labels = c("2-4 projects", "5-7 projects"),
  right = TRUE,  # Intervals include the right boundary
  include.lowest = TRUE  # Includes the lowest value in the first interval
)

# View the new column to check the distribution
table(dataset_bins$number_project_bucket)


# Create a new column 'last_evaluation_bucket' with 3 buckets for bell curve distribution
dataset_bins$last_evaluation_bucket <- cut(
  dataset_bins$last_evaluation,
  breaks = c(0.36, 0.58, 0.83, 1), 
  labels = c("Low", "Medium", "High"), #range "0.36-0.58", "0.59-0.83", "0.84-1"
  right = TRUE,  # Intervals include the right boundary
  include.lowest = TRUE  # Includes the lowest value in the first interval
)

# View the new column to check the distribution
table(dataset_bins$last_evaluation_bucket)


# Create a new column 'satisfaction_level_bucket' with 3 buckets for bell curve distribution
dataset_bins$satisfaction_level_bucket <- cut(
  dataset_bins$satisfaction_level,
  breaks = c(0.09, 0.4, 0.8, 1), 
  labels = c("Low", "Medium", "High"), #range is "0.09-0.4", "0.41-0.8", "0.81-1"
  right = TRUE,  # Intervals include the right boundary
  include.lowest = TRUE  # Includes the lowest value in the first interval
)

# View the new column to check the distribution
table(dataset_bins$satisfaction_level_bucket)


############################### Churn Employee Analysis ####################################

# Filter the dataset for only those employees who have left the company
filtered_dataset <- subset(dataset, left == 1)# & promotion_last_5years == 1#)

# View the first few rows of the filtered dataset to confirm
head(filtered_dataset)

summary(filtered_dataset)

count(filtered_dataset)

# Filter the dataset for only those employees who have left the company
filtered_dataset0 <- subset(dataset, left == 0)# & promotion_last_5years == 1#)

# View the first few rows of the filtered dataset to confirm
head(filtered_dataset0)

summary(filtered_dataset0)
count(filtered_dataset0)

# Count the number of employees who have left (filtered_dataset) and total employees (dataset)
left_count <- nrow(filtered_dataset)  # Number of employees who have left
total_count <- nrow(dataset)  # Total number of employees

# Calculate the percentage
left_percentage <- (left_count / total_count) * 100

# Print the percentage
left_percentage

##################################### HYPOTHESIS TEST ##########################################


# Assuming salary is categorical, let's plot the frequency of each salary level
ggplot(filtered_dataset, aes(x = salary)) +
  geom_bar(fill = "skyblue", color = "black") +  # Bar plot with filled bars
  labs(title = "Distribution of Salary Levels on Churned Data", 
       x = "Salary Level", 
       y = "Count") +
  theme_minimal()  # Apply a minimal theme for clean visuals


# Plot the distribution of Promotion in the Last 5 Years
ggplot(filtered_dataset, aes(x = as.factor(promotion_last_5years))) +
  geom_bar(fill = "skyblue", color = "black") +  # Bar plot with green bars
  labs(title = "Promotion in the Last 5 Years", 
       x = "Promotion (0 = No, 1 = Yes)", 
       y = "Count") +
  theme_minimal()  # Apply a minimal theme for clean visuals


# Load the ggplot2 library
library(ggplot2)

# Plot the density of Average Monthly Hours
ggplot(filtered_dataset, aes(x = average_montly_hours)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Density Plot of Average Monthly Hours", 
       x = "Average Monthly Hours", 
       y = "Density") +
  theme_minimal()  # Apply a minimal theme for clean visuals

summary(dataset)
# View the first few rows of the filtered dataset to confirm
#head(filtered_dataset_potential)


# Plot the distribution of Work Accidents
ggplot(filtered_dataset, aes(x = as.factor(Work_accident))) +
  geom_bar(fill = "skyblue", color = "black") +  # Bar plot with salmon-colored bars
  labs(title = "Distribution of Work Accidents", 
       x = "Work Accident (0 = No, 1 = Yes)", 
       y = "Count") +
  theme_minimal()  # Apply a minimal theme for clean visuals


# Plot the bar plot of Number of Projects
ggplot(filtered_dataset, aes(x = as.factor(number_project))) +
  geom_bar(fill = "skyblue", color = "black") +  # Bar plot with light coral bars
  labs(title = "Bar Plot of Number of Projects", 
       x = "Number of Projects", 
       y = "Count") +
  theme_minimal()  # Apply a minimal theme for clean visuals


#count(filtered_dataset_potential_lo)
#count(filtered_dataset_potential_hig)
#head(filtered_dataset_potential_lo)


##### The third hypothesis is: this company is a good place to grow professionally.

# The employees who were woking more than an average hours and more than average of number_project
filtered_dataset <- subset(dataset_bins, left == 1 & average_montly_hours >201)

filtered_dataset1 <- subset(dataset_bins, left == 1 )

#count(filtered_dataset1)

str(dataset_bins$satisfaction_level_bucket)

# Filter the dataset based on the specified criteria
filtered_dataset_potenitial <- subset(filtered_dataset, 
                                      # time_spend_company >= 3 & #
                                      number_project >= 4 & 
                                        promotion_last_5years == 0 &
                                        last_evaluation_bucket %in% c("High", "Medium") &  # Greater than medium evaluation (High bucket)
                                        satisfaction_level_bucket == "Low")  # Satisfaction level in the Low bucket


count(filtered_dataset_potenitial)
count(filtered_dataset1)



# Create a summary table
summary_table <- data.frame(
  Dataset = c("Filtered Dataset (Potential)", "Filtered Dataset 1"),
  Count = c(nrow(filtered_dataset_potenitial), nrow(filtered_dataset1))
)

# Print the summary table
print(summary_table)


# Load ggplot2 if not already loaded
library(ggplot2)

ggplot(summary_table, aes(x = Dataset, y = Count, fill = Dataset)) +
  geom_col(fill = "skyblue", color = "black")  +  # Use geom_col instead of geom_bar
  theme_minimal() +
  labs(title = "Count of Records in Each Dataset", 
       x = "Dataset", 
       y = "Count of Records") +
  scale_x_discrete(labels = c("Filtered Dataset (Potential)" = "Not Good professional", 
                              "Filtered Dataset 1" = "Left")) +  # Custom x-axis labels
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 360, hjust = 1))  # Rotate x-axis labels if needed



# Filter the dataset based on the conditions for average_montly_hours and salary
#filtered_dataset_potential_lo <- subset(filtered_dataset, 
#                                       average_montly_hours > 261.2 & 
#                                        salary == "low")

#count(filtered_dataset_potential_lo)

#filtered_dataset_potential_hig <- subset(filtered_dataset, 
#                                       average_montly_hours > 261.2 & 
#                                        salary == "high")

#count(filtered_dataset_potential_hig)


########## Chi Square for categorical Variables #######################

# Assuming your dataset is stored in df
# Create a contingency table
contingency_table <- table(dataset$salary, dataset$left)

# Perform the Chi-Square test# Perform the datasetChi-Square test
chisq_test <- chisq.test(contingency_table)

# Output the results
chisq_test


# Assuming your dataset is stored in df
# Create a contingency table
contingency_table1 <- table(dataset$Work_accident, dataset$left)

# Perform the Chi-Square test# Perform the datasetChi-Square test
chisq_test1 <- chisq.test(contingency_table1)

# Output the results
chisq_test1


################## One Hot Encoding ########################################

# Load necessary libraries
library(purrr)

# Define categorical columns for one-hot encoding
categorical_columns <- c("sales", "salary")

# Function to perform one-hot encoding
one_hot_encode <- function(df, column) {
  model_matrix <- model.matrix(~ . - 1, data = df[, column, drop = FALSE])
  as.data.frame(model_matrix)
}

# Apply one-hot encoding to the specified columns
encoded_columns <- map(categorical_columns, ~ one_hot_encode(dataset, .x))

# Combine one-hot encoded columns into a single dataframe
encoded_df <- bind_cols(encoded_columns)

# Manually exclude the original categorical columns
dataset_excluded <- dataset[ , !(names(dataset) %in% categorical_columns)]

# Combine the original dataframe (excluding categorical columns) with the encoded columns
dataset_encoded <- bind_cols(dataset_excluded, encoded_df)
str(dataset_encoded)


####################### LASSO ####################

# Load necessary libraries
library(glmnet)
library(ggplot2)

# Prepare the data for Lasso
X_balanced <- dataset[, -which(names(dataset) == "left")]  # Predictor matrix
Y_balanced <- dataset$left  # Target variable

# Convert factors to numeric if necessary
X_balanced <- as.data.frame(lapply(X_balanced, function(x) if(is.factor(x)) as.numeric(x) else x))

# Fit the Lasso model with cross-validation
lasso_model_balanced <- cv.glmnet(as.matrix(X_balanced), Y_balanced, alpha = 1, family = "binomial")

# Get the best lambda value
best_lambda_balanced <- lasso_model_balanced$lambda.min

# Get the coefficients of the best model
lasso_coefficients_balanced <- coef(lasso_model_balanced, s = best_lambda_balanced)


# Convert the coefficients to a data frame
lasso_coefficients_df <- as.data.frame(as.matrix(lasso_coefficients_balanced))
lasso_coefficients_df$Feature <- rownames(lasso_coefficients_df)
colnames(lasso_coefficients_df) <- c("Coefficient", "Feature")
lasso_coefficients_df <- lasso_coefficients_df[lasso_coefficients_df$Coefficient != 0, ]

# Plot the coefficients
ggplot(lasso_coefficients_df, aes(x = reorder(Feature, Coefficient), y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Lasso Model Coefficients",
       x = "Feature",
       y = "Coefficient Value") +
  scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "salmon"), 
                    name = "Coefficient Sign",
                    labels = c("Negative", "Positive")) +
  theme_minimal()


summary(dataset)

# Load necessary library

################################### partioning #########################################

library(caret)
# Set seed for reproducibility
set.seed(123)

train_index <- createDataPartition(dataset$left, p = 0.6, list = FALSE)
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]
dim(train_data)
dim(test_data)

dim(dataset)


############################### Modelling ###########################################

############# Logistic Regression ###############


# Load necessary library
library(glmnet)

set.seed(2024)
# Train a logistic regression model
logistic_model <- glm(left ~ ., data = train_data, family = "binomial")
summary(logistic_model)
# Make predictions on the test set
logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Convert probabilities to class labels (positive class = 1)
logistic_class <- factor(ifelse(logistic_predictions > 0.5, 1, 0), levels = c(1, 0))

#logistic_pred <- factor(logistic_predictions$class, levels = c("1", "0"))
test_pred <- factor(test_data$left, levels = c("1", "0"))

####### Evaluating model performance 
# Evaluate the model with the positive class set to 1
confusionMatrix(logistic_class, test_pred, positive = "1")



################### DECISION TREE #########################

library(rpart)
library(rpart.plot)

set.seed(2024)
model <- rpart(left ~ . , data = train_data, method = "class")
summary(model)

rpart.plot(model, digits=4, cex=0.7)
print(model)

####### Evaluating model performance 

tree_pred <- predict(model, newdata= test_data)  
tree_pred <- ifelse(tree_pred[, 2] > 0.5, 1, 0)
tree_pred <- factor(tree_pred, levels = c("1", "0"))  
test_pred <- factor(test_data$left, levels = c("1", "0")) 

# Recreate the confusion matrix
confusionMatrix(tree_pred, test_pred, positive = "1")


######################### AUROC CURVE ##########################

library(pROC)

# Create ROC curves for each model
roc_logistic <- roc(test_data$left, logistic_predictions)
tree_pred_probs <- predict(model, test_data, type = "prob")[,2]
roc_tree <- roc(test_data$left, tree_pred_probs)

# Plot the ROC curve for Decision Tree first
plot(roc_tree, col="darkblue", lwd=2, main="ROC Curves for Decision Tree and Logistic Regression",
     xlab="1 - Specificity", ylab="Sensitivity", xlim=c(1, 0), ylim=c(0, 1))

# Add shaded area under the Decision Tree curve
polygon(c(rev(roc_tree$specificities), 1), c(rev(roc_tree$sensitivities), 0), col=rgb(0,0,1,0.2), border=NA)

# Add Logistic Regression curve
lines(roc_logistic, col="green", lwd=2)

# Add shaded area under the Logistic Regression curve
polygon(c(rev(roc_logistic$specificities), 1), c(rev(roc_logistic$sensitivities), 0), col=rgb(1,0.5,0,0.2), border=NA)

# Add AUC values to the plot
text(0.3, 0.3, paste("Tree AUC:", round(auc(roc_tree), 3)), col="darkblue", cex=1.0)
text(0.3, 0.2, paste("Logistic AUC:", round(auc(roc_logistic), 3)), col="green", cex=1.0)

# Add legend
legend("bottomright", legend = c("Decision Tree", "Logistic Regression"),
       col = c("darkblue", "green"), lwd=2, cex=0.8)



### PROFIT ANALYSIS #######


# Add a new column classifying TP, TN, FP, FN
test_data$classification <- ifelse(tree_pred == 1 & test_data$left == 1, "TP",
                                   ifelse(tree_pred == 0 & test_data$left == 0, "TN",
                                          ifelse(tree_pred == 1 & test_data$left == 0, "FP", "FN")))

# No need to create a new column for salary; just use test_data$salary
# Group data by classification and salary group
salary_groups <- table(test_data$classification, test_data$salary)

# View the results
print(salary_groups)

