NPL.df <- read.csv("North-Point List.csv")
library(ggplot2)
library(gridExtra)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(gains)

#Check for missing value
missing_values <-is.na(NPL.df)
col_missing_count <- colSums(missing_values)
print(col_missing_count)

# Check for zero values 
non_zero_purchase_zero_spending <- with(NPL.df , Purchase !=0 & Spending == 0)
count_non_zero_purchase_zero_spending <- sum(non_zero_purchase_zero_spending)
print(count_non_zero_purchase_zero_spending)

# Categorical Variables_Histogram
categorical_columns <- c('US', 'source_a', 'source_c', 'source_b', 'source_d', 'source_e',
                         'source_m', 'source_o', 'source_h', 'source_r', 'source_s', 'source_t',
                         'source_u', 'source_p', 'source_x', 'source_w', 'Web.order', 'Gender.male',
                         'Address_is_res', 'Purchase')
theme_set(theme_minimal())
plot_histogram <- function(col) {
  ggplot(NPL.df, aes(x = as.factor(.data[[col]]))) +
    geom_bar(fill = "yellow", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram -", col)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
plots <- lapply(categorical_columns, function(col) {
  if (length(unique(NPL.df[[col]])) > 1) {
    plot_histogram(col)
  } else {
    ggplot() + ggtitle(paste("Histogram -", col)) +
      theme_minimal()
  }
})
grid.arrange(grobs = plots, ncol = 4)

# Categorical Variables_Sources
source_columns <- NPL.df[, c("source_a", "source_c", "source_b", "source_d", "source_e", "source_m", "source_o", "source_h", "source_r", "source_s", "source_t", "source_u", "source_p", "source_x", "source_w")]
source_counts <- colSums(source_columns == 1)
print(source_counts)
source_columns <- NPL.df[, c("source_a", "source_c", "source_b", "source_d", "source_e", "source_m", "source_o", "source_h", "source_r", "source_s", "source_t", "source_u", "source_p", "source_x", "source_w")]
source_counts <- colSums(source_columns == 1)
source_counts_df <- data.frame(Source = names(source_counts), Count = source_counts)
histogram <- ggplot(source_counts_df, aes(x = Source, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Counts of Occurrences of '1' for Each Source Column",
       x = "Source", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(histogram)

#Categorical Variables_Relation between Web.Order and Purchase

filtered_data <- subset(NPL.df, Purchase == 1)
percentage_data <- data.frame(table(filtered_data$Web.order) / nrow(filtered_data) * 100)
ggplot(percentage_data, aes(x = Var1, y = Freq, fill = as.factor(Var1))) +
  geom_bar(stat = "identity", position = "dodge", fill = c("pink", "purple")) +  # Specify fill colors here
  geom_text(aes(label = sprintf("%.1f%%", Freq)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Web Order when Purchase is 1",
       x = "Web Order", y = "Percentage", fill = "Web Order") +
  theme_minimal()


# Numerical_Histogram
NPL.df$days_difference <- NPL.df$X1st_update_days_ago - NPL.df$last_update_days_ago
plot_spending <- ggplot(NPL.df, aes(x = Spending, fill = ..count..)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  ylab("Count") +
  scale_fill_gradient("Count", low = "red", high = "purple") +
  theme_minimal()

plot_freq <- ggplot(NPL.df, aes(x = Freq, fill = ..count..)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  ylab("Count") +
  scale_fill_gradient("Count", low = "red", high = "purple") +
  theme_minimal()

plot_difference <- ggplot(NPL.df, aes(x = days_difference, fill = ..count..)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  ylab("Count") +
  scale_fill_gradient("Count", low = "red", high = "purple") +
  theme_minimal()
grid.arrange(plot_spending, plot_freq, plot_difference, ncol = 3)

#Histogram for purchase = 1 Observation
purchased_data <- NPL.df %>% filter(Purchase == 1)
purchased_data$days_difference <- purchased_data$X1st_update_days_ago - purchased_data$last_update_days_ago
plot_spending <- ggplot(purchased_data, aes(x = Spending, fill = ..count..)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  ylab("Count") +
  scale_fill_gradient("Count", low = "pink", high = "purple") +
  theme_minimal()

plot_freq <- ggplot(purchased_data, aes(x = Freq, fill = ..count..)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  ylab("Count") +
  scale_fill_gradient("Count", low = "pink", high = "purple") +
  theme_minimal()

plot_difference <- ggplot(purchased_data, aes(x = days_difference, fill = ..count..)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  ylab("Count") +
  scale_fill_gradient("Count", low = "pink", high = "purple") +
  theme_minimal()

# Arrange plots in one space
grid.arrange(plot_spending, plot_freq, plot_difference, ncol = 3)


# Scatter plot - Spending and Purchase
ggplot(NPL.df, aes(x = Spending, y = Purchase)) +
  geom_point(aes(color = factor(Purchase))) +
  labs(title = "Scatter Plot of Amount Spent and Purchase",
       x = "Amount Spent",
       y = "Purchase",
       color = "Purchase Status") +
  scale_color_manual(values = c("red", "green"), labels = c("Non-Purchase", "Purchase")) +
  theme_minimal()

#Data Transformation
NPL.df <- NPL.df[, -1]  # Remove the first column (sequence_number) from the dataset
NPL.df$days_difference <- NPL.df$X1st_update_days_ago - NPL.df$last_update_days_ago
colnames(NPL.df) <- gsub("Gender.male", "Gender is male", colnames(NPL.df))
colnames(NPL.df) <- gsub("Web.order", "Web_order", colnames(NPL.df))
colnames(NPL.df) <- gsub("last_update_days_ago", "Last_Interaction", colnames(NPL.df))
colnames(NPL.df) <- gsub("X1st_update_days_ago", "First_Interaction", colnames(NPL.df))
colnames(NPL.df) <- gsub("Spending", "Amount_Spent", colnames(NPL.df))
print(NPL.df)
write.csv(NPL.df, "North_Point.csv", row.names = FALSE)

## Model Building 

NP.df <- read.csv("North_Point.csv")

## Logistic Regression
set.seed(2024)
indices <- sample(1:2000)
train_data <- NP.df[indices[1:800], ]
validation_data <- NP.df[indices[801:1500], ]
holdout_data <- NP.df[indices[1501:2000], ]
dim(train_data)
dim(holdout_data)
dim(validation_data)
# Specify the model formula
formula <- as.formula("Purchase ~ source_a + source_c + source_b + source_d + source_e + source_m + source_o + source_h + source_r + source_s + source_t + source_u + source_p + source_x + source_w + Gender.is.male + Web_order + Freq + Address_is_res + days_difference")
# Fit the logistic regression model on the training data
logistic_model <- glm(formula, data = train_data, family = "binomial")
# Predict purchase probability using logistic regression
validation_data$predicted_purchase <- predict(logistic_model, newdata = validation_data, type = "response")
validation_data$predicted_purchase <- ifelse(as.numeric(validation_data$predicted_purchase) >= 0.5, "1", "0")
# Ensuring that Purchase and predicted_purchase have the same levels
validation_data$Purchase <- factor(validation_data$Purchase, levels = c("1","0"))
validation_data$predicted_purchase <- factor(validation_data$predicted_purchase, levels = c("1","0"))
# Create confusion matrix
conf_matrix <- confusionMatrix(validation_data$Purchase, validation_data$predicted_purchase, positive = "1")
# View the confusion matrix
conf_matrix
# Perform stepwise backward variable selection using AIC
backward_model <- step(logistic_model, direction = "backward", trace = 0)
# Display the selected model
summary(backward_model)
# Fit logistic regression model with stepwise variable selection
formula_Bic <- as.formula("Purchase ~ source_a + source_h + source_r + source_u + source_w + Web_order + Freq + Address_is_res")
logistic_model_Stepwise <- glm(formula_Bic, data = train_data, family = "binomial")
validation_data$predicted_purchase_stepwise <- predict(logistic_model_Stepwise, newdata = validation_data, type = "response")
validation_data$predicted_purchase_stepwise <- as.factor(ifelse(validation_data$predicted_purchase_stepwise >= 0.5, 1, 0))
# Ensuring that Purchase and predicted_purchase_stepwise have the same levels
validation_data$Purchase <- as.factor(validation_data$Purchase)
validation_data$Purchase <- factor(validation_data$Purchase, levels = c("1","0"))
validation_data$predicted_purchase_stepwise <- factor(validation_data$predicted_purchase_stepwise, levels = c("1","0"))
# Calculate confusion matrix
conf_matrix1 <- confusionMatrix(validation_data$Purchase, validation_data$predicted_purchase_stepwise, positive = "1")
conf_matrix1

### Classification Tree

# Specify the model formula
formula_tree <- as.formula("Purchase ~ source_a + source_h + source_r + source_u + source_w + Web_order + Freq + Address_is_res")
# Build the classification tree model
tree_model <- rpart(formula_tree, data = train_data, method = "class", control = rpart.control(minsplit = 5, cp = 0.01))
# Plot the tree
rpart.plot(tree_model, main = "Classification Tree")
# Make predictions on the validation set
validation_data$predicted_purchase <- predict(tree_model, newdata = validation_data, type = "class")
validation_data$Purchase <- factor(validation_data$Purchase, levels = c(1, 0))
validation_data$predicted_purchase <- factor(validation_data$predicted_purchase, levels = c(1, 0))

# Calculate confusion matrix on the validation set
conf_matrix_tree <- confusionMatrix(validation_data$Purchase, validation_data$predicted_purchase, positive = "1")
conf_matrix_tree

##KNN
# Assuming 'train_data' is your training set
# Specify the model formula
formula_knn <- as.formula("Purchase ~ source_a + source_h + source_r + source_u + source_w + Web_order + Freq + Address_is_res")
X_train <- model.matrix(formula_knn, data = train_data)[,-1]  # Exclude intercept column
X_valid <- model.matrix(formula_knn, data = validation_data)[,-1]
k_neighbors <- 7
library(class)
# Building the KNN model
knn_model <- knn(train = X_train, test = X_valid, cl = train_data$Purchase, k = k_neighbors)
validation_data$Purchase<- factor(validation_data$Purchase, levels = c("1","0"))
validation_data$predicted_purchase  <- factor(validation_data$predicted_purchase , levels = c("1","0"))
# Calculate confusion matrix on the validation set
conf_matrix_knn <- confusionMatrix(validation_data$Purchase, validation_data$predicted_purchase, positive = "1")
conf_matrix_knn

#Evaluation for Holdout  accurary of logistic regression 
formula_Bic <- as.formula("Purchase ~ source_a + source_h + source_r + source_u + source_w + Web_order + Freq + Address_is_res")
logistic_model_Stepwise <- glm(formula_Bic, data = holdout_data, family = "binomial")
holdout_data$predicted_purchase_binary <- predict(logistic_model_Stepwise, newdata = holdout_data, type = "response")
holdout_data$predicted_purchase_binary <- as.factor(ifelse(holdout_data$predicted_purchase_binary >= 0.5, 1, 0))
holdout_data$Purchase <- as.factor(holdout_data$Purchase)
holdout_data$predicted_purchase_binary <- as.factor(holdout_data$predicted_purchase_binary)
holdout_data$Purchase<- factor(holdout_data$Purchase, levels = c("1","0"))
holdout_data$predicted_purchase_binary  <- factor(holdout_data$predicted_purchase_binary , levels = c("1","0"))
# Calculate confusion matrix on the test set
conf_matrix_test <- confusionMatrix(holdout_data$Purchase, holdout_data$predicted_purchase_binary, positive = "1")
conf_matrix_test

# Goal 2 

##Linear Regression
# Read the dataset
NP.df <- read.csv("North_Point.csv")
set.seed(2024)
selected_features <- c('source_a', 'source_c', 'source_b', 'source_d', 'source_e', 'source_m', 'source_o', 'source_h', 'source_r', 'source_s', 'source_t', 'source_u', 'source_p', 'source_x', 'source_w', 'Gender.is.male', 'Web_order', 'Freq', 'First_Interaction', 'Address_is_res')
# Generate random indices for data partitioning
indices <- sample(which(NP.df$Purchase == 1))
# Define the proportions for each partition
train_prop <- 0.4
validation_prop <- 0.35
holdout_prop <- 0.25
# Calculate the number of rows for each partition
train_size <- floor(train_prop * length(indices))
validation_size <- floor(validation_prop * length(indices))
holdout_size <- length(indices) - train_size - validation_size
# Create partitions
train_indices <- indices[1:train_size]
validation_indices <- indices[(train_size + 1):(train_size + validation_size)]
holdout_indices <- indices[(train_size + validation_size + 1):length(indices)]
train_data_lm <- NP.df[train_indices, ]
validation_data_lm <- NP.df[validation_indices, ]
holdout_data_lm <- NP.df[holdout_indices, ]
# Display the dimensions of each subset
dim(train_data_lm)
dim(validation_data_lm)
dim(holdout_data_lm)
# Define the independent variables (X) and the dependent variable (y) for training set
X_train <- train_data_lm[, selected_features]
y_train <- train_data_lm$Amount_Spent
# Define the independent variables (X) and the dependent variable (y) for validation set
X_validation <- validation_data_lm[, selected_features]
y_validation <- validation_data_lm$Amount_Spent
# Create and fit the linear regression model using the training set
model <- lm(y_train ~ ., data = cbind(y_train, X_train))
y_pred_validation <- predict(model, newdata = cbind(X_validation))
# Calculate Mean Absolute Error (MAE) on the validation set
mae_validation <- mean(abs(y_pred_validation - y_validation))
print(paste("Validation MAE:", mae_validation))
# Evaluate the model on the validation set (you can use any appropriate metric)
validation_rmse <- sqrt(mean((y_pred_validation - y_validation)^2))
print(paste("Validation RMSE:", validation_rmse))
# Define the dependent variable
dependent_variable <- "Amount_Spent"
selected_features1 <- c('source_h', 'Freq', 'First_Interaction', 'Address_is_res')
# Create a formula with all potential predictors
initial_formula <- as.formula(paste(dependent_variable, "~", paste(selected_features, collapse = " + ")))
# Perform stepwise forward selection
stepwise_model <- step(lm(initial_formula, data = NP.df), direction = "backward", trace = 0)
# Display the selected model
summary(stepwise_model)
X_train1 <- train_data_lm[, selected_features1]
y_train1 <- train_data_lm$Amount_Spent
model <- lm(y_train1 ~ ., data = cbind(y_train1, X_train1))
X_validation1 <- validation_data_lm[, selected_features1]
y_validation1 <- validation_data_lm$Amount_Spent
y_pred_validation1 <- predict(model, newdata = cbind(X_validation1))
# Calculate Mean Absolute Error (MAE) on the validation set
mae_validation1 <- mean(abs(y_pred_validation1 - y_validation1))
print(paste("Validation MAE:", mae_validation1))


##Regression tree
# Fit the regression tree
tree_model <- rpart(Amount_Spent ~ ., data = NP.df, method = "anova")
# Plot the regression tree
rpart.plot(tree_model)
# Predict the Amount_Spent using the regression tree model
predicted_amount_spent <- predict(tree_model, newdata = holdout_data_lm)
# Calculate the Mean Absolute Error (MAE)
mae_tree <- mean(abs(predicted_amount_spent - holdout_data_lm$Amount_Spent))
print(paste("Regression Tree MAE:", mae_tree))

## Holdout testing for Selected Model for Goal 2
# Make predictions on the holdout set
X_holdout <- holdout_data[, selected_features]
y_pred_holdout <- predict(model, newdata = cbind(X_holdout))
# Calculate Mean Absolute Error (MAE) on the holdout set
mae_holdout <- mean(abs(y_pred_holdout - holdout_data$Amount_Spent))
print(paste("Holdout MAE:", mae_holdout))

####
# Gross Profit
purchasers <- NP.df$Amount_Spent[NP.df$Purchase == 1]
purchasers_numeric <- purchasers[!is.na(purchasers) & is.numeric(purchasers)]
mean_spending_purchase <- mean(purchasers_numeric)
remaining_customers <- 180000
markup_percentage <- 0.053
mailing_cost_per_booklet <- 2
gross_profit <- remaining_customers * (markup_percentage * mean_spending_purchase - mailing_cost_per_booklet)
gross_profit

# Adding Columns acc to Business requirement 
dim(holdout_data)
holdout_data <- holdout_data[, !(names(holdout_data) == "predicted_purchase_binary")]
# Predict purchase probability using the Purchase.glm model
holdout_data$predicted <- predict(logistic_model_Stepwise, holdout_data, type = 'response')
head(holdout_data)
# Adjust predicted purchase probability by multiplying by 0.1065
holdout_data$AdjustedProbPurchase <- holdout_data$predicted * 0.1065
head(holdout_data)
# Pred_Spending
holdout_purchasers <- holdout_data
holdout_purchasers$pred_spend <- predict(model, newdata = holdout_data)
head(holdout_purchasers)
# Calculate expected spending by multiplying predicted spending with adjusted purchase probability
holdout_purchasers$expectedSpending <- holdout_purchasers$pred_spend * holdout_purchasers$AdjustedProbPurchase
# Display column names of holdout_purchasers
names(holdout_purchasers)
# Calculate the sum of expected spending
sum(holdout_purchasers$expectedSpending)

## Gain Chart
# load package gains, compute gains (we will use package caret for categorical y later)
library(gains)
gain <- gains(holdout_purchasers$Amount_Spent, holdout_purchasers$expectedSpending)
df <- data.frame(
  ncases=c(0, gain$cume.obs),
  cumSpending=c(0, gain$cume.pct.of.total * sum(holdout_purchasers$expectedSpending))
)
ggplot(df, aes(x=ncases, y=cumSpending)) +
  geom_line() +
  geom_line(data=data.frame(ncases=c(0, nrow(holdout_purchasers)), cumSpending=c(0, sum(holdout_purchasers$expectedSpending))),
            color="blue", linetype=2) + # adds baseline
  labs(x="Observation ", y="Cumulative Expected Spending", title="Cumulative gains chart") +
  scale_y_continuous(labels = scales::comma)


