# Import libraries
library(corrplot)
library(ggplot2)
library(dplyr)
library(caret)
library(ROCR)
library(randomForest)
library(car)
library(olsrr)
library(lmtest)
#Import file
data <- read.csv("C:/Users/Admin/Desktop/ad_click_dataset.csv")
head(data)
# Replace empty strings with NA
data[data== ""] <- NA
str(data)
# Count occurrences of blanks and NA in each column
na_count <- sapply(data, function(x) sum(is.na(x)))
# Print the count of missing values per column
print(na_count)
cleaned_data <- data

#Summarize each user’s total records and total clicks
user_summary <- cleaned_data %>% group_by(full_name) %>% summarise(total_records = n(), 
                                                                   total_clicks = sum(click))
user_summary <- user_summary %>% mutate(user_category = case_when(total_records == 1 ~ "first-time", 
                                                                  total_records > 1 ~ "recurring", 
                                                                  TRUE ~ "other"))
cat("Total Unique Users:", nrow(user_summary), "\n")

first_time_count <- sum(user_summary$user_category == "first-time") 
recurring_count <- sum(user_summary$user_category == "recurring")
cat("Number of first-time users:", first_time_count, "\n")
cat("Number of recurring users :", recurring_count, "\n")

#Define a helper function to get the mode of a vector
get_mode <- function(x) {
  x <- x[!is.na(x)]  # Remove NA
  if (length(x) == 0) return(NA)  # Return NA if all values are NA
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
# Fill NA values in categorical columns with the mode of each user
cleaned_data <- cleaned_data %>% 
  group_by(full_name) %>% 
  mutate(age = ifelse(is.na(age), median(age, na.rm = TRUE), age), # For numeric column, use the median age of each user
         # For categorical columns, use the per-user mode:
         gender = ifelse(is.na(gender), get_mode(gender), gender),
         device_type = ifelse(is.na(device_type), get_mode(device_type), device_type),
         ad_position = ifelse(is.na(ad_position), get_mode(ad_position), ad_position),
         browsing_history = ifelse(is.na(browsing_history), get_mode(browsing_history), browsing_history),
         time_of_day = ifelse(is.na(time_of_day), get_mode(time_of_day), time_of_day)
  ) %>%
  ungroup()
# Fill NA values in age column with -1
cleaned_data$age[is.na(cleaned_data$age)] <- -1
# Fill other NA with unknown
categorical_cols <- c("gender", "device_type", "ad_position", "time_of_day", "browsing_history") 
for (col in categorical_cols) { 
  cleaned_data[[col]][is.na(cleaned_data[[col]])] <- "Unknown" 
}
# Check for missing values
na_count <- sapply(cleaned_data, function(x) sum(is.na(x)))
print(na_count)

# drop full_name and id column
cleaned_data <- cleaned_data[, c("age","gender","device_type","ad_position",
                                 "browsing_history","time_of_day","click")]
# Recheck our data by displaying to the console
str(cleaned_data)
# Basic information about our refined data
summary(cleaned_data)
summary(cleaned_data %>% filter(age != -1)) # Without -1 age


# Frequency Table for categorical variables:
apply(cleaned_data[c("gender","device_type","ad_position",
                     "browsing_history","time_of_day","click")],2,table)

#Basic histogram of "age"
ggplot(cleaned_data %>% filter(age != -1), aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Age", x = "Age", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Distribution of age by click
ggplot(cleaned_data %>% filter(age != -1), aes(x = age, fill = factor(click))) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 5, color = "black") +
  scale_fill_manual(values = c("red", "skyblue"),
                    name = "Click",
                    labels = c("No Click", "Click")) +
  labs(title = "Histogram of Age by Click Behavior", x = "Age", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Barplot & set up grid for more plots
par(mfrow = c(3, 2), mar = c(2, 2, 2, 2) + 0.1) # Reduced margins
categorical<-c("gender","device_type","ad_position","browsing_history","time_of_day","click")
for (var in categorical){
  barplot(table(cleaned_data[[var]]), 
          main = paste("Barplot of", var), 
          col = "skyblue", 
          border = "black", 
          cex.names=0.6)
}
# reset plotting layout to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1) 
#Pie chart & set up grid for more plots
par(mfrow = c(3, 2), mar = c(1, 2, 2, 2) + 0.1) # Reduced margins
for (var in categorical){
  counts <- table(cleaned_data[[var]])
  percents <- round(100 * counts / sum(counts), 1)  # Calculate percentages
  labels <- paste0(names(counts), "\n", percents, "%")  # Create labels with percent
  pie(counts, 
      labels=labels,
      main = paste("Pie chart of", var),
      col = "skyblue", border = "black",
      radius=1,
      cex=0.7)
}
# reset plotting layout to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# Filter out rows where device_type or ad_position is 'Unknown'
filtered_data <- cleaned_data %>% 
  filter(device_type != "Unknown", ad_position != "Unknown")

# Create summary table: click rate by device type and ad position (excluding 'Unknown')
click_table <- tapply(filtered_data$click, 
                      list(filtered_data$ad_position, filtered_data$device_type), 
                      mean) #Calculates the mean of the click variable grouped by ad_position and device_type

# Print the click rate table
print(click_table)

# Create barplot
barplot(click_table,
        beside = TRUE,
        col = c("skyblue", "lavender", "lightcoral"),  # Adjust number of colors based on number of ad positions
        ylim = c(0, 1),
        main = "Click Rate by Device Type and Ad Position (without Unknown)",
        xlab = "Device Type",
        ylab = "Click Rate",
        cex.main = 0.85,
        cex.lab = 0.8, # Size of axis labels (x and y labels)
        cex.axis = 0.8,  # Size of axis tick labels
        legend.text = rownames(click_table), # Adds a legend using the row names 
        args.legend = list(x = 13.5, y = 1.15, cex = 0.6)) # Specifies the legend position and size
      
# Correlation matrix visualization
correlation_matrix <- cor(cleaned_data[,sapply(cleaned_data, is.numeric)], use = "complete.obs")
corrplot(correlation_matrix, method = "square", #square tiles
         addCoef.col = "red",                   #coefficient color
         tl.col = "black",                      #text label color
         number.digits = 4)                     #ensure enough space 

# Box plots and Set layout to display multiple plots 
par(mfrow = c(3, 2), mar = c(2, 2, 2, 0) + 0.1) # Reduced margins
categorical_cols<-c("gender","device_type","ad_position","browsing_history","time_of_day","click")
# Loop through each variable and create box plot
for (var in categorical) {
  formula <- as.formula(paste("age ~", var))
  # Filter out -1 ages and 'Unknown' for clean plots
  filtered_data <- cleaned_data %>% filter(age != -1, !!as.name(var) != "Unknown")
  par(las = 1)  # Adjust axis label size
  box_stats <- boxplot(formula, data = filtered_data,
                       col = c("skyblue", "lightgreen", "plum", "lightcoral", "gold"), #set colors
                       border = "darkblue",
                       main = paste("Age by", var),
                       xlab = var,
                       xaxt = "n",  # Suppress default x-axis
                       cex.axis = 0.75,  # Size for y-axis tick labels
                       ylab = "Age")  
  # Add custom x-axis 
  axis(1, at = seq_along(unique(filtered_data[[var]])), 
       labels = unique(filtered_data[[var]]), 
       cex.axis = 0.58  # Smaller size for x-axis tick labels
       )  
  # Calculate means by group
  group_means <- tapply(filtered_data$age, filtered_data[[var]], mean)
  # Overlay means (red points)
  points(1:length(group_means), group_means, col = "red", pch = 19)  # Red dots for means
  
  # Add text for mean and median values
  text(1:length(group_means), group_means + 7,  # Adjust vertical position for clarity
       labels = round(group_means, 1), col = "red", cex = 0.8)  # Text for means
  text(1:length(box_stats$names), box_stats$stats[3, ] - 2,  # Adjust vertical position for clarity
       labels = round(box_stats$stats[3, ], 1), col = "black", cex = 0.7)  # Text for medians
}
# reset plotting layout to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, cex.axis = 1, las = 0)


# ----- Predictive modelling begins -----
# Set up training and testing data:
set.seed(100) # Set seed for reproducibility
# Randomly sample 80% of rows from data for training
train.rows <- sample(rownames(cleaned_data), dim(cleaned_data)[1] * 0.8)
train_data <- cleaned_data[train.rows, ] # Create training set from chosen data

# Use remaining 20% as testing data
test.rows <- setdiff(rownames(cleaned_data), train.rows)
test_data <- cleaned_data[test.rows, ] #Create test set from chosen data
# 1.Logistic regression model begins
# Fit logistic regression model using all predictors to predict 'click'
# and all other variables as independent variables in the training data. Model follows a binomial distribution.
RM_model <- glm(click ~ ., family = "binomial", data = train_data)

# View detailed information about the trained logistic regression model, including parameters, p-values, and other statistics
summary(RM_model)

# 2. Predict "click" probabilities for test data
# Use the trained model to predict the probability of "click" for observations in test_data
# type="response" returns the probability of the "click = 1" class.
predicted <- predict(RM_model, test_data, type = "response")

# Round predicted probabilities to 0 or 1
# If probability >= 0.5, predict "click" (1), otherwise predict "no click" (0).
test_data$predicted <- ifelse(predicted >= 0.5, 1, 0)

# Display the first 10 rows of test data, including the "predicted" column
head(test_data, 10)

# 3. Evaluate the model with a confusion matrix
# Calculate and display the confusion matrix for the model
# positive = '1' indicates that the positive class is "click = 1"
confusionMatrix(as.factor(test_data$predicted), as.factor(test_data$click), positive = "1")

# 4. Plot ROC curve and calculate AUC
ROCRpred <- prediction(as.numeric(test_data$predicted), test_data$click)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, main = "Logistic Regression ROC Curve")
# Calculate the area under the ROC curve (AUC)
auc <- as.numeric(performance(ROCRpred, "auc")@y.values)
cat("AUC:", auc, "\n")

" ---------- MULTICOLLINEARITY CHECK -----------"
print(vif(RM_model))

# ---------- LEVERAGE PLOTS -----------"
ols_plot_resid_lev(RM_model)



# Random Forest model begins
train_data$click <- as.factor(train_data$click)
model_rf <- randomForest(click ~ ., data = train_data, ntree = 100)
print(model_rf)
predicted_rf_prob <- predict(model_rf, test_data, type = "prob")
predicted_rf <- predicted_rf_prob[, 2]

# Round predicted probabilities to 0 or 1
# If probability >= 0.5, predict "click" (1), otherwise predict "no click" (0).
test_data$predicted_rf <- ifelse(predicted_rf >= 0.5, 1, 0)
getOption("width") #expand the console window
head(test_data, 10)

# 3. Evaluate the model with a confusion matrix
confusionMatrix(as.factor(test_data$predicted_rf), as.factor(test_data$click), positive = "1")

# 4. Plot ROC curve and calculate AUC
ROCRpred = prediction(as.numeric(test_data$predicted_rf),test_data$click)
ROCRperf = performance(ROCRpred, "tpr","fpr")
plot(ROCRperf, main = "Random Forest ROC Curve")
auc <- as.numeric(performance(ROCRpred,"auc")@y.values)
cat("AUC:", auc, "\n")

# Independence of errors check
durbinWatsonTest(RM_model)