
# ========================================================================================================
setwd("C:/Users/sasha/Downloads/NTU NBS/Y2S1/bc2406/grp 4 project/Report Datasets")
geopolitical_data <- read.csv("Geopolitical Events.csv")

# Load required libraries
library(data.table)
library(ggplot2)
library(corrplot)
library(dplyr)
library(caTools)
library(car)
library(MASS)
library(tidyr)
library(rpart)
library(rpart.plot)

#---------------------------------------------------------------------------------------------------------

# EDA GEOPOLITICAL EVENTS DATA

#---------------------------------------------------------------------------------------------------------

# Summary statistics and structure of the dataset
summary(geopolitical_data)
str(geopolitical_data)


# Aggregate the data to count conflicts by ConflictTypeNumeric
conflicttype_counts <- table(geopolitical_data$ConflictTypeNumeric)

# Sort the data
sorted_data <- sort(conflicttype_counts, decreasing = TRUE)

# Define colors for each category
bar_colors <- c("lightcoral", "lightslategray", "lightseagreen", "lightgoldenrodyellow")

# Determine the y-axis limit based on the maximum count
y_axis_limit <- max(conflicttype_counts) + 50

# Plot the bar graph with the adjusted y-axis limit
bp <- barplot(sorted_data, main = "Distribution of Conflict Types", col = bar_colors, las = 2, 
              ylim = c(0, y_axis_limit), names.arg = names(sorted_data))

# Add labels to all bars indicating their count
text(bp, sorted_data + 5, labels = sorted_data, cex = 0.8, pos = 3)


# Boxplot and density plot of SeverityIndex
boxplot(geopolitical_data$SeverityIndex, col = "lightblue", main = "Box Plot of Severity Index")
plot(density(geopolitical_data$SeverityIndex), col = "blue", main = "Density Plot of Severity Index")


# Box plot of Severity Index by Conflict Types with added color
ggplot(geopolitical_data, aes(x = as.factor(ConflictTypeNumeric), y = SeverityIndex, fill = as.factor(ConflictTypeNumeric), group = as.factor(ConflictTypeNumeric))) +
  geom_boxplot() +
  labs(title = "Severity Index by Conflict Types", x = "Conflict Types", y = "Severity Index") +
  scale_fill_brewer(palette = "Set3")

# Time Series Plot of SumConflicts

# Identify and group conflicts with duplicate rows
conflict_groups <- geopolitical_data %>%
  group_by(Headline) %>%
  mutate(ConflictGroup = row_number())

# Calculate SumConflicts based on ConflictTypeNumeric
conflict_groups <- conflict_groups %>%
  group_by(ConflictGroup, Year) %>%
  summarise(SumConflicts = sum(ConflictTypeNumeric))

# Create a time series object with specified start and end years
time_series <- ts(conflict_groups$SumConflicts, start = 1993, end = 2023, frequency = 1)

# Plot the time series
plot(time_series, main = "Time Series of SumConflicts", xlab = "Year", ylab = "Total SumConflicts")

#---------------------------------------------------------------------------------------------------------

# EDA OIL FACTORS DATA

#---------------------------------------------------------------------------------------------------------

oil_factors_data <- read.csv("Factors affecting Oil Price.csv")

summary(oil_factors_data)
str(oil_factors_data)


# Select columns you want for boxplots (excluding Year and Price)
boxplot_data <- oil_factors_data[, -c(1, 2)]

# Melt the data for easy plotting
melted_data <- reshape2::melt(boxplot_data)

# Create boxplots
ggplot(melted_data, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Boxplots of Oil Factors", x = "Variables", y = "Values") +
  scale_fill_brewer(palette = "Set3")

# Convert Year to a Date format
oil_factors_data$Year <- as.Date(paste(oil_factors_data$Year, "-01-01", sep = ""), format = "%Y-%m-%d")

# Define custom colors for each variable
custom_colors <- c("Price" = "darkblue", "Prod.Mil" = "darkgreen", "Demand.Mil" = "darkred", 
                   "Gas" = "darkorange", "Consum.Mil" = "purple", "Int.Price.USD" = "darkcyan", "Export" = "darkmagenta")

# Melt the data for plotting
melted_data <- reshape2::melt(oil_factors_data, id.vars = "Year")

# Create time series analysis plots with custom colors
ggplot(melted_data, aes(x = Year, y = value, color = variable)) +
  geom_line() +
  labs(title = "Time Series Analysis of Oil Factors", x = "Year", y = "Values") +
  scale_color_manual(values = custom_colors) +
  facet_wrap(~variable, scales = "free_y")


oil_factors_data$Year <- as.integer(format(oil_factors_data$Year, "%Y"))

correlations <- cor(oil_factors_data, use = "complete.obs")
corrplot(correlations, method = "circle", type = "upper", tl.cex = 0.8)

#---------------------------------------------------------------------------------------------------------

# DATASET MERGING

#---------------------------------------------------------------------------------------------------------

# Filter geopolitical_data for years between 2000 and 2022
filtered_geopolitical_data <- geopolitical_data %>%
  filter(Year >= 2000 & Year <= 2022)

# Group and aggregate SumConflicts and SeverityIndex by Year, including events spanning multiple years
aggregated_geopolitical_data <- filtered_geopolitical_data %>%
  group_by(Year) %>%
  summarise(TotalSumConflicts = sum(ConflictTypeNumeric), MeanSeverityIndex = mean(SeverityIndex))

# Now, create a separate data frame for events spanning multiple years
events_spanning_multiple_years <- filtered_geopolitical_data %>%
  group_by(Year, Headline) %>%
  summarise(
    YearSpan = max(Year) - min(Year) + 1,
    SumConflicts = sum(ConflictTypeNumeric)
  )


# Merge aggregated_geopolitical_data with oil_factors_data using the 'Year' column
merged_data <- merge(oil_factors_data, aggregated_geopolitical_data, by = "Year", all.x = TRUE)

# Now, calculate the contribution of events spanning multiple years
merged_data <- merged_data %>%
  left_join(events_spanning_multiple_years, by = c("Year" = "Year")) %>%
  mutate(
    TotalSumConflicts = ifelse(is.na(TotalSumConflicts), 0, TotalSumConflicts) + YearSpan * SumConflicts,
    MeanSeverityIndex = ifelse(is.na(MeanSeverityIndex), 0, MeanSeverityIndex)
  ) %>%
  dplyr::select(-YearSpan, -Headline, -SumConflicts)


# Now, merged_data contains 'SeverityIndex' merged with oil_factors_data for the years between 2000 and 2022, taking into account events spanning multiple years.


#---------------------------------------------------------------------------------------------------------

# EDA for merged

#---------------------------------------------------------------------------------------------------------

# Check the merged data structure and summary
str(merged_data)
summary(merged_data)

# Calculate the correlation matrix
correlation_matrix <- cor(merged_data[sapply(merged_data, is.numeric)], use = "complete.obs")

# Display the correlation matrix
print(correlation_matrix)


# Create the correlation plot
corrplot(correlation_matrix, method = "circle", type = "upper")

# Create time series plots for TotalSumConflicts and MeanSeverityIndex
ggplot(merged_data, aes(x = Year, y = TotalSumConflicts)) +
  geom_line() +
  labs(title = "Time Series of Total Sum Conflicts", x = "Year", y = "Total Sum Conflicts")

ggplot(merged_data, aes(x = Year, y = MeanSeverityIndex)) +
  geom_line() +
  labs(title = "Time Series of Mean Severity Index", x = "Year", y = "Mean Severity Index")


#---------------------------------------------------------------------------------------------------------

# LINEAR REG 

#---------------------------------------------------------------------------------------------------------

# Load necessary libraries
library(caret)
library(car)
library(DescTools)
library(ggplot2)
library(MASS) # for stepAIC
library(rpart)
library(rpart.plot)

# Set seed for reproducibility
set.seed(2)

# Assuming 'merged_data' is your full dataset and 'Price' is the target variable
split <- sample.split(Y = merged_data$Price, SplitRatio = 0.7)
train_data <- subset(merged_data, split == TRUE)
test_data <- subset(merged_data, split == FALSE)
model <- lm(Price ~ ., data = train_data)
par(mfrow=c(2,2))
plot(model)

library(ggplot2)
library(tidyr)

# Reshape the data to long format
merged_data_long <- merged_data %>%
  gather(key = "Variable", value = "Value", MeanSeverityIndex, TotalSumConflicts, 
         Export, `Demand.Mil`, `Int.Price.USD`, `Consum.Mil`)

# Now, create the scatter plot with facets
ggplot(merged_data_long, aes(x = Value, y = Price)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "lightblue") +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Scatter plots of Price against various variables")


# Define a function to identify and replace outliers
identify_and_replace_outliers <- function(x, replace_with = "mean") {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  if(replace_with == "mean") {
    x[x < lower_bound | x > upper_bound] <- mean(x, na.rm = TRUE)
  } else if(replace_with == "median") {
    x[x < lower_bound | x > upper_bound] <- median(x, na.rm = TRUE)
  }
  return(x)
}

# Apply Winsorization and outlier replacement
numeric_columns <- sapply(train_data, is.numeric)
train_data[numeric_columns] <- lapply(train_data[numeric_columns], Winsorize, probs = c(0.05, 0.95))
train_data[numeric_columns] <- lapply(train_data[numeric_columns], identify_and_replace_outliers, replace_with = "median")

# Log transformation
# Add a small constant to avoid taking log of 0
constant <- 1e-6
train_data_log <- train_data
test_data_log <- test_data

# Apply log transformation to numeric predictors and target variable in training and testing datasets
train_data_log[numeric_columns] <- lapply(train_data_log[numeric_columns], function(x) log(x + constant))
train_data_log$Price <- log(train_data_log$Price + constant)
test_data_log[numeric_columns] <- lapply(test_data_log[numeric_columns], function(x) log(x + constant))
test_data_log$Price <- log(test_data_log$Price + constant)


# Reshape the data to long format for the variables of interest
merged_data_long <- merged_data %>%
  pivot_longer(cols = c(MeanSeverityIndex, TotalSumConflicts, Export, `Demand.Mil`, `Int.Price.USD`, `Consum.Mil`),
               names_to = "Variable",
               values_to = "Value")

# Create density plots for each variable in separate facets without log transformation
ggplot(merged_data_long, aes(x = Value)) + 
  geom_density() +
  facet_wrap(~ Variable, scales = "free") +  # Separate plots for each variable
  labs(title = "Density Plots of Variables",
       x = "Values",
       y = "Density") +
  theme_minimal()  # Optional: a minimal theme for a nicer look

# Model Building with Stepwise Regression on log-transformed data
stepwise_model_log <- stepAIC(lm(Price ~ ., data = train_data_log), direction = "both")

# VIF Analysis to check for multicollinearity after log transformation
vif_values_log <- vif(stepwise_model_log)
print(vif_values_log)

# Building the model with selected predictors on log-transformed data
selected_model_log <- lm(Price ~ MeanSeverityIndex + TotalSumConflicts+Consum.Mil+Int.Price.USD+ Demand.Mil+ Export, data = train_data_log)

# Summarize the selected model
summary(selected_model_log)
par(mfrow=c(2,2))
plot(selected_model_log)

# Predict on training data using the selected model
train_predictions_log <- predict(selected_model_log, newdata = train_data_log)
train_predictions <- exp(train_predictions_log) - constant

# Calculate RMSE on the original scale of training data
train_rmse <- sqrt(mean((exp(train_data_log$Price) - constant - train_predictions)^2))
print(paste("Training RMSE (back-transformed):", train_rmse))

# Predict on test data using the selected model
test_predictions_log <- predict(selected_model_log, newdata = test_data_log)
test_predictions <- exp(test_predictions_log) - constant

# Calculate RMSE on the original scale of test data
test_rmse <- sqrt(mean((exp(test_data_log$Price) - constant - test_predictions)^2))
print(paste("Test RMSE (back-transformed):", test_rmse))

# CART
CART <- rpart(Price ~ ., data = train_data_log, method = "anova", control = rpart.control(minsplit = 2, cp = 0))

# Pruning of the tree
CVerror.cap <- CART$cptable[which.min(CART$cptable[,"xerror"]), "xerror"] + CART$cptable[which.min(CART$cptable[,"xerror"]), "xstd"]
i <- 1; j <- 4
while(CART$cptable[i, j] > CVerror.cap) {
  i <- i + 1
}
cp.opt <- ifelse(i > 1, sqrt(CART$cptable[i,1] * CART$cptable[i-1,1]), 1)

# Prune the tree with the optimal CP value
CART_pruned <- prune(CART, cp = cp.opt)
rpart.plot(CART_pruned)

# Predict on the log-transformed training data using the pruned CART model
predicted_CART_train_log <- predict(CART_pruned, newdata = train_data_log)
predicted_CART_train <- exp(predicted_CART_train_log) - constant

# Calculate residuals for the training data
residuals_CART_train <- (exp(train_data_log$Price) - constant) - predicted_CART_train

# Calculate RMSE for the training data
RMSE.CART.train <- sqrt(mean(residuals_CART_train^2))
print(paste("CART RMSE on training data:", RMSE.CART.train))

# Predict on the log-transformed test data using the pruned CART model
predicted_CART_test_log <- predict(CART_pruned, newdata = test_data_log)
predicted_CART_test <- exp(predicted_CART_test_log) - constant

# Calculate RMSE on the original scale of test data
RMSE.CART.test <- sqrt(mean((exp(test_data_log$Price) - constant - predicted_CART_test)^2))
print(paste("CART RMSE on test data:", RMSE.CART.test))

# Model Complexity Comparison
LinearM_complexity <- length(coefficients(selected_model_log)) # Number of coefficients in the linear model
CARTM_complexity <- sum(CART_pruned$frame$var != "<leaf>") # Number of splits in the CART model

# Include the complexity in the results data frame
results <- data.frame(
  Model = c("Linear Regression", "CART"),
  Trainset_RMSE = c(train_rmse, RMSE.CART.train),
  Testset_RMSE = c(test_rmse, RMSE.CART.test),
  Model_Complexity = c(LinearM_complexity, CARTM_complexity)
)

print(results)

CART_pruned$variable.importance
scaledVarImpt <- round(100*CART_pruned$variable.importance/sum(CART_pruned$variable.importance))
scaledVarImpt[scaledVarImpt > 3]

