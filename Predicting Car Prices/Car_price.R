# Load the data from the text file
library(readr)
library(tidyr)
library(dplyr)
cars <- read.table("car.txt", sep = ",", header = FALSE, stringsAsFactors = FALSE)

# Assign column names
colnames(cars) <- c(
  "symboling",
  "normalized_losses",
  "make",
  "fuel_type",
  "aspiration",
  "num_doors",
  "body_style",
  "drive_wheels",
  "engine_location",
  "wheel_base",
  "length",
  "width",
  "height",
  "curb_weight",
  "engine_type",
  "num_cylinders",
  "engine_size",
  "fuel_system",
  "bore",
  "stroke",
  "compression_ratio",
  "horsepower",
  "peak_rpm",
  "city_mpg",
  "highway_mpg",
  "price"
)

# Perform Initial Data Cleaning
# Convert columns to appropriate data types
# Removing non-numerical columns and removing missing data
cars <- cars %>% 
  select(
    symboling, wheel_base, length, width, height, curb_weight,
    engine_size, bore, stroke, compression_ratio, horsepower, 
    peak_rpm, city_mpg, highway_mpg, price
  ) %>% 
  filter(
    stroke != "?",
    bore != "?",
    horsepower != "?",
    peak_rpm != "?",
    price != "?"
  ) %>% 
  mutate(
    stroke = as.numeric(stroke),
    bore = as.numeric(bore),
    horsepower = as.numeric(horsepower),
    peak_rpm = as.numeric(peak_rpm),
    price = as.numeric(price)
  )


# Save the cleaned data to a CSV file
write.csv(cars, "cars.csv", row.names = FALSE)

# Confirming that each of the columns are numeric
library(purrr)
map(cars, typeof)

cars <- read.csv("cars.csv")

# Visualize Feature-Price Relationships with featurePlot
# to create scatterplots (and other types of plots) for each feature against a target variable, which in this case is price. This allows you to quickly examine the relationships between the features in the cars dataset and price.
# Load the caret library

library(caret)

# Use featurePlot to create a lattice plot of features against price
# featurePlot(cars, cars$price)

featurePlot(x = cars[, -which(names(cars) == "price")], 
            y = cars$price, 
            plot = "scatter")

# Visualize the Distribution of the Price Outcome
# Visualize the distribution of the price variable
hist(cars$price, breaks = 30, main = "Distribution of Car Prices", xlab = "Price", col = "skyblue")

# Alternatively, you could use ggplot2 for a more refined plot
library(ggplot2)
ggplot(cars, aes(x = price)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Car Prices", x = "Price", y = "Frequency")

# Split the Data
# Set seed for reproducibility
set.seed(123)

# Create a partition with 70% of the data for training
trainIndex <- createDataPartition(cars$price, p = 0.7, list = FALSE)

# Split the data into training and testing sets
trainSet <- cars[trainIndex, ]
testSet <- cars[-trainIndex, ]

#Verify the Sizes of the Training and Test Sets
# Check the size of the training set
nrow(trainSet)
# Check the size of the test set
nrow(testSet)

# Alternatively, you can check both at once
cat("Training set size:", nrow(trainSet), "\n")
cat("Test set size:", nrow(testSet), "\n")

# Set Up trainControl() for Cross-Validation
# The trainControl() function in the caret package is used to define how the training process should be conducted.
# Load necessary library
library(caret)

# Set up trainControl with cross-validation
train_control <- trainControl(method = "cv",   # Use cross-validation
                              number = 10)     # Number of folds

# Create a grid of hyperparameters for k-nearest neighbors
knn_grid <- expand.grid(k = seq(3, 21, by = 2))  # k from 3 to 21, odd numbers only

# Train the KNN Model Using train()
# Train the KNN model based on all the features
knn_model <- train(price ~ .,          # Formula for the model
                   data = trainSet,    # Training dataset
                   method = "knn",     # Specify the model type as k-nearest neighbors
                   trControl = train_control,  # Pass in the trainControl object
                   tuneGrid = knn_grid,         # Pass in the hyperparameter grid
                   preProcess = c("center", "scale"))        

# Review the Model Results
# Review the results
print(knn_model)

# Best hyperparameter
print(knn_model$bestTune)

# Plot the model to visualize performance across different values of k
plot(knn_model)

# Fit the linear regression model
lm_model <- lm(price ~ ., data = trainSet)


# Generate Predictions on the Test Set
# Generate predictions using the KNN model
knn_predictions <- predict(knn_model, newdata = testSet)
# Generate predictions using the Linear Regression model
lm_predictions <- predict(lm_model, newdata = testSet)

# Evaluate Model Performance Using postResample()
# Evaluate KNN model
knn_performance <- postResample(pred = knn_predictions, obs = testSet$price)
# Evaluate Linear Regression model
lm_performance <- postResample(pred = lm_predictions, obs = testSet$price)

#Combine Results into a Tibble
# Combine the results into a tibble
library(tibble)

model_performance <- tibble(
  Model = c("K-Nearest Neighbors", "Linear Regression"),
  RMSE = c(knn_performance["RMSE"], lm_performance["RMSE"]),
  Rsquared = c(knn_performance["Rsquared"], lm_performance["Rsquared"]),
  MAE = c(knn_performance["MAE"], lm_performance["MAE"])
)

# Print the performance table
print(model_performance)

