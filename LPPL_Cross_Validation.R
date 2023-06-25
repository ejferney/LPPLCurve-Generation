library(caret)
library(purrr)
# Function to perform LPPL modeling and evaluation
data <- read.csv(file.choose(), header = TRUE)
perform_lppl_model <- function(train_data, test_data) {
  # Create x and y vectors for training and testing
  x_train <- 0:(length(train_data$Close) - 1)
  y_train <- train_data$Close
  x_test <- 0:(length(test_data$Close) - 1)
  y_test <- test_data$Close
  
  # Define LPPL function
  lppl <- function(x, A, B, C, D, m, t, w) {
    y <- exp(A + B * (x - t)^m + C * (x - t)^m * cos(w * log(x - t)) + D * (x - t)^m * sin(w * log(x - t)))
    return(y)
  }
  
  # Define the objective function for gradient descent
  objective <- function(params) {
    A <- params[1]
    B <- params[2]
    C <- params[3]
    D <- params[4]
    m <- params[5]
    w <- params[6]
    
    lppl_vals <- lppl(x_train, A, B, C, D, m, t = -1, w)
    sum((y_train - lppl_vals)^2)
  }
  
  # Set the initial parameter values
  params_init <- c(0, 0, 0, 0, 0.6, 4)  # Initial values for A, B, C, D, m, and w
  
  # Set the learning rate
  learning_rate <- 0.001
  
  # Set the maximum number of iterations
  max_iterations <- 1000
  
  # Perform gradient descent
  for (i in 1:max_iterations) {
    # Calculate the gradient of the objective function numerically
    gradient <- numeric(length(params_init))
    for (j in 1:length(params_init)) {
      params_plus_h <- params_init
      params_plus_h[j] <- params_plus_h[j] + 0.0001
      params_minus_h <- params_init
      params_minus_h[j] <- params_minus_h[j] - 0.0001
      gradient[j] <- (objective(params_plus_h) - objective(params_minus_h)) / (2 * 0.0001)
    }
    
    # Update the parameter values using gradient descent
    params_init <- params_init - learning_rate * gradient
  }
  
  # Extract the optimized parameter values
  A <- params_init[1]
  B <- params_init[2]
  C <- params_init[3]
  D <- params_init[4]
  m <- params_init[5]
  w <- params_init[6]
  
  # Calculate predicted y values for the test data
  predicted_y <- lppl(x_test, A, B, C, D, m, t = -1, w)
  
  # Calculate correlation coefficient (r) for the test data
  correlation <- cor(y_test, predicted_y)
  
  # Calculate coefficient of determination (r^2) for the test data
  r_squared <- correlation^2
  
  # Calculate residual sum of squares (RSS) for the test data
  rss <- sum((y_test - predicted_y)^2)
  
  return(list(predicted_y = predicted_y,
              correlation = correlation,
              r_squared = r_squared,
              rss = rss))
}

# Set the number of folds for cross-validation
k <- 5

# Set seed for reproducibility
set.seed(123)

# Perform k-fold cross-validation
folds <- createFolds(data$Close, k = k, list = TRUE)

# Perform LPPL modeling and evaluation for each fold
results <- map(folds, function(fold) {
  train_data <- data[-fold, ]
  test_data <- data[fold, ]
  perform_lppl_model(train_data, test_data)
})

# Aggregate the evaluation results
correlation <- mean(sapply(results, function(result) result$correlation))
r_squared <- mean(sapply(results, function(result) result$r_squared))
rss <- sum(sapply(results, function(result) result$rss))

# Print the cross-validation results
print("Cross-Validation Results:")
print(paste("Mean Correlation (r):", correlation))
print(paste("Mean Coefficient of Determination (r^2):", r_squared))
print(paste("Total Residual Sum of Squares (RSS):", rss))

# Perform LPPL modeling using the full data
full_results <- perform_lppl_model(data, data)

# Print the out-of-sample test results
print("Out-of-Sample Test Results:")
print(paste("Correlation (r):", full_results$correlation))
print(paste("Coefficient of Determination (r^2):", full_results$r_squared))
print(paste("Residual Sum of Squares (RSS):", full_results$rss))

