library(ggplot2)
library(numDeriv)

# Read stock data from CSV file
data <- read.csv(file.choose(), header = TRUE)

# Create x and y vectors
x <- 0:(length(data$Close) - 1)
y <- data$Close
t <- -1

# Define LPPL function
lppl <- function(x, A, B, C, D, m, t, w) {
  expr <- A + B * (x - t)^m + C * (x - t)^m * cos(w * log(abs(x - t) + 1e-8)) + D * (x - t)^m * sin(w * log(abs(x - t) + 1e-8))
  y <- exp(ifelse(is.nan(expr), -Inf, expr))
  return(y)
}

# Define the objective function for gradient descent
objective <- function(params) {
  lppl_vals <- lppl(x, params[1], params[2], params[3], params[4], params[5], t = -1, params[6])
  sum((y - lppl_vals)^2)
}

# Set the initial parameter values
params_init <- c(0, 0, 0, 0, 0.6, 4)  # Initial values for A, B, C, D, m, and w

# Set the learning rate
learning_rate <- 0.001

# Set the maximum number of iterations
max_iterations <- 1000

# Perform gradient descent
for (i in 1:max_iterations) {
  gradient <- numDeriv::grad(objective, params_init)
  params_init <- params_init - learning_rate * gradient
}

# Extract the optimized parameter values
A <- params_init[1]
B <- params_init[2]
C <- params_init[3]
D <- params_init[4]
m <- params_init[5]
w <- params_init[6]

# Generate LPPL curve data
curve_points <- 1000
x_curve <- seq(min(x), max(x), length.out = curve_points)
lppl_curve_data <- data.frame(x = x_curve, y = lppl(x_curve, A, B, C, D, m, t = -1, w))

# Create a scatter plot of the data
scatter_plot <- ggplot(data = data, aes(x = x, y = Close)) +
  geom_point(color = "black") +
  labs(title = "Stock over time", x = "Time (days)", y = "Close") +
  theme_minimal()

# Overlay the LPPL curve equation on the scatter plot
scatter_plot +
  geom_line(data = lppl_curve_data[!is.na(lppl_curve_data$y), ], aes(x = x, y = y), color = "blue") +
  geom_text(data = data.frame(x = max(x), y = lppl(max(x), A, B, C, D, m, t = -1, w)),
            aes(x = x, y = y, label = paste("y = e^(", round(A, 4), "+", round(B, 4), "(x -", round(t, 4), ")^", round(m, 4),
                                            "+", round(C, 4), "(x -", round(t, 4), ")^", round(m, 4), "* cos(", round(w, 4), "* ln(x -", round(t, 4), "))",
                                            "+", round(D, 4), "(x -", round(t, 4), ")^", round(m, 4), "* sin(", round(w, 4), "* ln(x -", round(t, 4), ")))")),
            color = "blue", hjust = -0.1) +
  coord_cartesian(xlim = c(min(x), max(x)))  # Set x-axis limits to match the data range

# Calculate predicted y values
predicted_y <- lppl(x, A, B, C, D, m, t = -1, w)


# Calculate correlation coefficient (r)
correlation <- cor(y, predicted_y)

# Calculate coefficient of determination (r^2)
r_squared <- correlation^2

# Perform goodness-of-fit test
observed_values <- y
expected_values <- predicted_y
df <- length(y) - length(params_init)  # Degrees of freedom
chi_square <- sum((observed_values - expected_values)^2 / expected_values)
p_value <- 1 - pchisq(chi_square, df)

# Parameter significance test
hessian_matrix <- numDeriv::hessian(objective, params_init)
param_std_errors <- sqrt(diag(solve(hessian_matrix)))
t_values <- params_init / param_std_errors
p_values <- 2 * pt(abs(t_values), df)
param_significance <- data.frame(Parameter = c("A", "B", "C", "D", "m", "w"),
                                 Estimate = params_init,
                                 Std.Error = param_std_errors,
                                 t_value = t_values,
                                 p_value = p_values)

# Residual analysis
residuals <- observed_values - expected_values

# Plot the residuals
residual_plot <- ggplot(data = data, aes(x = x, y = residuals)) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Analysis", x = "Time (days)", y = "Residuals") +
  theme_minimal()

print("Residual Analysis:")
print(paste("Correlation coefficient (r):", correlation))
print(paste("Coefficient of determination (r^2):", r_squared))
print(paste("Goodness-of-fit test:"))
print(paste("  Chi-squared statistic:", chi_square))
print(paste("  Degrees of freedom:", df))
print(paste("  p-value:", p_value))
print("Parameter significance test:")
print(param_significance)

# Print the residual plot
print(residual_plot)

