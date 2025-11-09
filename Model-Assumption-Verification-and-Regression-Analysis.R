
setwd("")

install.packages("haven")
install.packages("ggplot2")
install.packages("lmtest")

# Load packages
library(haven)    # for reading .dta files
library(ggplot2)  # for scatter plots
library(lmtest)   # for heteroscedasticity test

# Open the dataset (Stata format)
bmi_data <- read_dta("bmi.dta")


# Simple linear model (equivalent to `regress bmi age`)
model <- lm(bmi ~ age, data = bmi_data)

# Predicted values (equivalent to `predict yhat`)
bmi_data$yhat <- predict(model)

# Residuals (equivalent to `predict resid, rstudent`)
bmi_data$resid <- rstudent(model)  # Studentized residuals

# Histogram of residuals
hist(bmi_data$resid, main = "Histogram of Residuals", xlab = "Residuals", col = "blue", border = "white")

# Histogram of residuals with normal density curve
ggplot(bmi_data, aes(x = resid)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", color = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(bmi_data$resid), sd = sd(bmi_data$resid)), color = "red") +
  labs(title = "Histogram of Residuals with Normal Curve", x = "Residuals", y = "Density") +
  theme_minimal()

# Q-Q plot of residuals (equivalent to `qnorm resid`)
qqnorm(bmi_data$resid)
qqline(bmi_data$resid, col = "red")

# Residuals vs fitted values plot (equivalent to `rvfplot`)
## with raw residuals
ggplot(bmi_data, aes(x = fitted(model), y = residuals(model))) +
  geom_point() +
  labs(title = "Residuals vs Fitted Values", x = "Fitted values", y = "Residuals") +
  theme_minimal()
# with studentized residuals
ggplot(bmi_data, aes(x = yhat, y = resid)) +
  geom_point() +
  labs(title = "Residuals vs Fitted Values", x = "Fitted values", y = "Residuals") +
  theme_minimal()

# Scatter plot of residuals vs age (equivalent to `scatter resid age`)
ggplot(bmi_data, aes(x = age, y = resid)) +
  geom_point() +
  labs(title = "Residuals vs Age", x = "Age", y = "Residuals") +
  theme_minimal()

# Heteroscedasticity test (equivalent to `estat hettest`)
bptest(model)  # Breusch-Pagan test for heteroscedasticity



install.packages("car")

# Load necessary libraries
library(car)      # for influence plots and DFBeta

# Leverage plot (equivalent to 'lvr2plot')
bmi_data$leverage <- hatvalues(model) # Calculate leverage (hat) values
bmi_data$residuals_raw <- residuals(model) # Extract raw residuals
rss <- sum(bmi_data$residuals_raw^2) # Calculate residual sum of squares
bmi_data$norm_resid <- (bmi_data$residuals_raw) / sqrt(rss) # Calculate normalized residuals squared
bmi_data$norm_resid_sq <- bmi_data$norm_resid^2 # Calculate normalized residuals squared

ggplot(bmi_data, aes(x = norm_resid_sq, y = leverage)) +
  geom_point() +
  labs(title = "Leverage vs. Squared Standardized Residuals",
       x = "Normalized Residuals Squared",
       y = "Leverage") +
  theme_minimal()

# R Alternative (influence plot showing leverage and Cook's distance)
influencePlot(model, main = "Influence Plot", id.method = "identify", id.n = 5)

# Compute DFBETAs
dfbetas_values <- dfbetas(model)

# Convert to a dataframe to facilitate plotting
dfbetas_data <- as.data.frame(dfbetas_values)

# Add DFBeta for age to the bmi_data dataframe
bmi_data$dfbeta_1 <- dfbetas_data[, "age"]

# Dot plot of DFBeta values for age (equivalent to `dotplot _dfbeta`)
ggplot(bmi_data, aes(y = dfbeta_1)) +
  geom_histogram(binwidth = 0.005, fill = "blue", color = "black") +
  labs(title = "Frequency Plot of DFBETA for Age",
       x = "Frequency",
       y = "DFBETA, age") +
  theme_minimal()

# Generate absolute DFBETAs (equivalent to `gen abs_dfbeta=abs(_dfbeta_1)`)
bmi_data$abs_dfbeta <- abs(bmi_data$dfbeta_1)

# Scatter plot of bmi vs age, weighted by abs_dfbeta (equivalent to `twoway (scatter bmi age [weight=abs_dfbeta], msym(Oh))`)
ggplot(bmi_data, aes(x = age, y = bmi)) +
  geom_point(aes(size = abs_dfbeta), shape = 1) + # Circles with sizes proportional to abs_dfbeta
  geom_line(aes(y = yhat), color = "red") +       # Line for fitted values (yhat)
  labs(title = "Relationship Between BMI & Age with DFBETA Weighting", 
       x = "Age", 
       y = "BMI") +
  theme_minimal()  +
  scale_size_continuous(range = c(1, 10)) # Change size of circles



install.packages("dplyr")

# Load packages
library(dplyr)    # for data manipulation

# Load the dataset (assumed to be vitd.dta)
vitd_data <- read_dta("vitd.dta")

# Histogram of vitd
hist(vitd_data$vitd, main = "Histogram of vitd", xlab = "vitd", col = "blue", border = "white", breaks = 30)

# Summary statistics of vitd
summary(vitd_data$vitd)

# Generate the variable 't'
vitd_data <- vitd_data %>%
  mutate(year = as.integer(year), # assuming `year` is a variable in the dataset
         t = as.numeric((date_test - as.Date(paste0("01jan", year), format = "%d%b%Y")) / 365.25))

# Scatter plot of vitd against t (takes a while to generate the plot)
ggplot(vitd_data, aes(x = t, y = vitd)) +
  geom_point(aes(y = vitd), color = "blue") +
  labs(title = "Scatter plot of vitd vs. t", x = "t", y = "vitd") +
  theme_minimal()

# Regression of vitd on t (quadratic term for t)
fit <- lm(vitd ~ t + I(t^2), data = vitd_data)

# Predicted values of vitd
vitd_data$vitd_pred <- predict(fit)

# Sorting by t for the plot
vitd_data <- vitd_data %>%
  arrange(t)

# Plot of scatter and predicted line
ggplot(vitd_data, aes(x = t)) +
  geom_point(aes(y = vitd), color = "blue") +
  geom_line(aes(y = vitd_pred), color = "red") +
  labs(title = "Scatter of vitd and fitted values", x = "t", y = "vitd") +
  theme_minimal()



install.packages("sandwich")

# Load packages
library(sandwich)

# Check heteroscedasticity - residual-vs-fitted plot (rvfplot equivalent)
model <- lm(vitd ~ t + I(t^2), data = vitd_data)
ggplot(vitd_data, aes(x = fitted(model), y = residuals(model))) +
  geom_point() +
  labs(title = "Residuals vs Fitted Values", x = "Fitted values", y = "Residuals") +
  theme_minimal()

# Regular regression (equivalent to `regress vitd c.t##c.t`)
regular_model <- lm(vitd ~ t + I(t^2), data = vitd_data)

# Summary of the regular regression
summary(regular_model)

# Regression with robust standard errors (equivalent to `regress vitd c.t##c.t, robust`)
robust_se <- coeftest(regular_model, vcov = vcovHC(regular_model, type = "HC1"))

# Output with robust standard errors
print(robust_se)



install.packages("boot")

# Load packages
library(boot)     # for bootstrap

# Bootstrap regression (equivalent to `regress vitd c.t##c.t, vce(bootstrap)`)
bootstrap_fn <- function(data, indices) {
  d <- data[indices, ]  # Resample the data
  model <- lm(vitd ~ t + I(t^2), data = d)
  return(coef(model))
}

# Run the bootstrap with default 50 reps (as done in Stata's vce(bootstrap)). 
# Please note: R default reps would be 1000 instead.
set.seed(123)  # For reproducibility
bootstrap_result <- boot(data = vitd_data, statistic = bootstrap_fn, R = 50)

# View the bootstrap results
print(bootstrap_result)

# Bootstrap regression with 250 reps (equivalent to `regress vitd c.t##c.t, vce(bootstrap, reps(250))`)
set.seed(123)  # For reproducibility
bootstrap_result_250 <- boot(data = vitd_data, statistic = bootstrap_fn, R = 250)

# View the bootstrap results for 250 reps
print(bootstrap_result_250)
