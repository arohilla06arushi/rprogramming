

# 1. PACKAGE INSTALLATION

install.packages(c(
  "readxl",
  "dplyr",
  "ggplot2",
  "tidyr",
  "lmtest",
  "car",
  "sandwich",
  "tseries",
  "forecast",
  "broom",
  "corrplot"
))


# 2. LOADING PACKAGES


library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lmtest)
library(car)
library(sandwich)
library(tseries)
library(forecast)
library(broom)
library(corrplot)

data <- readxl::read_excel(file.choose())

View(data)

head(data)

str(data)

summary(data)

# Data Preparation

data <- dplyr::rename(
  data,
  GDP_Growth = `Real GDP growth (%)`,
  Inflation = `Inflation (CPI, %)`,
  FDI = `FDI net inflows (% of GDP)`,
  Government_Expenditure = `Government expenditure (% of GDP)`
)

data <- data |>
  dplyr::arrange(Year)

# Check Missing Values

colSums(is.na(data))

# Check Duplicate Years

sum(duplicated(data$Year))

# Check Years and Observations

range(data$Year)

nrow(data)

# View Prepared Dataset

View(data)

summary(data)

# Descriptive Statistics

# Summary of all variables
summary(data)

# Mean
sapply(
  data[, c("GDP_Growth", "Inflation", "FDI", "Government_Expenditure")],
  mean,
  na.rm = TRUE
)

# Median
sapply(
  data[, c("GDP_Growth", "Inflation", "FDI", "Government_Expenditure")],
  median,
  na.rm = TRUE
)

# Standard Deviation
sapply(
  data[, c("GDP_Growth", "Inflation", "FDI", "Government_Expenditure")],
  sd,
  na.rm = TRUE
)

# Minimum and Maximum
sapply(
  data[, c("GDP_Growth", "Inflation", "FDI", "Government_Expenditure")],
  min,
  na.rm = TRUE
)

sapply(
  data[, c("GDP_Growth", "Inflation", "FDI", "Government_Expenditure")],
  max,
  na.rm = TRUE
)

# Variance
sapply(
  data[, c("GDP_Growth", "Inflation", "FDI", "Government_Expenditure")],
  var,
  na.rm = TRUE
)

# Exploratory Data Analysis

library(ggplot2)

# Real GDP Growth Over Time
ggplot(data, aes(x = Year, y = GDP_Growth)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Real GDP Growth in India (1995–2025)",
    x = "Year",
    y = "Real GDP Growth (%)"
  ) +
  theme_minimal()

# Inflation Over Time
ggplot(data, aes(x = Year, y = Inflation)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Inflation Rate in India (1995–2025)",
    x = "Year",
    y = "Inflation (%)"
  ) +
  theme_minimal()

# FDI Net Inflows Over Time
ggplot(data, aes(x = Year, y = FDI)) +
  geom_line() +
  geom_point() +
  labs(
    title = "FDI Net Inflows in India (1995–2025)",
    x = "Year",
    y = "FDI Net Inflows (% of GDP)"
  ) +
  theme_minimal()

# Government Expenditure Over Time
ggplot(data, aes(x = Year, y = Government_Expenditure)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Government Expenditure in India (1995–2025)",
    x = "Year",
    y = "Government Expenditure (% of GDP)"
  ) +
  theme_minimal()

# Relationship Between Inflation and GDP Growth
ggplot(data, aes(x = Inflation, y = GDP_Growth)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Inflation and Real GDP Growth in India",
    x = "Inflation (%)",
    y = "Real GDP Growth (%)"
  ) +
  theme_minimal()

# Correlation Analysis

# Select variables for analysis
variables <- data[, c(
  "GDP_Growth",
  "Inflation",
  "FDI",
  "Government_Expenditure"
)]

# Pearson Correlation Matrix
correlation_matrix <- cor(
  variables,
  use = "complete.obs",
  method = "pearson"
)

print(correlation_matrix)

# Visualise Correlations
corrplot::corrplot(
  correlation_matrix,
  method = "number",
  type = "upper",
  tl.col = "black",
  tl.srt = 45
)

# Spearman Rank Correlation Matrix
spearman_matrix <- cor(
  variables,
  use = "complete.obs",
  method = "spearman"
)

print(spearman_matrix)

# Test Correlation Between GDP Growth and Inflation
cor.test(
  data$GDP_Growth,
  data$Inflation,
  method = "pearson"
)

# Test Correlation Between GDP Growth and FDI
cor.test(
  data$GDP_Growth,
  data$FDI,
  method = "pearson"
)

# Test Correlation Between GDP Growth and Government Expenditure
cor.test(
  data$GDP_Growth,
  data$Government_Expenditure,
  method = "pearson"
)

# Test Spearman Correlation Between GDP Growth and Inflation
cor.test(
  data$GDP_Growth,
  data$Inflation,
  method = "spearman",
  exact = FALSE
)

# Multiple Linear Regression

# Estimate the regression model
model <- lm(
  GDP_Growth ~ Inflation + FDI + Government_Expenditure,
  data = data
)

# Display regression results
summary(model)

# Estimated regression coefficients
coef(model)

# 95% Confidence Intervals
confint(model, level = 0.95)

# Tidy Regression Output
broom::tidy(model)

# Model Fit Statistics
broom::glance(model)

# Extract R-squared
summary(model)$r.squared

# Extract Adjusted R-squared
summary(model)$adj.r.squared

# Extract Residual Standard Error
summary(model)$sigma

# Extract F-statistic
summary(model)$fstatistic

# Calculate Fitted Values
data$Fitted_GDP_Growth <- fitted(model)

# Calculate Regression Residuals
data$Regression_Residuals <- residuals(model)

# View Actual and Predicted GDP Growth
head(data[, c(
  "Year",
  "GDP_Growth",
  "Fitted_GDP_Growth",
  "Regression_Residuals"
)])

# Plot Actual vs Predicted GDP Growth
ggplot(data, aes(x = GDP_Growth, y = Fitted_GDP_Growth)) +
  geom_point() +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Actual vs Predicted Real GDP Growth",
    x = "Actual GDP Growth (%)",
    y = "Predicted GDP Growth (%)"
  ) +
  theme_minimal()

# Multicollinearity Diagnostics

# Variance Inflation Factors
car::vif(model)

# Tolerance Values
1 / car::vif(model)

# Correlation Between Independent Variables
cor(
  data[, c(
    "Inflation",
    "FDI",
    "Government_Expenditure"
  )],
  use = "complete.obs"
)

# Condition Number
X <- model.matrix(model)[, -1]

kappa(scale(X))

# Pairwise Correlation Plot
corrplot::corrplot(
  cor(
    data[, c(
      "Inflation",
      "FDI",
      "Government_Expenditure"
    )],
    use = "complete.obs"
  ),
  method = "number",
  type = "upper",
  tl.col = "black"
)

# Heteroskedasticity Diagnostics

# Breusch-Pagan Test
lmtest::bptest(model)

# Breusch-Pagan Test Using Fitted Values
lmtest::bptest(
  model,
  ~ fitted(model) + I(fitted(model)^2)
)

# White-Style Heteroskedasticity Test
lmtest::bptest(
  model,
  ~ Inflation + I(Inflation^2) +
    FDI + I(FDI^2) +
    Government_Expenditure +
    I(Government_Expenditure^2) +
    Inflation:FDI +
    Inflation:Government_Expenditure +
    FDI:Government_Expenditure,
  data = data,
  studentize = FALSE
)

# Goldfeld-Quandt Test
lmtest::gqtest(model)

# Residuals vs Fitted Values
plot(
  fitted(model),
  residuals(model),
  main = "Residuals vs Fitted Values",
  xlab = "Fitted Values",
  ylab = "Residuals"
)

abline(h = 0, lty = 2)

# Scale-Location Plot
plot(
  fitted(model),
  sqrt(abs(rstandard(model))),
  main = "Scale-Location Plot",
  xlab = "Fitted Values",
  ylab = "Square Root of Absolute Standardised Residuals"
)

# Heteroskedasticity-Robust Standard Errors (HC1)
lmtest::coeftest(
  model,
  vcov. = sandwich::vcovHC(model, type = "HC1")
)

# Heteroskedasticity-Robust Standard Errors (HC3)
lmtest::coeftest(
  model,
  vcov. = sandwich::vcovHC(model, type = "HC3")
)

# Functional Form and Model Specification Tests

# Ramsey RESET Test
lmtest::resettest(
  model,
  power = 2:3,
  type = "fitted"
)

# Alternative Model Including a Quadratic Inflation Term
model_quadratic <- lm(
  GDP_Growth ~ Inflation + I(Inflation^2) +
    FDI + Government_Expenditure,
  data = data
)

# Display Quadratic Model Results
summary(model_quadratic)

# Test Whether the Quadratic Term Improves Model Fit
anova(model, model_quadratic)

# Compare Model Fit Using AIC and BIC
AIC(model, model_quadratic)

BIC(model, model_quadratic)

# Compare Adjusted R-squared
summary(model)$adj.r.squared

summary(model_quadratic)$adj.r.squared

# RESET Test for the Quadratic Model
lmtest::resettest(
  model_quadratic,
  power = 2:3,
  type = "fitted"
)

# Residual Normality Diagnostics

# Shapiro-Wilk Test
shapiro.test(residuals(model))

# Jarque-Bera Test
tseries::jarque.bera.test(residuals(model))

# Histogram of Residuals
hist(
  residuals(model),
  breaks = 10,
  main = "Distribution of Regression Residuals",
  xlab = "Residuals"
)

# Normal Q-Q Plot
qqnorm(
  residuals(model),
  main = "Normal Q-Q Plot of Residuals"
)

qqline(
  residuals(model),
  col = "red",
  lwd = 2
)

# Density Plot of Residuals
plot(
  density(residuals(model)),
  main = "Kernel Density of Regression Residuals",
  xlab = "Residuals"
)

# Compare Residual Distribution with a Normal Distribution
hist(
  residuals(model),
  probability = TRUE,
  breaks = 10,
  main = "Residuals and Normal Distribution",
  xlab = "Residuals"
)

curve(
  dnorm(
    x,
    mean = mean(residuals(model)),
    sd = sd(residuals(model))
  ),
  add = TRUE,
  lwd = 2
)

# Autocorrelation Diagnostics

# Durbin-Watson Test
lmtest::dwtest(model)

# Breusch-Godfrey Test: First-Order Autocorrelation
lmtest::bgtest(model, order = 1)

# Breusch-Godfrey Test: Second-Order Autocorrelation
lmtest::bgtest(model, order = 2)

# Breusch-Godfrey Test: Fourth-Order Autocorrelation
lmtest::bgtest(model, order = 4)

# Autocorrelation Function of Regression Residuals
acf(
  residuals(model),
  main = "ACF of Regression Residuals"
)

# Partial Autocorrelation Function of Regression Residuals
pacf(
  residuals(model),
  main = "PACF of Regression Residuals"
)

# Newey-West Standard Errors
lmtest::coeftest(
  model,
  vcov. = sandwich::NeweyWest(
    model,
    lag = 1,
    prewhite = FALSE,
    adjust = TRUE
  )
)

# Outlier and Influential Observation Diagnostics

# Studentised Residuals
studentised_residuals <- rstudent(model)
studentised_residuals

# Identify Potential Outliers
which(abs(studentised_residuals) > 2)

# Identify Extreme Outliers
which(abs(studentised_residuals) > 3)

# Leverage Values
leverage_values <- hatvalues(model)
leverage_values

# Identify High-Leverage Observations
which(leverage_values > 2 * length(coef(model)) / nrow(data))

# Cook's Distance
cooks_d <- cooks.distance(model)
cooks_d

# Identify Potentially Influential Observations
which(cooks_d > 4 / nrow(data))

# Plot Studentised Residuals
plot(
  studentised_residuals,
  type = "h",
  main = "Studentised Residuals",
  xlab = "Observation",
  ylab = "Studentised Residual"
)
abline(h = c(-2, 2), lty = 2)

# Plot Leverage Values
plot(
  leverage_values,
  type = "h",
  main = "Leverage Values",
  xlab = "Observation",
  ylab = "Leverage"
)
abline(
  h = 2 * length(coef(model)) / nrow(data),
  lty = 2
)

# Plot Cook's Distance
plot(
  cooks_d,
  type = "h",
  main = "Cook's Distance",
  xlab = "Observation",
  ylab = "Cook's Distance"
)
abline(h = 4 / nrow(data), lty = 2)

# Combine Diagnostic Plots
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

# Time-Series Stationarity Tests

# Augmented Dickey-Fuller Test: GDP Growth
tseries::adf.test(data$GDP_Growth)

# Augmented Dickey-Fuller Test: Inflation
tseries::adf.test(data$Inflation)

# Augmented Dickey-Fuller Test: FDI
tseries::adf.test(data$FDI)

# Augmented Dickey-Fuller Test: Government Expenditure
tseries::adf.test(data$Government_Expenditure)

# KPSS Test: GDP Growth
tseries::kpss.test(data$GDP_Growth)

# KPSS Test: Inflation
tseries::kpss.test(data$Inflation)

# KPSS Test: FDI
tseries::kpss.test(data$FDI)

# KPSS Test: Government Expenditure
tseries::kpss.test(data$Government_Expenditure)

# First Differences
data <- data |>
  dplyr::mutate(
    GDP_Growth_Diff = c(NA, diff(GDP_Growth)),
    Inflation_Diff = c(NA, diff(Inflation)),
    FDI_Diff = c(NA, diff(FDI)),
    Government_Expenditure_Diff =
      c(NA, diff(Government_Expenditure))
  )

# ADF Tests on First Differences
tseries::adf.test(na.omit(data$GDP_Growth_Diff))

tseries::adf.test(na.omit(data$Inflation_Diff))

tseries::adf.test(na.omit(data$FDI_Diff))

tseries::adf.test(
  na.omit(data$Government_Expenditure_Diff)
)