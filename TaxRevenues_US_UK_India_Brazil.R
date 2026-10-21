install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

# Data Loading
data <- read.csv("C:/R Data/Model_R_Dataset.csv")

head(data)
str(data)
names(data)

# Descriptive Statistical Analysis

# Mean Calculation
mean(data$Tax.Revenue, na.rm = TRUE)
mean(data$Gini.Index, na.rm = TRUE)

# Median Calculation
median(data$Tax.Revenue, na.rm = TRUE)
median(data$Gini.Index, na.rm = TRUE)

# Standard Deviation Calculation
sd(data$Tax.Revenue, na.rm = TRUE)
sd(data$Gini.Index, na.rm = TRUE)

# Variance Calculation
var(data$Tax.Revenue, na.rm = TRUE)
var(data$Gini.Index, na.rm = TRUE)

# Summary Statistics
summary(data)
summary(data[, c("Tax.Revenue", "Gini.Index")])

# Tax Revenue vs Gini Index
plot(data$Tax.Revenue, data$Gini.Index,
     main = "Tax Revenue vs Income Inequality",
     xlab = "Tax Revenue",
     ylab = "Gini Index",
     pch = 19,
     col = "blue")

abline(lm(Gini.Index ~ Tax.Revenue, data = data),
       col = "darkblue",
       lwd = 2)

# Health Expenditure vs Gini Index

plot(data$Health.Expenditure, data$Gini.Index,
     main = "Health Expenditure vs Income Inequality",
     xlab = "Health Expenditure",
     ylab = "Gini Index",
     pch = 19,
     col = "green")

abline(lm(Gini.Index ~ Health.Expenditure, data = data),
       col = "darkgreen",
       lwd = 2)

# Education vs Gini Index

plot(data$Education, data$Gini.Index,
     main = "Education vs Income Inequality",
     xlab = "Education",
     ylab = "Gini Index",
     pch = 19,
     col = "purple")

abline(lm(Gini.Index ~ Education, data = data),
       col = "darkred",
       lwd = 2)

# Load ggplot2
library(ggplot2)

# Tax Revenue vs Gini Index by Country
ggplot(data, aes(x = Tax.Revenue, y = Gini.Index, color = Country)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tax Revenue vs Income Inequality",
       x = "Tax Revenue",
       y = "Gini Index") +
  theme_minimal()

# Education vs Gini Index by Country

ggplot(data, aes(x = Education, y = Gini.Index, color = Country)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Education vs Income Inequality",
       x = "Education",
       y = "Gini Index") +
  theme_minimal()

# Correlation Analysis

# Variable Selection
vars <- data[, c("Tax.Revenue",
                 "Health.Expenditure",
                 "Education",
                 "Gini.Index")]

# Correlation Matrix Calculation
cor_matrix <- cor(vars,
                  use = "complete.obs",
                  method = "pearson")

# Display Correlation Matrix
cor_matrix

# Regression Analysis

# Multiple Linear Regression
model_all <- lm(Gini.Index ~ Tax.Revenue +
                  Health.Expenditure +
                  Education,
                data = data)

# Regression Results
summary(model_all)

# Hypothesis Testing

# Simple Linear Regression
model_tax <- lm(Gini.Index ~ Tax.Revenue, data = data)

# Regression Summary
summary_tax <- summary(model_tax)

# Coefficient Table
coeff_df <- as.data.frame(summary_tax$coefficients)

colnames(coeff_df) <- c("Estimate",
                        "Std_Error",
                        "t_value",
                        "p_value")

# Significance Level
alpha <- 0.05

# Hypothesis Testing Interpretation
coeff_df$Interpretation <- ifelse(
  coeff_df$p_value < alpha,
  "Reject H0 - Significant effect",
  "Fail to reject H0 - No significant effect"
)

# Display Results
coeff_df