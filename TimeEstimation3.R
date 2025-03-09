library(pROC)
library(ggplot2)
library(ggpubr)
library(effects)
library(dplyr)

#### Data Analysis ----

# Load & view datasets
load("TimeEstimation2.rdata")
View(df_filtered)
View(df_logistic)

# Exclude "Other" Ethnicity (n = 4) to prevent unreliable estimates in MLR
df_filtered <- df_filtered |> filter(Ethnicity != "Other")

# Multiple Linear Regression Model
mlr_model <- lm(Absolute_Error_Size ~ Study_Period + Relevancy + Ethnicity, data = df_filtered, na.action = na.exclude)
summary(mlr_model)

# Logistic Regression Model
logistic_model <- glm(Stress_Bin ~ Study_Period, data = df_logistic, family = binomial, na.action = na.exclude)
summary(logistic_model)

# Convert log-odds coefficient to odds ratios for interpretability
exp(coef(logistic_model))

# Get predicted probabilities for logistic regression
df_logistic$predicted_prob <- predict(logistic_model, type = "response")

#### Display results for MLR model ----

# Multiple Linear Regression effects plot
plot(allEffects(mlr_model))

# Boxplot of Study Period effect on Absolute Error Size
ggplot(df_filtered, aes(x = Study_Period, y = Absolute_Error_Size, fill = Study_Period)) +
  geom_boxplot(alpha = 0.6) +
  stat_compare_means(method = "t.test", label = "p.signif") +  # Add p-value significance
  labs(title = "Effect of Study Period on Absolute Error Size",
       x = "Study Period",
       y = "Absolute Error Size") +
  theme_bw()

#### Display results for Logistic Regression model ----

# Generate predicted probabilities with confidence intervals for logistic regression
effect_data <- as.data.frame(Effect("Study_Period", logistic_model))

# Plot predicted probability of high stress by Study Period 
ggplot(effect_data, aes(x = Study_Period, y = fit)) +
  geom_point(size = 3, color = "blue") +  # Plot the estimated probabilities
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +  # Add confidence intervals
  labs(title = "Effect of Study Period on Predicted Probability of High Stress",
       x = "Study Period",
       y = "Predicted Probability of High Stress") +
  theme_bw()

# Create & plot ROC Curve for Logistic Regression
roc_curve <- roc(df_logistic$Stress_Bin, df_logistic$predicted_prob)
auc_value = auc(roc_curve)
plot(roc_curve, col = "blue", main = paste0("ROC Curve (AUC = ", round(auc_value, 2), ")"))

# Compute and display mean Stress Score per Study Period
df_logistic |> 
  group_by(Study_Period) |> 
  summarise(Mean_Stress = mean(Stress_Score, na.rm = TRUE))
