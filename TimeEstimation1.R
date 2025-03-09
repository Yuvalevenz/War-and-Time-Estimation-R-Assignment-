library(dplyr)
library(ggdist)
library(ggpubr)
library(ggplot2)
library(readr)

#### Descriptive statistics and data exploration (Question A2) ----

# Create dataframes for each dataset (before & during the war)
df_before <- read.csv("Study 1 analysis.csv")
df_during <- read.csv("Study 2 analysis.csv")

# Create dataframes with one row per participant (to remove duplicates in stress, anxiety & ethnicity)
df_before_unique <- df_before |>
  group_by(Participant) |>
  summarise(Gender = first(Gender), Ethnicity = first(Ethnicity), Age = first(Age),
            Anxiety_Score = first(Anxiety_Score),Stress_Score = first(Stress_Score), .groups = "drop")

df_during_unique <- df_during |>
  group_by(Participant) |>
  summarise(Gender = first(Gender), Ethnicity = first(Ethnicity), Age = first(Age),
            Anxiety_Score = first(Anxiety_Score), Stress_Score = first(Stress_Score), .groups = "drop")

# View & summarize dfs
View(df_before)
View(df_before_unique)
View(df_during)
View(df_during_unique)

summary(df_before_unique)
summary(df_during_unique)

# Summarize Ethnicity & Gender groups (Before War)
desc_before <- df_before_unique |>
  group_by(Ethnicity, Gender) |>
  summarise(Count = n(), .groups = "drop")

# Count Ethnicity & Gender groups (During War)
desc_during <- df_during_unique |>
  group_by(Ethnicity, Gender) |>
  summarise(Count = n(), .groups = "drop")

# Display summaries
desc_before
desc_during

# Use ggplot & ggdist to display variables
ggplot(df_during_unique |> filter(Ethnicity != "גם וגם"), # Filter out "גם וגם" due to insufficient observations for this ethnic group
       aes(x = Ethnicity, y = Anxiety_Score, fill = Ethnicity)) +
  stat_halfeye(justification = -0.15, adjust = 0.5, width = 0.7, .width = c(0.5, 0.9)) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8) +
  stat_dots(side = "left", dotsize = 0.4) +
  labs(title = "Anxiety Scores by Ethnicity (During War)", y = "Anxiety Score") +
  theme_bw()

ggplot(df_during_unique |> filter(Ethnicity != "גם וגם"),  # Filter out "גם וגם" due to insufficient observations for this ethnic group
       aes(x = Ethnicity, y = Stress_Score, fill = Ethnicity)) +
  stat_halfeye(justification = -0.15, adjust = 0.5, width = 0.7, .width = c(0.5, 0.9)) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8) +
  stat_dots(side = "left", dotsize = 0.4) +
  labs(title = "Stress Scores by Ethnicity (During War)", y = "Stress Score") + 
  theme_bw()
                        
ggplot(df_during_unique |> filter(Gender != "אחר"), # Exclude "other" due to a small sample size
       aes(x = Gender, y = Stress_Score, fill = Gender)) +
  geom_boxplot(color = "black") + 
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.7) +
  labs(title = "Stress Scores by Gender (During War)", y = "Stress Score") + 
  theme_bw()

# Convert variable to numeric (some values may have been read as characters)
df_during <- df_during |> mutate(across(Absolute_Error_Size, as.numeric))

ggplot(df_during, aes(x = Absolute_Error_Size, fill = Ethnicity)) + 
  geom_histogram(alpha = 0.4, bins = 30, position = "identity") +  
  labs(title = "Absolute Error Size by Ethnicity (During War)", x = "Absolute Error Size", y = "Count", fill = "Ethnicity") +
  xlim(c(0,500)) +
  theme_minimal()

# Save dfs
save(df_before, df_during, df_before_unique, df_during_unique, file = "TimeEstimation1.rdata")
