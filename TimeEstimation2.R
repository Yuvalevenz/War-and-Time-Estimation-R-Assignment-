library(dplyr)
library(tidyr)
library(forcats)

#### Pre-Processing ----

load("TimeEstimation1.rdata")

# Convert character variables to numeric in both datasets (for merging them)
df_before <- df_before |> 
  mutate(across(c(Absolute_Error_Size, Negative_Error_Size, Positive_Error_Size, Relevancy), as.numeric))

df_during <- df_during |> 
  mutate(across(c(Absolute_Error_Size, Negative_Error_Size, Positive_Error_Size, Relevancy), as.numeric))

# Ensure unique Participant and Event IDs across studies
df_before$Participant <- paste0("S1_", df_before$Participant)  # Study 1
df_during$Participant <- paste0("S2_", df_during$Participant)  # Study 2
df_before$Event <- paste0(df_before$Event, "_S1")
df_during$Event <- paste0(df_during$Event, "_S2")

# Merge both datasets into a single df
df_combined <- bind_rows(df_before, df_during)

# Convert variables to needed types and select only relevant variables
df_combined <- df_combined |> mutate(
  across(c(Participant, Event, Study_No., Gender, Ethnicity), as_factor), # Use "forcats" package function
  across(c(Age, Anxiety_Score, Stress_Score), as.numeric)) |> # Other numeric variables were already converted before
  select(Participant, Event, Study_No., Gender, Ethnicity, Age, Anxiety_Score, Stress_Score,
         Negative_Error_Size, Positive_Error_Size, Absolute_Error_Size, Relevancy)

# Ensure Absolute Error Size is always positive
df_combined$Absolute_Error_Size <- abs(df_combined$Absolute_Error_Size)

# Save raw dataset before filtering NAs
save(df_combined, file = "TimeEstimationRaw.rdata")

# Remove NAs only from Absolute Error Size
df_filtered <- df_combined |> drop_na(Absolute_Error_Size)

# Create Study_Period (Before/During War) variable, & delete Study_No.
df_filtered <- df_filtered |> mutate(
  Study_Period = factor(Study_No., levels = c("1", "2"), labels = c("Before War", "During War"))
) |> select(-Study_No.)

# Create a binary variable for Stress Score (for logistic regression)
df_filtered <- df_filtered |> 
  mutate(Stress_Bin = factor(ifelse(Stress_Score > median(Stress_Score, na.rm = TRUE), 1, 0), 
                            levels = c(0,1), labels = c("Low", "High")))

# Load function to translate Hebrew categories into English
source("TranslationFunction.R")

# Translate categorical values for Ethnicity and Gender   
df_filtered <- Translate(
  df = df_filtered,
  var = "Ethnicity",
  values_from = c("יהודי", "ערבי", "גם וגם", "אחר"),
  values_to = c("Jewish", "Arab", "Other", "Other")
)

df_filtered <- Translate(
  df = df_filtered,
  var = "Gender",
  values_from = c("ז", "נ", "אחר"),
  values_to = c("Man", "Woman", "Other")
)  

# Create dataset with one stress score per participant (for logistic regression)
# (prevents inflation due to multiple event-level records)
df_logistic <- df_filtered |> 
  group_by(Participant) |>
  summarise(
    Participant = first(Participant),
    Study_Period = first(Study_Period),
    Stress_Score = first(Stress_Score), # Use only one Stress Score per participant
    Stress_Bin = first(Stress_Bin), # Use first occurrence for binary stress variable
    .groups = "drop") 

# View final dataset
View(df_filtered)
View(df_logistic)

# Save processed datasets
save(df_filtered, df_logistic, file = "TimeEstimation2.rdata")
write.csv(df_filtered, file = "df_filtered.csv", row.names = FALSE)
write.csv(df_logistic, file = "df_logistic.csv", row.names = FALSE)
