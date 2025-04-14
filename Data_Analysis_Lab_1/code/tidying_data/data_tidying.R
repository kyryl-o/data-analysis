library(tidyverse)
library(ggplot2)
library(qqplotr)

# Read 'heart_2022_no_nans.csv' file from our directory.
raw_df <- read.csv("Data_Analysis/Data_Analysis_Lab_1/data/heart_2022_no_nans.csv", stringsAsFactors = FALSE) # nolint
df <- raw_df

# Display the structure of the dataset without any attributes.
str(df, give.attr = FALSE)
colnames(df)

# Function that find descriptive data such as min, max,
# mean, median and sd.
descriptive_data <- function(data, name) {
  cat(sprintf("%s: min: %.3f, max: %.3f, mean: %.3f, median: %.3f, sd: %.3f",
              name, min(data), max(data), mean(data), median(data), sd(data)), "\n") # nolint
}

descriptive_data(df$HeightInMeters, 'HeightInMeters') # nolint
descriptive_data(df$WeightInKilograms, 'WeightInKilograms') # nolint
descriptive_data(df$BMI, 'BMI') # nolint 
descriptive_data(df$SleepHours, 'SleepHours') # nolint
descriptive_data(df$PhysicalHealthDays, 'PhysicalHealthDays') # nolint
descriptive_data(df$MentalHealthDays, 'MentalHealthDays') # nolint

# Rename specific columns for better clarity and standardization.
df <- df %>%
  rename(DifficultyHearing = DeafOrHardOfHearing,
         DifficultyVision = BlindOrVisionDifficulty)

# Reorder and select relevant columns for better organization and readability.
df_new <- df %>%
  select(State, Sex, RaceEthnicityCategory, AgeCategory,  # Demographic data.
         HeightInMeters, WeightInKilograms, BMI,          # Anthropometric data.
         PhysicalActivities, SmokerStatus, ECigaretteUsage, AlcoholDrinkers,  # Lifestyle and habits. # nolint
         GeneralHealth, PhysicalHealthDays, MentalHealthDays, SleepHours, LastCheckupTime,  # General health indicators. # nolint
         RemovedTeeth, ChestScan,  # Other medical factors.
         everything())  # Keep all other columns in their original order.

# Remove unused veriables.
df_new <- subset(df_new, select = -c(PhysicalHealthDays, MentalHealthDays))

# Check for the number of unique keys in each column in the dataset.
check_unique_key <- df_new %>%
  summarise(across(everything(), ~list(unique(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variables", values_to = "UniqueValues") %>% # nolint
  mutate(UniqueKeys = sapply(UniqueValues, function(x) paste(x, collapse = ", "))) # nolint


# Identify the missing value indicator and change them to 'NA'.
df_new <- df_new %>%
  mutate(across(everything(), ~ ifelse(. %in% c("", " ", "NA", "NULL", "-",
                                                "-1", "999"), NA, .)))

# Identify inconsistent records where the 'SmokerStatus'
# is 'Never smoked' and 'Former smoker' but 'ECigaretteUsage' indicates related
# status of usage Cigarette.
check_smoker_status <- df_new %>%
  filter(SmokerStatus %in% c("Never smoked", "Former smoker"),
         ECigaretteUsage %in% c("Use them every day", "Use them some days", "Not at all (right now)", "Not at all (right now)")) %>% # nolint
  count(SmokerStatus, ECigaretteUsage) %>%
  pivot_longer(cols = n, values_to = "Count")
check_smoker_status

# Change the inconsistent records in the 'SmokerStatus',
# where 'ECigaretteUsagendicates' related status of usage Cigarette.
df_new <- df_new %>%
  mutate(SmokerStatus = ifelse(ECigaretteUsage == "Use them every day", # nolint
        "Current smoker - now smokes every day", SmokerStatus)) %>% # nolint
  mutate(SmokerStatus = ifelse(ECigaretteUsage == "Use them some days", # nolint
        "Current smoker - now smokes some days", SmokerStatus)) %>%  # nolint
  mutate(SmokerStatus = ifelse(ECigaretteUsage == "Not at all (right now)", # nolint
        "Former smoker", SmokerStatus)) # nolint

# Identify inconsistent records where the sex is 'Male' but diabetes history
# indicates pregnancy-related diabetes.
check_diabet_pregnancy <- df_new %>%
  filter(Sex == "Male",
         HadDiabetes == "Yes, but only during pregnancy (female)") %>%
  select(Sex, HadDiabetes)
check_diabet_pregnancy

count_diabet_pregnancy <- df_new %>%
  filter(Sex == "Male",
         HadDiabetes == "Yes, but only during pregnancy (female)")  %>%
  nrow()

percentage <- (count_diabet_pregnancy / nrow(df_new)) * 100
percentage

# Remove the inconsistent records from the original dataset 'df_new'.
df_new <- df_new %>%
  filter(!(Sex == "Male" &
             HadDiabetes == "Yes, but only during pregnancy (female)"))

# Change the inconsistent records in the 'TetanusLast10Tdap'.
df_new <- df_new %>%
  mutate(TetanusLast10Tdap = recode(TetanusLast10Tdap,
    "Yes, received Tdap" = "Yes", # nolint
    "Yes, received tetanus shot but not sure what type" = "No",
    "No, did not receive any tetanus shot in the past 10 years" = "No",
    "Yes, received tetanus shot, but not Tdap" = "No"))

# Change the inconsistent records in the 'TetanusLast10Tdap'.
df_new <- df_new %>%
  mutate(CovidPos = recode(CovidPos,
    "Tested positive using home test without a health professional" = "Yes")) # nolint

# Clean up the 'RaceEthnicityCategory' variable to standardize the format.
df_new$RaceEthnicityCategory <- df_new$RaceEthnicityCategory %>%
  gsub("([A-Za-z]+) only, Non-Hispanic", "\\1", .) %>%
  gsub("([A-Za-z]), Non-Hispanic", "\\1", .)

# Clean up the 'AgeCategory' variable to standardize the format.
df_new$AgeCategory <- df_new$AgeCategory %>%
  gsub("Age (\\d+) to (\\d+)", "\\1-\\2", .) %>%
  gsub("Age (\\d+) or older", "\\1+", .)

# Clean up the 'ECigaretteUsage' variable to standardize the format.
df_new$ECigaretteUsage <- df_new$ECigaretteUsage %>%
  gsub("Never used e-cigarettes in my entire life", "\\Never", .) %>%
  gsub("Use them every day", "\\Everyday", .) %>%
  gsub("Not at all \\(right now\\)", "\\Not at all", .) %>%
  gsub("Use them some days", "\\Somedays", .)

# Clean up the 'LastCheckupTime' variable to standardize the format.
df_new$LastCheckupTime <- df_new$LastCheckupTime %>%
  gsub("Within past year \\(anytime less than 12 months ago\\)", "1<", .) %>%
  gsub("Within past 2 years \\(1 year but less than 2 years ago\\)", "1-2", .) %>% # nolint
  gsub("Within past 5 years \\(2 years but less than 5 years ago\\)", "2-5", .) %>% # nolint
  gsub("5 or more years ago", ">5", .)

# Clean up the 'RemovedTeeth' variable to standardize the format.
df_new$RemovedTeeth <- df_new$RemovedTeeth %>%
  gsub("None of them", "0", .) %>%
  gsub("1 to 5", "1-5", .) %>% # nolint
  gsub("6 or more, but not all", "6-31", .) %>% # nolint
  gsub("All", "31+", .)

# Change the type of variables to factor and orded factor.
df_new <- df_new %>% mutate(
  State = as.factor(State),
  Sex = as.factor(Sex),
  RaceEthnicityCategory = as.factor(RaceEthnicityCategory),
  AgeCategory = as.factor(AgeCategory),
  SmokerStatus = as.factor(SmokerStatus),
  ECigaretteUsage = as.factor(ECigaretteUsage),
  GeneralHealth = as.ordered(factor(GeneralHealth, levels = c("Poor", "Fair", "Good", "Very good", "Excellent"))), # nolint
  LastCheckupTime = as.factor(LastCheckupTime),
  RemovedTeeth = as.factor(RemovedTeeth),
  ChestScan = as.factor(ChestScan),
  HadDiabetes = as.factor(HadDiabetes)
)

# Check for any columns that may contain logical values (i.e., 'Yes' or 'No').
logical_check <- df_new %>%
  summarize(across(everything(), ~ any(. %in% c("No", "Yes")))) %>%
  pivot_longer(cols = everything(), names_to = "Variables", values_to = "IsLogical") %>% # nolint
  filter(IsLogical == TRUE)
logical_check

# Modify appropriate variables to logical type (i.e., convert 'Yes'/'No' to logical TRUE/FALSE). # nolint
df_new <- df_new %>%
  mutate(across(everything(),
                ~ if (all(na.omit(.) %in% c("Yes", "No"))) . == "Yes" else .))

# Check percentage of missing data for each column in dataset.
check_missing_data <- df %>%
  summarize(across(everything(), ~ sum(is.na(.)))) %>%
  select(where(~ all(.) > 0))

if (ncol(check_missing_data) == 0) {
  print("No columns with missing values")
} else {
  check_missing_data <- check_missing_data %>%
    pivot_longer(cols = everything(), names_to = "Variables",
                 values_to = "TotalNumberOfMissedData") %>%
    mutate(PercentageOfMissingData = TotalNumberOfMissedData / nrow(df) * 100) # nolint
  check_missing_data
}

# Display the structure of the dataset without any attributes.
str(df_new, give.attr = FALSE)
colnames(df_new)

# Save the cleaned dataframe to a CSV file without row names
write.csv(df_new, "Data_Analysis/Data_Analysis_Lab_1/data/intermediate_data.csv", row.names = FALSE) # nolint

# Outliers detection.
# Read 'heart_2022_cleaned.csv' file from our directory.
raw_df <- read.csv("Data_Analysis/Data_Analysis_Lab_1/data/intermediate_data.csv", stringsAsFactors = FALSE) # nolint
df <- raw_df

# Check for any columns that may contain numeric values.
numeric_check <- df %>%
  summarize(across(everything(), ~ all(is.numeric(.)))) %>%
  select(where(~ .)) %>%
  pivot_longer(cols = everything(), names_to = "Variables",
               values_to = "IsNumeric") %>%
  mutate(IsNumeric = "num")

# Personal theme of displaying graphics.
my_theme <- theme(plot.margin = margin(1, 1, 0.55, 0.55, "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth =  0.8), # nolint
        panel.background = element_rect(fill = "gray90", color = NA),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 25)),
        axis.title.y = element_text(margin = margin(r = 25)))

# Function that build and save Q-Q plots.
qq_plot <- function(data, veriables, file_name, y_name) {
  plot <- ggplot(data, aes(sample = veriables)) +
    stat_qq_point() + stat_qq_line(color = "blue", linewidth = 0.8) +
    labs(x = "Теоретичні квантилі", y = y_name) +
    my_theme
  path_save <- paste0("Data_Analysis/Data_Analysis_Lab_1/images/", file_name, "_qq_plot.png") # nolint
  ggsave(path_save, plot, width = 20, height = 16, dpi = 100)
}

# Function that build and save logarithm Q-Q plots.
log_qq_plot <- function(data, veriables, file_name, y_name) {
  plot <- ggplot(data, aes(sample = log(veriables))) +
    stat_qq_point() + stat_qq_line(color = "blue",) + stat_qq_band(color = NA, fill = "blue", alpha = 0.3) + # nolint
    labs(x = "Теоретичні квантилі", y = y_name) +
    my_theme
  path_save <- paste0("Data_Analysis/Data_Analysis_Lab_1/images/", file_name, "_qq_plot_log.png") # nolint
  ggsave(path_save, plot, width = 20, height = 16, dpi = 100)
  plot
}

# Function that find descriptive data such as min, max,
# mean, median and sd.
descriptive_data <- function(data) {
  list(min = min(data), max = max(data), # nolint 
       mean = mean(data), median = median(data), # nolint
       sd = sd(data))
}

# Build Q-Q plot for better visualization of finding
# outliers in the variable 'HeightInMeters'.
qq_plot(df, df$HeightInMeters, "HeightInMeters", "Зріст людини")

# Finding the first ten records and the last ten records.
last_10 <- tail(sort(df$HeightInMeters, decreasing = FALSE), 10)
first_10 <- head(sort(df$HeightInMeters, decreasing = FALSE), 10)

# Build logarithm Q-Q plot for better visualization of finding
# outliers in the variable 'HeightInMeters'.
log_qq_plot(df, df$HeightInMeters, "HeightInMeters", "Логарифм зросту людини")

# Find descriptive data of veriables 'HeightInMeters'.
descriptive_data(df$HeightInMeters)

# Deleting outliers from 'HeightInMeters'.
df <- df %>%
  filter(!(HeightInMeters < 1.4 | HeightInMeters > 2.28))

# Build Q-Q plot for better visualization of finding
# outliers in the variable 'WeightInKilograms'.
qq_plot(df, df$WeightInKilograms, "WeightInKilograms", "Вага людини")

# Finding the first ten records and the last ten records.
last_10 <- tail(sort(df$WeightInKilograms, decreasing = FALSE), 10)
first_10 <- head(sort(df$WeightInKilograms, decreasing = FALSE), 10)

# Build logarithm Q-Q plot for better visualization of finding
# outliers in the variable 'WeightInKilograms'.
log_qq_plot(df, df$WeightInKilograms, "WeightInKilograms", "Логарифм ваги людини") # nolint

# Find descriptive data of veriables 'WeightInKilograms'.
descriptive_data(df$WeightInKilograms)

# Deleting outliers from 'WeightInKilograms'.
df <- df %>%
  filter(!(WeightInKilograms < 30 | WeightInKilograms > 260))

# Build Q-Q plot for better visualization of finding
# outliers in the variable 'BMI'.
qq_plot(df, df$BMI, "BMI", "Індекс маси тіла")

# Finding the first ten records and the last ten records.
last_10 <- tail(sort(df$BMI, decreasing = FALSE), 10)
first_10 <- head(sort(df$BMI, decreasing = FALSE), 10)

# Build logarithm Q-Q plot for better visualization of finding
# outliers in the variable 'BMI'.
log_qq_plot(df, df$BMI, "BMI", "Логарифм індексу маси тіла") # nolint

# Find descriptive data of veriables 'BMI'.
descriptive_data(df$BMI)

# Deleting outliers from 'BMI'.
df <- df %>%
  filter(!(BMI < 15 | BMI > 84))

# Build Q-Q plot for better visualization of finding
# outliers in the variable 'SleepHours'.
sleephours_plot <- ggplot(df, aes(x = "", y = SleepHours)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "",
       y = "Години сну") +
  my_theme

ggsave("Data_Analysis/Data_Analysis_Lab_1/images/Sleephours_qq_plot.png", sleephours_plot, width = 20, height = 16, dpi = 100) # nolint

# Find descriptive data of veriables 'SleepHours'.
descriptive_data(df$SleepHours)

# Deleting outliers from 'SleepHours'.
df <- df %>%
  filter(!(SleepHours > 16))

# Display the structure of the dataset without any attributes.
str(df, give.attr = FALSE)

write.csv(df, "Data_Analysis/Data_Analysis_Lab_1/data/latest_data.csv", row.names = FALSE) # nolint