library(dplyr)
df <- read.csv('/Users/saiakhil/Documents/College_Related/Data_Analysis/Project1/RTA_Dataset.csv')
df[df == ""] <- NA

# To view the unique values
unique_values_all_columns <- lapply(df, unique)
print(unique_values_all_columns)

# Count the number of NA values in each column and print them.
na_counts <- sapply(df, function(x) sum(is.na(x)))
print(na_counts)

df_copy <- data.frame(df)
df_copy$Accident_severity_bin <- as.factor(df$Accident_severity)

# Create a binary indicator for missingness in 'Lanes_or_Medians'
df_copy$Lanes_or_Medians_missing <- is.na(df$Lanes_or_Medians)
# Using a chi-square test to compare the distribution of 'Accident_severity' between missing and non-missing groups of 'Lanes_or_Medians'
table_data <- table(df_copy$Lanes_or_Medians_missing,df_copy$Accident_severity_bin)
unique_values_Lanes_or_Medians_missing <- unique(df_copy$Lanes_or_Medians_missing)
print(unique_values_Lanes_or_Medians_missing)
chisq.test(table_data)

# Create a binary indicator for missingness in 'Educational_level'
df_copy$Educational_level_missing <- is.na(df$Educational_level)
# Using a chi-square test to compare the distribution of 'Accident_severity' between missing and non-missing groups of 'Educational_level'
table_data <- table(df_copy$Educational_level_missing,df_copy$Accident_severity_bin)
unique_values_Educational_level_missing <- unique(df_copy$Educational_level_missing)
print(unique_values_Educational_level_missing)
chisq.test(table_data)

# Create a binary indicator for missingness in 'Owner_of_vehicle'
df_copy$Owner_of_vehicle_missing <- is.na(df$Owner_of_vehicle)
# Using a chi-square test to compare the distribution of 'Accident_severity' between missing and non-missing groups of 'Owner_of_vehicle'
table_data <- table(df_copy$Owner_of_vehicle_missing,df_copy$Accident_severity_bin)
unique_values_Owner_of_vehicle_missing <- unique(df_copy$Owner_of_vehicle_missing)
print(unique_values_Owner_of_vehicle_missing)
chisq.test(table_data)
  

