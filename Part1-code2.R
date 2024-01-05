# read the csv file
data <- read.csv("~/Development/RTA_Data.csv", header = TRUE)
# check the unique values in Driving_experience and Accident_severity
unique(data$Driving_experience)
unique(data$Accident_severity)
# replace "unknown" and "" with "undefined" in Driving_experience for easier manipulation
data$Driving_experience[data$Driving_experience %in% c("unknown", "")] <- "undefined"
# recheck the unique values in Driving_experience again
unique(data$Driving_experience)
# write the updated data to a new csv file for later use
write.csv(data, "modified_data.csv", row.names = FALSE)

# read the modified csv file
# reminder: you may have to move the modified_data.csv file by hand in advance to your workspace if they are the same as your R directory
modified_data <- read.csv("~/Development/modified_data.csv", header = TRUE)
# calculate the proportions of six useful values in Driving_experience by completely omitting the "undefined" values in the modified dataset
temp_data <- modified_data[modified_data$Driving_experience != "undefined", ]
proportion <- prop.table(table(temp_data$Driving_experience))
# check the proportions
print(proportion)
# identify the indices of all "undefined" values in the modified dataset
undefined_indices <- which(modified_data$Driving_experience == "undefined")
# set a seed for reproducibility
set.seed(123)

#--------------- 10% ---------------
# calculate the number of "undefined" values that need to be reassign (10% of total undefined values)
num_to_reassign10 <- round(0.10 * length(undefined_indices))
# randomly select 10% of the "undefined" values by deciding their indices
randomly_selected_indices10 <- sample(undefined_indices, num_to_reassign10)
# reassign the selected to one of the six useful values based on their proportions respectively
new_values10 <- sample(names(proportion), size = num_to_reassign10, prob = proportion, replace = TRUE)
modified_data$Driving_experience[randomly_selected_indices10] <- new_values10
#omit the other 90% "undefined" values in Driving_experience
modified_data <- modified_data[modified_data$Driving_experience != "undefined", ]
# write the updated data to a new csv file for later use
write.csv(modified_data, "modified10_data.csv", row.names = FALSE)

# read the modified10 csv file
# reminder: you may have to move the modified10_data.csv file by hand in advance to your workspace if they are the same as your R directory
modified10_data <- read.csv("~/Development/modified10_data.csv", header = TRUE)
# transform the values in Driving_experience and Accident_severity for easier interpretation
modified10_data$Driving_experience <- factor(modified10_data$Driving_experience, levels = c("No Licence", "Below 1yr", "1-2yr", "2-5yr", "5-10yr", "Above 10yr"))
modified10_data$Accident_severity <- factor(modified10_data$Accident_severity, levels = c("Slight Injury", "Serious Injury", "Fatal injury"))
# create a contingency table for Driving_experience and Accident_severity
contingency_table10 <- table(modified10_data$Driving_experience, modified10_data$Accident_severity)
# print the table
print(contingency_table10)
# perform the chi-square test
chi_square_result10 <- chisq.test(contingency_table10, simulate.p.value = TRUE)
# print the result
print(chi_square_result10)


# --------------- 20% ---------------  
# read the modified csv file again
modified_data <- read.csv("~/Development/modified_data.csv", header = TRUE)
# calculate the number of "undefined" values that need to be reassign (20% of total undefined values)
num_to_reassign20 <- round(0.20 * length(undefined_indices))
# randomly select 20% of the "undefined" values by deciding their indices
randomly_selected_indices20 <- sample(undefined_indices, num_to_reassign20)
# reassign the selected to one of the six useful values based on their proportions respectively
new_values20 <- sample(names(proportion), size = num_to_reassign20, prob = proportion, replace = TRUE)
modified_data$Driving_experience[randomly_selected_indices20] <- new_values20
#omit the other 80% "undefined" values in Driving_experience
modified_data <- modified_data[modified_data$Driving_experience != "undefined", ]
# write the updated data to a new csv file for later use
write.csv(modified_data, "modified20_data.csv", row.names = FALSE)

# read the modified20 csv file
# reminder: you may have to move the modified20_data.csv file by hand in advance to your workspace if they are the same as your R directory
modified20_data <- read.csv("~/Development/modified20_data.csv", header = TRUE)
# transform the values in Driving_experience and Accident_severity for easier interpretation
modified20_data$Driving_experience <- factor(modified20_data$Driving_experience, levels = c("No Licence", "Below 1yr", "1-2yr", "2-5yr", "5-10yr", "Above 10yr"))
modified20_data$Accident_severity <- factor(modified20_data$Accident_severity, levels = c("Slight Injury", "Serious Injury", "Fatal injury"))
# create a contingency table for Driving_experience and Accident_severity
contingency_table20 <- table(modified20_data$Driving_experience, modified20_data$Accident_severity)
# print the table
print(contingency_table20)
# perform the chi-square test
chi_square_result20 <- chisq.test(contingency_table20, simulate.p.value = TRUE)
# print the result
print(chi_square_result20)



# --------------- 30% ---------------
# read the modified csv file again
modified_data <- read.csv("~/Development/modified_data.csv", header = TRUE)
# calculate the number of "undefined" values that need to be reassign (30% of total undefined values)
num_to_reassign30 <- round(0.30 * length(undefined_indices))
# randomly select 30% of the "undefined" values by deciding their indices
randomly_selected_indices30 <- sample(undefined_indices, num_to_reassign30)
# reassign the selected to one of the six useful values based on their proportions respectively
new_values30 <- sample(names(proportion), size = num_to_reassign30, prob = proportion, replace = TRUE)
modified_data$Driving_experience[randomly_selected_indices30] <- new_values30
#omit the other 70% "undefined" values in Driving_experience
modified_data <- modified_data[modified_data$Driving_experience != "undefined", ]
# write the updated data to a new csv file for later use
write.csv(modified_data, "modified30_data.csv", row.names = FALSE)

# read the modified30 csv file
# reminder: you may have to move the modified30_data.csv file by hand in advance to your workspace if they are the same as your R directory
modified30_data <- read.csv("~/Development/modified30_data.csv", header = TRUE)
# transform the values in Driving_experience and Accident_severity for easier interpretation
modified30_data$Driving_experience <- factor(modified30_data$Driving_experience, levels = c("No Licence", "Below 1yr", "1-2yr", "2-5yr", "5-10yr", "Above 10yr"))
modified30_data$Accident_severity <- factor(modified30_data$Accident_severity, levels = c("Slight Injury", "Serious Injury", "Fatal injury"))
# create a contingency table for Driving_experience and Accident_severity
contingency_table30 <- table(modified30_data$Driving_experience, modified30_data$Accident_severity)
# print the table
print(contingency_table30)
# perform the chi-square test
chi_square_result30 <- chisq.test(contingency_table30, simulate.p.value = TRUE)
# print the result
print(chi_square_result30)


# --------------- 40% ---------------
# read the modified csv file again
modified_data <- read.csv("~/Development/modified_data.csv", header = TRUE)
# calculate the number of "undefined" values that need to be reassign (40% of total undefined values)
num_to_reassign40 <- round(0.40 * length(undefined_indices))
# randomly select 40% of the "undefined" values by deciding their indices
randomly_selected_indices40 <- sample(undefined_indices, num_to_reassign40)
# reassign the selected to one of the six useful values based on their proportions respectively
new_values40 <- sample(names(proportion), size = num_to_reassign40, prob = proportion, replace = TRUE)
modified_data$Driving_experience[randomly_selected_indices40] <- new_values40
#omit the other 60% "undefined" values in Driving_experience
modified_data <- modified_data[modified_data$Driving_experience != "undefined", ]
# write the updated data to a new csv file for later use
write.csv(modified_data, "modified40_data.csv", row.names = FALSE)

# read the modified40 csv file
# reminder: you may have to move the modified40_data.csv file by hand in advance to your workspace if they are the same as your R directory
modified40_data <- read.csv("~/Development/modified40_data.csv", header = TRUE)
# transform the values in Driving_experience and Accident_severity for easier interpretation
modified40_data$Driving_experience <- factor(modified40_data$Driving_experience, levels = c("No Licence", "Below 1yr", "1-2yr", "2-5yr", "5-10yr", "Above 10yr"))
modified40_data$Accident_severity <- factor(modified40_data$Accident_severity, levels = c("Slight Injury", "Serious Injury", "Fatal injury"))
# create a contingency table for Driving_experience and Accident_severity
contingency_table40 <- table(modified40_data$Driving_experience, modified40_data$Accident_severity)
# print the table
print(contingency_table40)
# perform the chi-square test
chi_square_result40 <- chisq.test(contingency_table40, simulate.p.value = TRUE)
# print the result
print(chi_square_result40)


# --------------- 50% ---------------
# read the modified csv file again
modified_data <- read.csv("~/Development/modified_data.csv", header = TRUE)
# calculate the number of "undefined" values that need to be reassign (50% of total undefined values)
num_to_reassign50 <- round(0.50 * length(undefined_indices))
# randomly select 50% of the "undefined" values by deciding their indices
randomly_selected_indices50 <- sample(undefined_indices, num_to_reassign50)
# reassign the selected to one of the six useful values based on their proportions respectively
new_values50 <- sample(names(proportion), size = num_to_reassign50, prob = proportion, replace = TRUE)
modified_data$Driving_experience[randomly_selected_indices50] <- new_values50
#omit the other 50% "undefined" values in Driving_experience
modified_data <- modified_data[modified_data$Driving_experience != "undefined", ]
# write the updated data to a new csv file for later use
write.csv(modified_data, "modified50_data.csv", row.names = FALSE)

# read the modified50 csv file
# reminder: you may have to move the modified50_data.csv file by hand in advance to your workspace if they are the same as your R directory
modified50_data <- read.csv("~/Development/modified50_data.csv", header = TRUE)
# transform the values in Driving_experience and Accident_severity for easier interpretation
modified50_data$Driving_experience <- factor(modified50_data$Driving_experience, levels = c("No Licence", "Below 1yr", "1-2yr", "2-5yr", "5-10yr", "Above 10yr"))
modified50_data$Accident_severity <- factor(modified50_data$Accident_severity, levels = c("Slight Injury", "Serious Injury", "Fatal injury"))
# create a contingency table for Driving_experience and Accident_severity
contingency_table50 <- table(modified50_data$Driving_experience, modified50_data$Accident_severity)
# print the table
print(contingency_table50)
# perform the chi-square test
chi_square_result50 <- chisq.test(contingency_table50, simulate.p.value = TRUE)
# print the result
print(chi_square_result50)





