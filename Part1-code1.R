# read the csv file
data <- read.csv("~/Development/RTA_Data.csv", header = TRUE)
# remove rows with missing values in Driving_experience and Accident_severity
data <- na.omit(data[, c("Driving_experience", "Accident_severity")])
# check the unique values in Driving_experience and Accident_severity
unique(data$Driving_experience)
unique(data$Accident_severity)
# continue to exclude unwanted values "unknown" and "" in Driving_experience
data <- data[data$Driving_experience != "unknown", ]
data <- data[data$Driving_experience != "", ]
# check the unique values in Driving_experience again
unique(data$Driving_experience)

# transform the values in Driving_experience and Accident_severity for easier interpretation
data$Driving_experience <- factor(data$Driving_experience, levels = c("No Licence", "Below 1yr", "1-2yr", "2-5yr", "5-10yr", "Above 10yr"))
data$Accident_severity <- factor(data$Accident_severity, levels = c("Slight Injury", "Serious Injury", "Fatal injury"))
# create a contingency table for Driving_experience and Accident_severity
contingency_table <- table(data$Driving_experience, data$Accident_severity)
# print the contingency table
print(contingency_table)
# perform the chi-square test
chi_square_result <- chisq.test(contingency_table, simulate.p.value = TRUE)
# print the result
print(chi_square_result)

# check the proportions of Accident_severity within each level of Driving_experience
proportions <- prop.table(contingency_table, margin = 1)
# print the proportions
print(proportions)
# install and load the ggplot2 package
install.packages("ggplot2")
library(ggplot2)
# plot the graph presenting the proportions of Accident_severity within each level of Driving_experience
proportions <- as.data.frame(prop.table(table(data$Driving_experience, data$Accident_severity), margin = 1))
proportions$Driving_experience <- factor(proportions$Var1, levels = c("No Licence", "Below 1yr", "1-2yr", "2-5yr", "5-10yr", "Above 10yr"))
ggplot(proportions, aes(x = Driving_experience, y = Freq, color = Var2, group = Var2)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Proportions of Accident Severity by Driving Experience", x = "Driving Experience", y = "Proportion") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_manual(values = c("lightblue", "lightcoral", "yellow"))







