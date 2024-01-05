library(car)
library(nnet)
library(dplyr)
library(caret)

df <- read.csv('/Users/saiakhil/Documents/College_Related/Data_Analysis/Project1/RTA_Dataset.csv')
df[df == ""] <- NA
df_glm <- df %>%
  select( Lanes_or_Medians ,Educational_level , Owner_of_vehicle ,Weather_conditions , Light_conditions,Accident_severity) %>%
  na.omit()
cat("Total Rows in Data Frame:",nrow(df_glm),ncol(df_glm))

df_glm$Accident_severity <- ifelse(df_glm$Accident_severity %in% c('Serious Injury', 'Fatal injury'), 'Heavy Injury', 'Slight Injury')
df_glm$Heavy_Injury_True <- as.integer(df_glm$Accident_severity == 'Heavy Injury')

df_glm <- df_glm[, !names(df_glm) %in% c("Accident_severity")]
df_glm<- df_glm[,c( "Lanes_or_Medians" ,"Educational_level" , "Owner_of_vehicle" ,"Weather_conditions" , "Light_conditions","Heavy_Injury_True")]

# Fit the linear regression model
lm_model <- lm(Heavy_Injury_True ~ Lanes_or_Medians + Educational_level + 
                 Owner_of_vehicle + Weather_conditions + Light_conditions, data = df_glm)
# Calculate VIF
vif_values <- vif(lm_model)
print(vif_values)



