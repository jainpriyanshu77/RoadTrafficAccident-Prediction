library(dplyr)
library(lmtest)
df <- read.csv('/Users/saiakhil/Documents/College_Related/Data_Analysis/Project1/RTA_Dataset.csv')
df[df == ""] <- NA

cat("Total Rows in Data Frame:",nrow(df),ncol(df))

df_glm <- df %>%
  select( Lanes_or_Medians ,Educational_level , Owner_of_vehicle ,Weather_conditions , Light_conditions,Accident_severity) %>%
  na.omit()

cat("Rows in Data Frame after removing all the null values:",nrow(df_glm),ncol(df_glm))

df_glm$Accident_severity <- ifelse(df_glm$Accident_severity %in% c('Serious Injury', 'Fatal injury'),
                                   'Heavy Injury', 'Slight Injury')
df_glm$Heavy_Injury_True <- as.integer(df_glm$Accident_severity == 'Heavy Injury')
df_glm<- df_glm[,c( "Lanes_or_Medians" ,"Educational_level" , "Owner_of_vehicle" ,
                    "Weather_conditions" , "Light_conditions","Heavy_Injury_True")]


cross_tab <- table(df_glm$Light_conditions, df_glm$Heavy_Injury_True)
cross_tab_with_totals <- addmargins(cross_tab)
cross_tab_percentages <- round(prop.table(cross_tab_with_totals, margin = 1) * 100 * 2,1)
print(cross_tab_percentages)



