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


# Modelling using GLM
glm_model <- glm(Heavy_Injury_True ~ Lanes_or_Medians + Educational_level + Owner_of_vehicle 
                 + Weather_conditions + Light_conditions,
                 data = df_glm, family = "binomial")
# Summary of the model
summary(glm_model)

# Fit a null model (Model 1)
null_model <- glm(Heavy_Injury_True ~ 1, family = binomial, data = df_glm)
# Perform the Likelihood Ratio Test
lrt_result <- lrtest(null_model, glm_model)
print(lrt_result)


introduce_missing_values <- function(data_frame, percentage) {
  for (col in names(data_frame[,-ncol(data_frame)])) {
    total_elements <- length(data_frame[[col]])
    num_missing <- round(percentage * total_elements / 100)
    # Randomly select indices to introduce missing values
    indices_to_null <- sample(seq_along(data_frame[[col]]), num_missing)
    # Introduce missing values (set selected indices to NA)
    data_frame[[col]][indices_to_null] <- NA
  }
  
  return(data_frame)
}
set.seed(369)
perc_list = list(10,20,30,40,50)
for (p in perc_list){
  cat("Results for",p,"% Null value intoduction: \n")
  df_10 <- introduce_missing_values(df_glm, p)
  glm_model <- glm(Heavy_Injury_True ~ Lanes_or_Medians + Educational_level + 
                     Owner_of_vehicle + Weather_conditions + Light_conditions,
                   data = df_10, family = "binomial")
  summ<-summary(glm_model)
  print(summ)
  cat("\n")}


cross_tab <- table(df_glm$Light_conditions, df_glm$Heavy_Injury_True)
cross_tab_with_totals <- addmargins(cross_tab)
print(cross_tab_with_totals)
cross_tab_percentages <- round(prop.table(cross_tab_with_totals, margin = 1) * 100 * 2,1)
print(cross_tab_percentages)


# In df_glm, make the add null values to column Light_conditions where the weather conditions 
# are Darkness - no Lighting.
df_glm_no_light <- df_glm %>%
  mutate(Light_conditions = ifelse(Light_conditions == "Darkness - no lighting", NA, Light_conditions))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

# Fill null values with the mode
df_glm_filled <- df_glm_no_light %>%
  mutate(Light_conditions = ifelse(is.na(Light_conditions), Mode(Light_conditions), Light_conditions))

cross_tab <- table(df_glm_filled$Light_conditions, df_glm_filled$Heavy_Injury_True)
cross_tab_with_totals <- addmargins(cross_tab)
print(cross_tab_with_totals)
cross_tab_percentages <- round(prop.table(cross_tab_with_totals, margin = 1) * 100 * 2,1)
print(cross_tab_percentages)


cat("Results after introduction of Null values not at random: \n")
glm_model <- glm(Heavy_Injury_True ~ Lanes_or_Medians + Educational_level + Owner_of_vehicle + 
                   Weather_conditions + Light_conditions,
                 data = df_glm_filled, family = "binomial")
summ<-summary(glm_model)
print(summ)



