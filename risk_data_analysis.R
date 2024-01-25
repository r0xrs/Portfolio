library(rio)
library(tidyverse)
library(tidyr)
library(zoo)
library(ggplot2)
library(car)
library(leaps)


###############################
###############################       DATA PREPROCESSING
###############################


# Reading the data and combining it to the list of 7 dataframes

data <- import_list("HT_Risk_Model_Analyst.xlsx")

# Having looked through the dataset, I have noticed that there was a mistake. All of the macroeconomic variables that contained
# quarterly data had '2004-Q4' written as '2004Q4'. Providing that the rest of the data was written in format 'YEAR-Q...', I
# have changed it so that it does not affect the further data manipulation.

data <- lapply(data, function(df) {
  df[df == "2004Q4"] <- "2004-Q4"
  return(df)
})

# Extracting dataframes from list to separate ones for easier handling

gdp <- data$GDP
ur <- data$UR
hpi <- data$HPI
hicp <- data$HICP
euribor_6m <- data$EURIBOR_6M
wage <- data$WAGE
riskLevel <- data$RiskLevel

# Changing 'Quarter' format from 'chr' to proper quarterly data format in order to be able to perform operations with date

gdp$Quarter <- as.yearqtr(gdp$Quarter, format = "%Y-Q%q")
ur$Quarter <- as.yearqtr(ur$Quarter, format = "%Y-Q%q")
hpi$Quarter <- as.yearqtr(hpi$Quarter, format = "%Y-Q%q")
hicp$Month <- as.yearqtr(hicp$Month, format = "%Y-%m")
euribor_6m$Quarter <- as.yearqtr(euribor_6m$Quarter, format = "%Y-Q%q")
wage$Quarter <- as.yearqtr(wage$Quarter, format = "%Y-Q%q")
riskLevel$Quarter <- as.yearqtr(riskLevel$Quarter, format = "%Y-Q%q")

# Regarding 'hicp': as we had monthly data for HICP, I have converted it to quarterly. 
# Next up, I have calculated the sum for each group (as there were three rows for each quarter representing 3 months)

hicp <- hicp %>%
  group_by(Month) %>%
  summarise(across(EE:LT, sum))
colnames(hicp)[1] <- "Quarter" # changing column name ('Month' to 'Quarter')

# Providing that the data in 'riskLevel' is unusable up until '2006 Q1', I have decided to keep the data from '2006 Q1' to
# '2022 Q4' in all of the dataframes (tibbles)

gdp <- gdp %>% 
  subset(Quarter > "2005 Q4") %>% group_by(Quarter)
ur <- ur %>% 
  subset(Quarter > "2005 Q4") %>% group_by(Quarter)
hpi <- hpi %>% 
  subset(Quarter > "2005 Q4") %>% group_by(Quarter)
hicp <- hicp %>% 
  subset(Quarter > "2005 Q4") %>% group_by(Quarter)
euribor_6m <- euribor_6m %>% 
  subset(Quarter > "2005 Q4") %>% group_by(Quarter)
wage <- wage %>% 
  subset(Quarter > "2005 Q4") %>% group_by(Quarter)
riskLevel <- riskLevel %>% 
  subset(Quarter > "2005 Q4") %>% group_by(Quarter)

# Adding a suffix to all of the columns so that it would be possible to know which is which after merging the tibbles (dataframes)

names(gdp)[-1] <- paste0(names(gdp)[-1], "_gdp")
names(ur)[-1] <- paste0(names(ur)[-1], "_ur")
names(hpi)[-1] <- paste0(names(hpi)[-1], "_hpi")
names(hicp)[-1] <- paste0(names(hicp)[-1], "_hicp")
names(euribor_6m)[-1] <- paste0(names(euribor_6m)[-1], "_euribor_6m")
names(wage)[-1] <- paste0(names(wage)[-1], "_wage")
names(riskLevel)[-1] <- paste0(names(riskLevel)[-1], "_riskLevel")

# Merging the tibbles into a single dataframe

data_list <- list(gdp, ur, hpi, hicp, euribor_6m, wage, riskLevel)

merge_by_quarter <- function(df1, df2) {
  merged <- merge(df1, df2, by = "Quarter", all.x = TRUE, all.y = TRUE)
  return(merged)
}

data2 <- Reduce(merge_by_quarter, data_list)


###############################
###############################       EXPLORATORY DATA ANALYSIS
###############################


summary(data2, digits = 2)

# Preparing data for plotting

riskData <- data.frame(Quarter = data2$Quarter, EE = data2$EE_riskLevel * 100, LV = data2$LV_riskLevel * 100,
                       LT = data2$LT_riskLevel * 100)

# Converting from wide to long format
riskData_long <- gather(riskData, key = "country", value = "value", -Quarter)

# Making sure that the order is as in the dataset
riskData_long$country <- factor(riskData_long$country, levels = c("EE", "LV", "LT"))


ggplot(riskData_long, aes(x = value, fill = country)) +
  geom_density(alpha = 0.5) +
  scale_fill_discrete(labels = c('Estonia', 'Latvia', 'Lithuania'), name = "") +
  labs(title = "Density Plot of Risk Level by Country",
       x = "Risk Level, %",
       y = "Density") +
  theme_bw()


###############################
###############################       REGRESSION MODEL
###############################


# Splitting the data into training and testing sets

# For reproducability

set.seed(123)

train_indices <- sample(1:nrow(data2), 0.7 * nrow(data2))  # 70% for training

training_set <- data2[train_indices, ]

# Excluding the training set rows to create testing set
testing_set <- data2[-train_indices, ]

######################## ######################## ######################## 

########################         ESTONIA
######################## ######################## ########################


# Checking for multicolinearity among independent macroeconomic variables

data_EE <- data.frame(gdp = data2$EE_gdp, ur = data2$EE_ur, hicp = data2$EE_hicp,
                      euribor_6m = data2$EE_euribor_6m, wage = data2$EE_wage)
cor_matrix_EE <- cor(data_EE)
cor_matrix_EE


# Writing a full model

model_EE_1 <- lm(EE_riskLevel ~ EE_gdp + EE_ur + EE_hicp +
               EE_euribor_6m + EE_wage + EE_gdp*EE_ur + EE_gdp*EE_hicp + EE_gdp*EE_euribor_6m +
               EE_gdp*EE_wage + EE_ur*EE_hicp + EE_ur*EE_euribor_6m + EE_ur*EE_wage +
               EE_hicp*EE_euribor_6m + EE_hicp*EE_wage + EE_euribor_6m*EE_wage, data = training_set)

# I did not include HPI as it did not seem to be of use

# Using 'step()' function in order to determine the best combination of variables to use in a model

selected_model_EE <- step(model_EE_1)
summary(selected_model_EE)

# Final model (after excluding coefficients which were statiscally insignificant):

model_EE_2 <- lm(EE_riskLevel ~ EE_ur + EE_euribor_6m + EE_wage + EE_ur:EE_hicp, 
                 data = training_set)
summary(model_EE_2)

# Providing that the 'p-value' for all of the coefficients is < 0.05, we say that they are 
# statistically significant

# Making predictions on the testing set

predictions_EE <- predict(model_EE_2, newdata = testing_set[, c("EE_gdp", "EE_ur", "EE_hicp", "EE_euribor_6m", "EE_wage")])
predictions_EE

# Calculate the RMSE (Root Mean Squared Error)

actual_values_EE <- testing_set$EE_riskLevel
rmse_EE <- sqrt(mean((predictions_EE - actual_values_EE) ^ 2))
print(rmse_EE)

# RMSE = 0.01223688. It means that, on average, the predictions made by the model have an error of approximately
# 0.012 units when compared to the true values of 'EE_riskLevel' in the testing set.
# Providing that RMSE is relatively small, it suggests that the model is performing quite well in predicting
# 'EE_riskLevel'.



######################## ######################## ######################## 

########################         LATVIA
######################## ######################## ########################


# Checking for multicolinearity among independent macroeconomic variables

data_LV <- data.frame(gdp = data2$LV_gdp, ur = data2$LV_ur, hicp = data2$LV_hicp,
                      euribor_6m = data2$LV_euribor_6m, wage = data2$LV_wage)
cor_matrix_LV <- cor(data_LV)
cor_matrix_LV

# Writing a full model

model_LV_1 <- lm(LV_riskLevel ~ LV_gdp + LV_ur + LV_hicp +
               LV_euribor_6m + LV_wage + LV_gdp*LV_ur + LV_gdp*LV_hicp + LV_gdp*LV_euribor_6m +
               LV_gdp*LV_wage + LV_ur*LV_hicp + LV_ur*LV_euribor_6m + LV_ur*LV_wage +
               LV_hicp*LV_euribor_6m + LV_hicp*LV_wage + LV_euribor_6m*LV_wage, data = training_set)

# Using 'step()' function in order to determine the best combination of variables to use in a model

selected_model_LV <- step(model_LV_1)
summary(selected_model_LV)


# After checking the results from 'step()' function, I copied the model formula and fitted a new model. Then,
# I began checking which of the coefficients are statistcally significant (p-value < 0.05) and have left with
# the following model:

model_LV_2 <- lm(LV_riskLevel ~ LV_gdp + LV_ur + LV_hicp + LV_euribor_6m + 
                   LV_gdp:LV_hicp + LV_ur:LV_hicp + LV_hicp:LV_euribor_6m, data = training_set)

summary(model_LV_2)

# In this case, all of the coefficients were statistically significant ('p-value' < 0.05). Thus, there was no need
# to exclude any of them after having applied 'step()' function.

# Making predictions on the testing set

predictions_LV <- predict(model_LV_2, newdata = testing_set[, c("LV_gdp", "LV_ur", "LV_hicp", "LV_euribor_6m", "LV_wage")])
predictions_LV

# Calculate the RMSE

actual_values_LV <- testing_set$LV_riskLevel
rmse_LV <- sqrt(mean((predictions_LV - actual_values_LV) ^ 2))
print(rmse_LV)

# RMSE = 0.01632332. In this case, the value is tad bit higher. Nevertheless, it still can predict the values of risk
# with an average error of 0.016 (for Estonia it was 0.012).



######################## ######################## ######################## 

########################         LITHUANIA
######################## ######################## ######################## 


# Checking for multicolinearity among independent macroeconomic variables

data_LT <- data.frame(gdp = data2$LT_gdp, ur = data2$LT_ur, hicp = data2$LT_hicp,
                      euribor_6m = data2$LT_euribor_6m, wage = data2$LT_wage)
cor_matrix_LT <- cor(data_LT)
cor_matrix_LT

# Writing a full model

model_LT_1 <- lm(LT_riskLevel ~ LT_gdp + LT_ur + LT_hicp +
                   LT_euribor_6m + LT_wage + LT_gdp*LT_ur + LT_gdp*LT_hicp + LT_gdp*LT_euribor_6m +
                   LT_gdp*LT_wage + LT_ur*LT_hicp + LT_ur*LT_euribor_6m + LT_ur*LT_wage +
                   LT_hicp*LT_euribor_6m + LT_hicp*LT_wage + LT_euribor_6m*LT_wage, data = training_set)

# Using 'step()' function in order to determine the best combination of variables to use in a model

selected_model_LT <- step(model_LT_1)
summary(selected_model_LT)


# The procedure is the same as before with 'step()' function

model_LT_2 <- lm(LT_riskLevel ~ LT_gdp + LT_ur + LT_gdp:LT_ur + LT_gdp:LT_hicp + 
                   LT_gdp:LT_euribor_6m + LT_gdp:LT_wage + LT_ur:LT_hicp + 
                   LT_ur:LT_wage + LT_euribor_6m:LT_wage, 
                 data = training_set)

summary(model_LT_2)

# The situation was the same as with Estonia, I had to manually exclude some of the coefficients as they were
# statistically insignificant ('p-value' < 0.05)

# Making predictions on the testing set

predictions_LT <- predict(model_LT_2, newdata = testing_set[, c("LT_gdp", "LT_ur", "LT_hicp", "LT_euribor_6m", "LT_wage")])
predictions_LT

# Calculate the RMSE

actual_values_LT <- testing_set$LT_riskLevel
rmse_LT <- sqrt(mean((predictions_LT - actual_values_LT) ^ 2))
print(rmse_LT)

# RMSE = 0.01448919. The value is not as good as with Estonia but better than 'Latvia's. It can predict the values of risk 
# with an average error of 0.014 (for Estonia it was 0.012, Latvia 0.016).