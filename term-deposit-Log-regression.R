library(dplyr)
library(ggplot2)
library(RColorBrewer) 
library(patchwork)
library(car)
library(pROC)
library(caret)

# 0) DATA LOADING & PREPROCESSING

df <- read.csv("bank-full.csv", header = TRUE, sep = ";")

any(is.null(df))
any(is.na(df))
any(duplicated(df))

lapply(df, FUN = class)

df <- df %>%
  mutate(across(c(job, marital, education, default, housing, loan, contact, month, poutcome, y), as.factor))

# Reordering factor levels so that "no" = 0 and "yes" = 1

levels(df$y)
levels(df$default)
levels(df$loan)
levels(df$housing)

df$y <- factor(df$y, levels = c("no", "yes"))
df$default <- factor(df$default, levels = c("no", "yes"))
df$loan <- factor(df$loan, levels = c("no", "yes"))
df$housing <- factor(df$housing, levels = c("no", "yes"))

levels(df$y)
levels(df$default)
levels(df$loan)
levels(df$housing)

####################################################
####################################################
####################################################

# 1) DATA MANIPULATION TASK

# a) select random subsample of dataset

sub_df <- sample_n(df, size = (nrow(df) * 0.3))

# b) filter desired rows using simple and more complex conditions

df1 <- filter(df, age == 41)
df2 <- df$y[df$age == 58 & df$education == "secondary"] 
df3 <- filter(df, age == 39 & job == "technician" & education == "primary")
df4 <- filter(df, (age %in% c(39, 41, 18)) & (job %in% c("technician", "entrepreneur"))
              & (marital %in% c("single", "married")) & pdays >= 150 & y == "yes")

# c) drop unnecessary variables, rename some variables

df5 <- subset(df, select = c(-contact, -day, -month, -duration))
df5 <- df5 %>% rename(edu = education, bal = balance)

# d) calculate summarizing statistics (for full sample and by categorical variables as well)

summary(df)

categ.stats <- function(data, cols){
  lapply(cols, function(col){
    prop.table(table(data[[col]])) * 100
  })
}

cols <- c("job", "marital", "education", "default",
          "housing", "loan", "contact", "month", "poutcome", "y")
prop.tables <- categ.stats(df, cols)

job <- prop.tables[[1]]
marital <- prop.tables[[2]]
edu <- prop.tables[[3]]
house <- prop.tables[[5]]
loan <- prop.tables[[6]]
y <- prop.tables[[10]]


job
marital
y

round(prop.table(table(df$marital, df$education), 1) * 100, 2)
round(prop.table(table(df$job, df$education), 1) * 100, 2)

# e) create new variables using simple transformation and custom functions

age_category <- function(age){
  if(age >= 18 & age <= 24) return("18 - 24")
  else if(age >= 25 & age <= 34) return("25 - 34")
  else if(age >= 35 & age <= 44) return("35 - 44")
  else if(age >= 45 & age <= 54) return("45 - 54")
  else if(age >= 55 & age <= 64) return("55 - 64")
  else return("65+")
}

neg_balance <- function(balance){
  if(balance < 0) return(1)
  else return(0)
}

df6 <- df

df6$age_log <- log(df$age)
df6$duration_sqrt <- sqrt(df$duration)

df6$age_category <- sapply(df$age, age_category) 
df6$negative_balance <- sapply(df$balance, neg_balance)

df6 <- df6 %>% relocate(age_category, .after = age) %>% relocate(age_log, .after = age_category) %>% 
  relocate(negative_balance, .after = balance) %>% relocate(duration_sqrt, .after = duration)

# f) order data set by several variables

df1_ordered <- arrange(df, age, balance)
df2_ordered <- arrange(df, desc(age), desc(balance))

####################################################
####################################################
####################################################

# 2) DATA VISUALISATION TASK

# Age histogram

ggplot(df6, aes(x = age)) + geom_histogram(color = "black", fill = "chocolate3") + 
  scale_x_continuous(breaks = seq(10, 100, by = 10)) +
  labs(title = "Age histogram", x = "Age", y = "Number of clients")

# Balance-education box plot

df %>% subset(education != "unknown") %>% 
  ggplot(aes(x = education, y = balance, fill =  education)) + geom_boxplot() + coord_cartesian(ylim = c(-4000, 5000)) +
  scale_y_continuous(breaks = seq(-4000, 5000, by = 1500)) + stat_summary(fun = mean, geom = "point", col = "white") +
  labs(
    title = "Clients' accounts balance in relation to the level of education", 
    x = "Level of education", 
    y = "Account's balance", 
    fill = "") +
  scale_x_discrete(labels = c("Primary", "Secondary", "Tertiary")) +
  scale_fill_discrete(labels = c("Primary", "Secondary", "Tertiary"))

# Grouped bar plot

loan_by_age <- as.data.frame(round(prop.table(table(df6$age_category, df$loan), 1) * 100, 2))

ggplot(loan_by_age, aes(x = Var1, y = Freq, fill = Var2)) +
geom_bar(stat = "identity", position = "dodge") + 
geom_text(aes(label = Freq), position = position_dodge(width = 0.9), color = "black", vjust = -0.3, size = 4) +
theme(axis.text.x = element_text(size = 10),
      axis.title.x = element_text(face = "bold"),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(face = "bold")) +
labs(
  title = "Personal Loan by Age Groups",
  x = "Age Group",
  y = "%",
  fill = "Has a personal loan") + 
scale_fill_manual(values = brewer.pal(n = 3, name = "Set1"), labels = c("No", "Yes"))

# Bar plot: 'job', 'marital', 'education', 'housing', 'loan'

levels(df$job)
levels(df$marital)
levels(df$education)

job_df <- as.data.frame(job) %>% subset(Var1 != "unknown")
marital_df <- as.data.frame(marital) 
edu_df <- as.data.frame(edu) %>% subset(Var1 != "unknown")

plot1 <- ggplot(job_df, aes(x = Var1, y = Freq, fill = Var1)) + 
         geom_bar(stat = "identity") +   
         labs(title = "Occupation", x = "Job Category", y = "%") +
         theme(axis.text.x = element_text(size = 12),
               axis.title.x = element_text(face = "bold"),
               axis.text.y = element_text(angle = 45, hjust = 1, size = 12),
               axis.title.y = element_text(face = "bold")) +
         scale_x_discrete(
          labels = c("Admin.", "Blue-collar", "Entrepreneur", "Housemaid", "Management", "Retired", "Self-employed",
                     "Services", "Student", "Technician", "Unemployed"))  + 
         scale_fill_brewer(palette = "Paired") +
         theme(legend.position = "None") + 
         coord_flip()

plot2 <- ggplot(marital_df, aes(x = Var1, y = Freq, fill = Var1)) + 
         geom_bar(stat = "identity") +  
         labs(title = "Marital Status", x = "Status", y = "%") +
         theme(axis.text.x = element_text(size = 12),
               axis.title.x = element_text(face = "bold"),
               axis.text.y = element_text(angle = 45, hjust = 1, size = 12),
               axis.title.y = element_text(face = "bold")) +
         scale_x_discrete(labels = c("Divorced", "Married", "Single")) +
         scale_fill_brewer(palette = "Paired") +
         theme(legend.position = "None") + 
         coord_flip()

plot3 <- ggplot(edu_df, aes(x = Var1, y = Freq, fill = Var1)) + 
         geom_bar(stat = "identity") +  
         labs(title = "Level of education", x = "Education", y = "%") +
         theme(axis.text.x = element_text(size = 12),
               axis.title.x = element_text(face = "bold"),
               axis.text.y = element_text(angle = 45, hjust = 1, size = 12),
               axis.title.y = element_text(face = "bold")) +
         scale_x_discrete(labels = c("Primary", "Secondary", "Tertiary")) +
         scale_fill_brewer(palette = "Set2") +
         theme(legend.position = "None") +
         coord_flip()

plot1 / (plot2 | plot3) + 
  plot_annotation(title = "Demographics of Clients", theme = theme(plot.title = element_text(hjust = 0.5, size = 25)),
                  tag_levels = "I", tag_suffix = " plot")

####################################################
####################################################
####################################################

# 3) MODELLING TASK              

# Perform a logistic regression to obtain the predicted probability that a customer has subscribed for a term deposit. 
# Use continuous variables and dummy variables created for categorical columns. Not necessarily all variables 
# provided in data sample should be used. Evaluate model goodness of fit and predictive ability. If needed, data set 
# could be split into training and test sets.

# Response variable y.

set.seed(123)

# Splitting dataset into train and test datasets

indexes <- sample(1:nrow(df), size = 0.7 * nrow(df))
train_set <- df[indexes, ]
test_set <- df[-indexes, ]


# Fitting the model

fit.null <- glm(y ~ 1, data = train_set, family = binomial)

fit.1 <- glm(y ~ ., data = train_set, family = binomial)
summary(fit.1)

# Since not all variables should be used, we begin excluding them by looking at the associated 'p-value'. If the value is
# less than 0.05, the variable is considered to be statistically significant and should not be excluded. 

# First of all, we see that 'age' has associated p-value of 0.49. Since it is more than the significance level of 0.05,
# it is excluded. 

fit.2 <- update(fit.1, . ~ . - age)
summary(fit.2)

fit.3 <- update(fit.2, . ~ . - pdays)
summary(fit.3)

fit.4 <- update(fit.3, . ~ . - previous)
summary(fit.4)

fit.5 <- update(fit.4, . ~ . - default)
summary(fit.5)

# Having excluded the variables which were statistically insignificant, we are left with the 4th model (fit.5)

# One of the requirements for logistic regression is that the independent variables must be unrelated to one another.
# Thus, it is essential to test for multicolinearity

vif <- vif(fit.5)
vif

# Providing that VIF values for all the variables are approximately equal to 1 suggests that there is no correlation
# between independent variables.

# Goodness of fit

AIC_null <- AIC(fit.null)
AIC_fit.5 <- AIC(fit.5)

AIC_null - AIC_fit.5

# By comparing the null model and the model where independent variables were included, we can see that the reduction in 
# AIC is quite big - 7788.671. This substantial decrease suggests that the predictors included in the full model 
# significantly improve its fit to the data.

fit.5_deviance <- deviance(fit.5)
fit.5_null_deviance <- fit.5$null.deviance

fit.5_deviance
fit.5_null_deviance
fit.5_null_deviance - fit.5_deviance

# The substantial decrease in deviance from the null model compared to the full model ('fit.5') (a reduction of 7804.154) 
# strongly suggests that the predictors included in 'fit.5' are effective at explaining the variability in the 
# response variable. 

# This indicates that the model successfully captures significant patterns in the data that the null model does not. 
# It is safe to say that that the reduction in deviance contributes to the statistical significance of a model.
# The included variables contribute important information for predicting the dependent variable.

# Predictive ability

predictions <- predict(fit.5, test_set, type = "response")

roc_curve <- roc(test_set$y, predictions)
auc(roc_curve)

# An AUC of 0.9033 suggests that the model does a very good job at distinguishing between the two classes.

predicted_classes <- ifelse(predictions > 0.5, "yes", "no")
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_set$y)
confusion_matrix
round(prop.table(confusion_matrix) * 100, 2)

# It is very important to note that there is strong imbalance in the values of dependent variable ('y'). 
# Approximately only 12% of 'yes' values and about 88% of 'no' values, is a significant factor contributing to the model's 
# performance, particularly its poorer performance on predicting 'yes' values accurately. 

y <- prop.tables[[10]]
y

# Having this in mind, we can see that the model has succesfully predicted 86.18% of no values while the accuracy of 
# prediction of 'yes' values is merely 4.01%. Such an imbalance has a huge effect on the predictive ability.