# Import of libraries
library(ggcorrplot)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caTools)
library(ROSE)
library(e1071)
library(caret)
library(plotROC)
library(psych)
library(rpart)
library(ISLR)

# Reading the csv file
online_payment_df <- read.csv("online_payment.csv", na.strings = "Nan")

# Count the total number of records
print(nrow(online_payment_df))

# IsFraud is needs to be converted to factor type
online_payment_df$isFraud <- as.factor(online_payment_df$isFraud)

# Printing the data type of each col
print(paste("Data Type of Step column: ", class(online_payment_df$step)))
print(paste("Data Type of Type column: ", class(online_payment_df$type)))
print(paste("Data Type of Amount column: ", class(online_payment_df$amount)))
print(paste("Data Type of Name Origin column: ", class(online_payment_df$nameOrig)))
print(paste("Data Type of Old Balance Origin column: ", class(online_payment_df$oldbalanceOrg)))
print(paste("Data Type of New Balance Origin column: ", class(online_payment_df$newbalanceOrig)))
print(paste("Data Type of Name Destination column: ", class(online_payment_df$nameDest)))
print(paste("Data Type of Old Balance Destination column: ", class(online_payment_df$oldbalanceDest)))
print(paste("Data Type of New Balance Destination column: ", class(online_payment_df$newbalanceDest)))
print(paste("Data Type of Is Fraud column: ", class(online_payment_df$isFraud)))
print(paste("Data Type of Is Flagged Fraud column: ", class(online_payment_df$isFlaggedFraud))) 

# Data Slicing

# Grouping base on price status
grp_by_fraud <- online_payment_df %>% group_by(isFraud) %>% summarise(n = n()) %>% as.data.frame()

# Plotting the price status distribution
ggplot(data = grp_by_fraud, aes(x=isFraud, y = n)) + geom_col(fill = "grey") + 
  ggtitle("Fraud distribution") + labs(x="Is Fraud", y="Count") +  
  geom_text(aes(label=n), position=position_stack(vjust = 0.4), vjust=-0.25, size=2.5) + 
  theme(plot.title = element_text(size = 10, face = "bold"))

# Splitting the data
set.seed(1234)
index_0.6 <- runif(nrow(online_payment_df)) < 0.6
set.seed(1234)
index_0.7 <- runif(nrow(online_payment_df)) < 0.7
set.seed(1234)
index_0.8 <- runif(nrow(online_payment_df)) < 0.8

training_0.6 <- online_payment_df[index_0.6 , ] 
testing_0.6 <- online_payment_df[!index_0.6 , ]

training_0.7 <- online_payment_df[index_0.7 , ] 
testing_0.7 <- online_payment_df[!index_0.7 , ]

training_0.8 <- online_payment_df[index_0.8 , ] 
testing_0.8 <- online_payment_df[!index_0.8 , ]

# 60: 40 distribution
print("Ratio dimension: 60:40")
dim(training_0.6)
dim(testing_0.6)

# 70: 30 distribution
print("Ratio dimension: 70:30")
dim(training_0.7)
dim(testing_0.7)

# 80: 20 distribution
print("Ratio dimension: 80:20")
dim(training_0.8)
dim(testing_0.8)

# Data resampling
print("Resample distribution for 80:20 slicing ratio: ")
training_0.8 <- ovun.sample(isFraud ~ ., data = training_0.8, method = "both", p=0.5,  N=5089710, seed = 1)$data
print("Training data distribution after resampling: ")
table(training_0.8$isFraud)
print("   ")
testing_0.8 <- ovun.sample(isFraud ~ ., data = testing_0.8, method = "both", p=0.5,  N=1272910, seed = 1)$data
print("Testing data distribution after resampling: ")
table(testing_0.8$isFraud)

# Grouping base on price status
grp_by_is_fraud_training <- training_0.8 %>% group_by(isFraud) %>% summarise(n = n()) %>% as.data.frame()

ggplot(data = grp_by_is_fraud_training, aes(x=isFraud, y = n)) + geom_col(fill = "grey") + 
  ggtitle("[80:20 Training Data] Is Fraud distribution") + labs(x="Is Fraud", y="Count") +  
  geom_text(aes(label=n), position=position_stack(vjust = 0.4), vjust=-0.25, size=2.5) + 
  theme(plot.title = element_text(size = 10, face = "bold"))


# Grouping base on price status
grp_by_is_fraud_testing <- testing_0.8 %>% group_by(isFraud) %>% summarise(n = n()) %>% as.data.frame()

ggplot(data = grp_by_is_fraud_testing, aes(x=isFraud, y = n)) + geom_col(fill = "grey") + 
  ggtitle("[80:20 Testing Data] Is Fraud distribution") + labs(x="Is Fraud", y="Count") +  
  geom_text(aes(label=n), position=position_stack(vjust = 0.4), vjust=-0.25, size=2.5) + 
  theme(plot.title = element_text(size = 10, face = "bold"))




















