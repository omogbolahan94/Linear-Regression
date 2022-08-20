library(odbc)
library(DBI)
library(dplyr)
library(tidyverse) 
library(hash)
library(glmnet)
library(Metrics)
library(MetricsWeighted)


# connect to the database using odbc
con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "DESKTOP-OTIQ1GA",
                 Database = "HomePrediction",
                 Port = 1433)

# create a train and test objects for the train and test table in database
train <- tbl(con, 'train1')
test <- tbl(con, 'test')

# collect all data from the train and test table objects
train <- collect(train)
test <- collect(test)

# VIEW TRAIN DATA
View(train)

# STATISTICAL SUMMARY OF TRAIN DATA
View(summary(train))

# CHECK FOR MISSING VALUES IN THE DATASET
colSums(is.na(train)) # there is no missing values in any of the column
colSums(is.na(test)) # there is no missing values in any of the column

# check for duplicate in train and test data
sum(duplicated(train)) # no duplicate
sum(duplicated(test)) # no duplicate

###############################################################################
######################## FIT MODEL ONLY ON NUMERICAL DATA #####################
###############################################################################
numerical_train <- train %>% select_if(is.numeric)

# split the data into 20% test and 80% train
train.subset <- numerical_train[gp<0.8, ]
test.subset <- numerical_train[gp>=0.8, ]
View(test.subset)


# fit linear model
fmla1 <- as.formula(log(SalePrice) ~.)
lin.model <- lm(fmla1, data = train.subset)
summary(lin.model)

# update the predictors: remove the insignificant variables
lin.model1 = update(lin.model, ~.-Id-YrSold)
summary(lin.model1)

# predict on the test data
pred.test <- predict(lin.model1, newdata = test.subset)
pred.actual <- predict(lin.model1)

# include the predicted train and predicted test to train and test data frame
# convert log to normal form
train.subset$pred.actual <- exp(pred.actual)
test.subset$pred.test <- exp(pred.test)
View(train.subset)

# calculate the r_squared and rmse on the test data
rmse(test.subset$pred.test, test.subset$SalePrice) # 31243.37
r_squared(test.subset$pred.test, test.subset$SalePrice) # 0.82

# calculate the  r_squared and rmse on the train data
rmse(train.subset$pred.actual, train.subset$SalePrice) # 47328.59
r_squared(train.subset$pred.actual, train.subset$SalePrice) # 0.6


# plot 
ggplot(test.subset, aes(test.subset$pred.test, test.subset$SalePrice)) + 
  geom_point() + geom_abline(colot='red') 
################################################################################





