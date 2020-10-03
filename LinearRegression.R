###LINEAR REGRESSION
# Determine how one variable influences another variables
library(datasets)
library(ggplot2)
View(diamonds)
#Splitting data in train and test
library(caTools)
split_values <- sample.split(diamonds$price, SplitRatio = 0.65) 
train_set <- subset(diamonds, split_values==T) 
test_set <- subset(diamonds, split_values==F)

#Building Linear Regression Model
lregression_model <- lm(price~., data = train_set)  #Dependent variable = price, Intependant variables = rest of cols
result_regression <- predict(lregression_model, test_set)
final_result <- cbind(Actual=test_set$price, Predicted=result_regression) #Create matrix with the actual rpice values and the predicted values
final_result <- as.data.frame(final_result)
View(final_result)

#Error in  prediction
error <- (final_result$Actual- final_result$Predicted)
final_result <- cbind(final_result, error) # Add error col 
View(final_result)

rmse <- sqrt(mean(final_result$error^2))
rmse
# 1006.233 high error