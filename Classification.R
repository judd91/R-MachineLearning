### CLASSIFICATION
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
               ggvis, httr, libridate, plotly, rio, rmarkdown, shiny, string, tidyr)

library(datasets)
head(iris)


#Remove columns
library(dplyr)
#Splitting data in train and test
library(caTools)
split_values <- sample.split(iris$Species, SplitRatio = 0.65) 
train_set <- subset(iris, split_values==T) 
test_set <- subset(iris, split_values==F)

#Build Classification Model
#Determine if the specie is related to the length and width of Petals and Sepal
library(rpart)
class_model <- rpart(Species~., data = train_set) 
result_class <- predict(class_model, test_set, type = "class") 
#Confusion Matrix
table(test_set$Species, result_class)

#Result Matrix
#               setosa versicolor virginica
# setosa         18          0         0
# versicolor      0         18         0
# virginica       0          3        15

#Accuracy
(18+18+15)/(18+18+15+3)
#0.9444444