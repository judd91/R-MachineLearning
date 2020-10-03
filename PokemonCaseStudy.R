pokedata <- read.csv("archive/Pokemon.csv")
View(pokedata)

library(dplyr)
library(caTools)

## Prepare Data
# Remove first Col
pokedata <- pokedata %>% select(-1)
# Change name of columns
colnames(pokedata)[2] <- "Primary_Type"
colnames(pokedata)[3] <- "Secundary_Type"


str(pokedata) #dataset info

# Change type to factor
pokedata$Name <- as.factor(pokedata$Name)
pokedata$Primary_Type <- as.factor(pokedata$Primary_Type)
pokedata$Secundary_Type <- as.factor(pokedata$Secundary_Type)
pokedata$Legendary <- as.factor(pokedata$Legendary)

table(pokedata$Primary_Type)

# Select Grass Pokemon
grass_pokemon <- pokedata %>% filter(Primary_Type=="Grass")
grass_poison_pokemon <- grass_pokemon %>% filter(Secundary_Type=="Poison")
range(grass_poison_pokemon$Speed) #Get the pokemon with higher speed (90)
grass_pokemon_selection <- grass_poison_pokemon %>% filter(Speed==90)
View(grass_pokemon_selection)

# Select Water Pokemon
water_pokemon <- pokedata %>% filter(Primary_Type=="Water")
water_psychic_pokemon <- water_pokemon %>% filter(Secundary_Type=="Psychic")
range(water_psychic_pokemon$Defense) #Get the pokemon with higher defense (180)
water_pokemon_selection <- water_psychic_pokemon %>% filter(Defense==180)
View(water_pokemon_selection)

# Select Fire Pokemon
fire_pokemon <- pokedata %>% filter(Primary_Type=="Fire")
fire_fighting_pokemon <- fire_pokemon %>% filter(Secundary_Type=="Fighting")
range(fire_fighting_pokemon$Defense)
fire_pokemon_selection <- fire_fighting_pokemon %>% filter(Defense==80)
View(fire_pokemon_selection)


my_pokemons <- rbind(fire_pokemon_selection,grass_pokemon_selection, water_pokemon_selection)
View(my_pokemons)

## Build Regression Models
# Factors influences pokemon's atacks
split_dataset <- sample.split(pokedata$Attack, SplitRatio = 0.65)
train_set1 <- subset(pokedata, split_dataset==T)
test_set1 <- subset(pokedata, split_dataset==F)

# Model1: Attack - Defense
regression_model1 <- lm(Attack~Defense, data=train_set1)
regression_result1 <- predict(regression_model1, test_set1)
data_result1 <- cbind(Actual=test_set1$Attack, Predicted=regression_result1)
data_result1 <- as.data.frame(data_result1)
View(data_result1)

# Finding Error
error <- (data_result1$Actual - data_result1$Predicted)
data_result1 <- cbind(data_result1, error)
rmse1 <- sqrt(mean(data_result1$error^2))
rmse1

# Model2: Attack - Defense & Speed & HP
regression_model2 <- lm(Attack~Defense+Speed+HP, data=train_set1)
regression_result2 <- predict(regression_model2, test_set1)
data_result2 <- cbind(Actual=test_set1$Attack, Predicted=regression_result2)
data_result2 <- as.data.frame(data_result2)
View(data_result2)

# Finding Error
error2 <- (data_result2$Actual - data_result2$Predicted)
data_result2 <- cbind(data_result2, error2)
View(data_result2)
rmse2 <- sqrt(mean(data_result2$error2^2))
rmse2

# Second Model gets better results than First Model

## Classify if a pokemon is Legendary or not
split_dataset2 <- sample.split(pokedata$Legendary, SplitRatio = 0.65)
train_set2 <- subset(pokedata, split_dataset2==T)
test_set2 <- subset(pokedata, split_dataset2==F)
nrow(train_set2)

# Decision Tree
library(rpart)

# Model1: using all cols
class_model1 <- rpart(Legendary~., data = train_set2) 
result_class1 <- predict(class_model1, test_set2, type = "class") 
#Confusion Matrix
table(test_set2$Legendary, result_class1)

# Evaluation
library(caret)
confusionMatrix(table(test_set2$Legendary, result_class1))

# Model2: using Defense, Attack and Speed variables
class_model2 <- rpart(Legendary~Attack+Speed+Defense, data = train_set2) 
result_class2 <- predict(class_model2, test_set2, type = "class") 
#Confusion Matrix
table(test_set2$Legendary, result_class2)

# Evaluation
library(caret)
confusionMatrix(table(test_set2$Legendary, result_class2))

# Second Model gets better results than First Model
