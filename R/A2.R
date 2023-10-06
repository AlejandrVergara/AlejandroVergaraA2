library(tidyverse)
library(caret)
library(class)
library(gmodels)
folder<-dirname(rstudioapi::getSourceEditorContext()$path)

parentFolder <-dirname(folder)

data <-
  read.csv(paste0(parentFolder,"/Dataset/diabetes_012.csv"))

data$Diabetes_012 <- ifelse(data$Diabetes_012 == 0, 0, 1)

## selección de 100 muestras de cada factor del dataset ##
data_estratificada <- data %>%
  group_by(Diabetes_012) %>%
  sample_n(1500, replace = TRUE) %>%
  ungroup()

summary(data_estratificada)

library(psych)
pairs.panels(data_estratificada[c("Age", "BMI", "HighBP", "Education")],
             pch = 21,
             bg = c("red", "green3", "blue", "orange" , "yellow")[unclass(data_estratificada$GenHlth)])


sample.index <- sample(1:nrow(data_estratificada)
                       ,nrow(data_estratificada)*0.7
                       ,replace = F)

### Modelos para hallar la Diabetes

predictors <- c("HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income")

# Original data
train.data <- data_estratificada[sample.index, c(predictors, "Diabetes_012"), drop = FALSE]
test.data <- data_estratificada[-sample.index, c(predictors, "Diabetes_012"), drop = FALSE]



train.data$Diabetes_012 <- factor(train.data$Diabetes_012)
test.data$Diabetes_012 <- factor(test.data$Diabetes_012)

# Entrena el modelo de k-NN
ctrl <- trainControl(method = "cv", p = 0.7)
knnFit <- train(Diabetes_012 ~ .
                , data = train.data
                , method = "knn", trControl = ctrl
                , preProcess = c("range") # c("center", "scale") for z-score
                , tuneLength = 20)

knnFit

plot(knnFit)

# Realiza las predicciones
knnPredict <- predict(knnFit, newdata = test.data)

# Crea la matriz de confusión
confusionMatrix(data = knnPredict, reference = test.data$Diabetes_012)
library(gmodels)

CrossTable(x = test.data$Diabetes_012,  y = knnPredict,
           prop.chisq = F)


### segundo Modelo

predictors_to_remove <- c("AnyHealthcare", "NoDocbcCost", "DiffWalk", "Education", "Income")
train.data2 <- train.data[, !(names(train.data) %in% predictors_to_remove)]
test.data2 <- test.data[, !(names(test.data) %in% predictors_to_remove)]


ctrl <- trainControl(method = "cv", number = 5)
knnFit2 <- train(Diabetes_012 ~ .
                 , data = train.data2
                 , method = "knn", trControl = ctrl
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)

knnFit2

plot(knnFit2)

# Realiza las predicciones
knnPredict2 <- predict(knnFit2, newdata = test.data2)

# Crea la matriz de confusión
confusionMatrix(data = knnPredict, reference = test.data2$Diabetes_012)
library(gmodels)

CrossTable(x = test.data2$Diabetes_012, y = knnPredict2
           , prop.chisq = F)

### Tercer Modelo

predictors_to_remove2 <- c("ChoclCheck", "MentHlth","PhysHlth", "Fruits", "Veggies")
train.data3 <- train.data2[, !(names(train.data2) %in% predictors_to_remove2)]
test.data3 <- test.data2[, !(names(test.data2) %in% predictors_to_remove2)]


ctrl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knnFit3 <- train(Diabetes_012 ~ .
                 , data = train.data3
                 , method = "knn", trControl = ctrl2
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)

knnFit3

plot(knnFit3)

# Realiza las predicciones
knnPredict3 <- predict(knnFit3, newdata = test.data3)

# Crea la matriz de confusión
confusionMatrix(data = knnPredict3, reference = test.data3$Diabetes_012)
library(gmodels)

CrossTable(x = test.data3$Diabetes_012, y = knnPredict3
           , prop.chisq = F)



### HeartDiseaseorAttack

## selección de 100 muestras de cada factor del dataset ##
data_estratificada <- data %>%
  group_by(HeartDiseaseorAttack) %>%
  sample_n(1500, replace = TRUE) %>%
  ungroup()

predictors <- c("HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "Diabetes_012", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income")

# Original data
train.data <- data_estratificada[sample.index, c(predictors, "HeartDiseaseorAttack"), drop = FALSE]
test.data <- data_estratificada[-sample.index, c(predictors, "HeartDiseaseorAttack"), drop = FALSE]

train.data$HeartDiseaseorAttack <- factor(train.data$HeartDiseaseorAttack)
test.data$HeartDiseaseorAttack <- factor(test.data$HeartDiseaseorAttack)

# Entrena el modelo de k-NN
ctrl <- trainControl(method = "cv", p = 0.7)
knnFit <- train(HeartDiseaseorAttack ~ .
                , data = train.data
                , method = "knn", trControl = ctrl
                , preProcess = c("range") # c("center", "scale") for z-score
                , tuneLength = 20)

knnFit

plot(knnFit)

# Realiza las predicciones
knnPredict <- predict(knnFit, newdata = test.data)

# Crea la matriz de confusión
confusionMatrix(data = knnPredict, reference = test.data$HeartDiseaseorAttack)


CrossTable(x = test.data$HeartDiseaseorAttack,  y = knnPredict,
           prop.chisq = F)
### segundo modelo

predictors_to_remove <- c("AnyHealthcare", "NoDocbcCost", "DiffWalk", "Education", "Income")
train.data2 <- train.data[, !(names(train.data) %in% predictors_to_remove)]
test.data2 <- test.data[, !(names(test.data) %in% predictors_to_remove)]


ctrl <- trainControl(method = "cv", number = 5)
knnFit2 <- train(HeartDiseaseorAttack ~ .
                 , data = train.data2
                 , method = "knn", trControl = ctrl
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)

knnFit2

plot(knnFit2)

# Realiza las predicciones
knnPredict2 <- predict(knnFit2, newdata = test.data2)

# Crea la matriz de confusión
confusionMatrix(data = knnPredict, reference = test.data2$HeartDiseaseorAttack)
library(gmodels)

CrossTable(x = test.data2$HeartDiseaseorAttack, y = knnPredict2
           , prop.chisq = F)

### Tercer Modelo

predictors_to_remove2 <- c("ChoclCheck", "MentHlth","HvyAlcoholConsump", "Fruits", "Veggies")
train.data3 <- train.data2[, !(names(train.data2) %in% predictors_to_remove2)]
test.data3 <- test.data2[, !(names(test.data2) %in% predictors_to_remove2)]


ctrl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knnFit3 <- train(HeartDiseaseorAttack ~ .
                 , data = train.data3
                 , method = "knn", trControl = ctrl2
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)

knnFit3

plot(knnFit3)

# Realiza las predicciones
knnPredict3 <- predict(knnFit3, newdata = test.data3)

# Crea la matriz de confusión
confusionMatrix(data = knnPredict, reference = test.data3$HeartDiseaseorAttack)
library(gmodels)

CrossTable(x = test.data3$HeartDiseaseorAttack, y = knnPredict3
           , prop.chisq = F)
####


## selección de 100 muestras de cada factor del dataset ##
data_estratificada <- data %>%
  group_by(Sex) %>%
  sample_n(1500, replace = TRUE) %>%
  ungroup()

predictors <- c("HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack" ,"Diabetes_012", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Age", "Education", "Income")

# Original data
train.data <- data_estratificada[sample.index, c(predictors, "Sex"), drop = FALSE]
test.data <- data_estratificada[-sample.index, c(predictors, "Sex"), drop = FALSE]

train.data$Sex <- factor(train.data$Sex)
test.data$Sex <- factor(test.data$Sex)

# Entrena el modelo de k-NN
ctrl <- trainControl(method = "cv", p = 0.7)
knnFit <- train(Sex ~ .
                , data = train.data
                , method = "knn", trControl = ctrl
                , preProcess = c("range") # c("center", "scale") for z-score
                , tuneLength = 20)

knnFit

plot(knnFit)

# Realiza las predicciones
knnPredict <- predict(knnFit, newdata = test.data)

# Crea la matriz de confusión
confusionMatrix(data = knnPredict, reference = test.data$Sex)


CrossTable(x = test.data$Sex,  y = knnPredict,
           prop.chisq = F)
### segundo modelo

predictors_to_remove <- c("AnyHealthcare", "NoDocbcCost", "DiffWalk", "Age", "PhysActivity")
train.data2 <- train.data[, !(names(train.data) %in% predictors_to_remove)]
test.data2 <- test.data[, !(names(test.data) %in% predictors_to_remove)]


ctrl <- trainControl(method = "cv", number = 5)
knnFit2 <- train(Sex ~ .
                 , data = train.data2
                 , method = "knn", trControl = ctrl
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)

knnFit2

plot(knnFit2)

# Realiza las predicciones
knnPredict2 <- predict(knnFit2, newdata = test.data2)

# Crea la matriz de confusión
  confusionMatrix(data = knnPredict2, reference = test.data2$Sex)


CrossTable(x = test.data2$HeartDiseaseorAttack, y = knnPredict2
           , prop.chisq = F)

### Tercer Modelo

predictors_to_remove2 <- c("ChoclCheck", "MentHlth","HvyAlcoholConsump", "Fruits", "Veggies")
train.data3 <- train.data2[, !(names(train.data2) %in% predictors_to_remove2)]
test.data3 <- test.data2[, !(names(test.data2) %in% predictors_to_remove2)]


ctrl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knnFit3 <- train(Sex ~ .
                 , data = train.data3
                 , method = "knn", trControl = ctrl2
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)

knnFit3

plot(knnFit3)

# Realiza las predicciones
knnPredict3 <- predict(knnFit3, newdata = test.data3)

# Crea la matriz de confusión
confusionMatrix(data = knnPredict3, reference = test.data3$Sex)


CrossTable(x = test.data3$Sex, y = knnPredict3
           , prop.chisq = F)
