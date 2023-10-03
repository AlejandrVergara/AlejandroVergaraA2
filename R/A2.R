library(tidyverse)
library(caret)
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

k <- 3
predictors <- c("HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income")

# Original data
train.data <- data_estratificada[sample.index, c(predictors, "Diabetes_012"), drop = FALSE]
test.data <- data_estratificada[-sample.index, c(predictors, "Diabetes_012"), drop = FALSE]

library(class)

# Entrenamiento y predicción
prediction <- knn(
  train = train.data[predictors],
  test = test.data[predictors],
  cl = train.data$Diabetes_012,
  k = k)


ctrl <- trainControl(method = "cv", p = 0.7)

# Entrenar el modelo KNN con normalización min-max
knnFit <- train(
  formula = as.formula(paste(class_variable, "~", paste(predictors, collapse = "+"))),
  data = train.data,
  method = "knn",
  trControl = ctrl,
  preProcess = c("range"),  # Normalización min-max
  tuneLength = 20
)

# Imprimir el resultado del modelo KNN
knnFit

# Realizar un gráfico para visualizar el rendimiento
plot(knnFit)

# Obtener predicciones para los datos de prueba
knnPredict <- predict(knnFit, newdata = test.data)

# Obtener la matriz de confusión para evaluar la precisión y otros parámetros
confusionMatrix(knnPredict, test.data[, class_variable])
