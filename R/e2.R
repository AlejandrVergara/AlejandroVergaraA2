library(tidyverse)
folder<-dirname(rstudioapi::getSourceEditorContext()$path)

parentFolder <-dirname(folder)

data <-
  read.csv(paste0(parentFolder,"/Dataset/diabetes_012.csv"))

summary(data)
# Visualizaciones
ggplot(data, aes(x = Age, y = BMI)) + geom_boxplot()

# Binarización de la variable de clase
data$Diabetes_012 <- ifelse(data$Diabetes_012 == 0, 0, 1)
