#SALARY EDA and PREDICTION
library(ggplot2)     # para visualización de datos
library(dplyr)       # para manipulación de datos
library(caret)       # para métricas de evaluación
library(keras)       # para construir modelos Keras

# Leer el archivo CSV
df <- read_csv("D:/ICO - AXEL JIMENEZ PEREZ/8.OCTAVO SEMESTRE/CIENCIA DE DATOS/FIRST EXAM/Salary_Data.csv")
View(Salary_Data)

# Renombrar las columnas
colnames(df) <- c('age', 'gender', 'educationLevel', 'jobTitle', 'yearsOfExp', 'salary')

# Eliminar filas con valores NA
df <- na.omit(df)

# Mostrar las primeras filas del dataframe
head(df)
