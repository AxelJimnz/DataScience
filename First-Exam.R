# Cargar librerías necesarias
install.packages("dplyr")
library(ggplot2)
library(dplyr)
library(caret)
library(psych)

# Leer los datos
data <- read.csv("Salary_Data.csv")
# Podemos comprobar el tipo de datos de una variable usando la función str
str(data)