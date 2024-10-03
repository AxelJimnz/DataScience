# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(caret)
library(psych)

# Leer los datos
data <- read.csv("Salary_Data.csv")
str(data)

# Eliminar filas con salario faltante
data <- data[!is.na(data$Salary),]

# Convertir variables categóricas a factores
data$Education.Level <- as.factor(data$Education.Level)
data$Job.Title <- as.factor(data$Job.Title)

# -------------------------------
# Exploratory Data Analysis (EDA)
# -------------------------------
# Distribución de la variable Salary
ggplot(data, aes(x = Salary)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "white", alpha = 0.7) +
  theme_minimal() +
  ggtitle("Distribución de los Salarios") +
  labs(x = "Salario", y = "Frecuencia")

# Distribución del Salario por Nivel Educativo
ggplot(data, aes(x = Education.Level, y = Salary, fill = Education.Level)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Distribución de Salario por Nivel Educativo") +
  labs(x = "Nivel Educativo", y = "Salario") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Relación entre Años de Experiencia y Salario
ggplot(data, aes(x = as.factor(Years.of.Experience), y = Salary)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.colour = "red") +
  theme_minimal() +
  ggtitle("Relación entre Años de Experiencia y Salario (Boxplot)") +
  labs(x = "Años de Experiencia", y = "Salario") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------------
# Preparar los datos para PCA y modelo predictivo
# -------------------------------







