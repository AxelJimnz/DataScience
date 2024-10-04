# Cargar librerías necesarias
install.packages("GGally")
install.packages("caret")
install.packages("purrr")
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(GGally)
library(caret)
library(purrr)
library(forcats)

# -------------------------------
# Definición del Problema
# -------------------------------
# Objetivo:  predecir el salario del empleado en función de las características disponibles en el conjunto de datos.

# -------------------------------
# Entendimiento de los Datos
# -------------------------------
# Cargar los datos
df <- read.csv("D:/ICO - AXEL JIMENEZ PEREZ/8.OCTAVO SEMESTRE/CIENCIA DE DATOS/FIRST EXAM/Salary_Data.csv")
head(df)

# Verificar número de filas y columnas
dim(df)

# Obtener los nombres de las columnas
colnames(df)

# Renombrar las columnas del DataFrame
colnames(df) <- c('age', 'gender', 'education_level', 'job_title', 'experience_years', 'salary')

# Obtener la información del DataFrame
str(df)

# -------------------------------
# Limpieza de los Datos
# -------------------------------
# Verificar los valores faltantes
colSums(is.na(df))

# Eliminar filas con todos los valores faltantes
df <- df[!apply(is.na(df), 1, all), ]

# Llenar los valores faltantes en "education_level" con el modo
df$education_level[is.na(df$education_level)] <- names(sort(table(df$education_level), decreasing = TRUE))[1]

# Llenar los valores faltantes en "experience_years" con la media
df$experience_years[is.na(df$experience_years)] <- mean(df$experience_years, na.rm = TRUE)

# Llenar los valores faltantes en "salary" con la mediana
df$salary[is.na(df$salary)] <- median(df$salary, na.rm = TRUE)

# Verificar nuevamente los valores faltantes en R
colSums(is.na(df))

# Verificar datos duplicados
duplicates <- df[duplicated(df), ]

# Eliminar duplicados del DataFrame
emp_data <- df[!duplicated(df), ]

# Obtener la forma del nuevo DataFrame
dim(emp_data)

# -------------------------------
# Exploracion y Visualizacion de los datos
# -------------------------------
# Estadísticas descriptivas de los datos numéricos
summary(emp_data)

# Histograma para 'age'
ggplot(emp_data, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histograma de Edad", x = "Edad", y = "Frecuencia")

# Histograma para 'experience_years'
ggplot(emp_data, aes(x = experience_years)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Histograma de Años de Experiencia", x = "Años de Experiencia", y = "Frecuencia")

# Histograma para 'salary'
ggplot(emp_data, aes(x = salary)) +
  geom_histogram(binwidth = 5000, fill = "purple", color = "black") +
  labs(title = "Histograma de Salario", x = "Salario", y = "Frecuencia")

# Correlación de variables numéricas
correlation_matrix <- cor(emp_data[, sapply(emp_data, is.numeric)])
print(correlation_matrix)

# Visualización de la correlación con un mapa de calor
correlation_melted <- melt(correlation_matrix)

# Crear el mapa de calor
ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlación") +
  theme_minimal() +
  geom_text(aes(label = round(value, 2)), color = "black") +
  labs(title = "Mapa de Calor de la Matriz de Correlación", x = "", y = "")

# Gráfico de dispersión para 'experience_years' por 'age' en R
ggplot(df, aes(x = age, y = experience_years)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión: Años de Experiencia vs Edad", 
       x = "Edad", 
       y = "Años de Experiencia") +
  theme_minimal()

# Gráfico de dispersión para 'salary' por 'experience_years' en R
ggplot(emp_data, aes(x = experience_years, y = salary)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión: Salario vs Años de Experiencia", 
       x = "Años de Experiencia", 
       y = "Salario") +
  theme_minimal()

# Par de gráficos
ggpairs(emp_data)

# Contar los valores en la columna 'education_level'
table(emp_data$education_level)

# Reemplazar atributos similares en la columna 'education_level' en R
emp_data$education_level <- recode(emp_data$education_level,
                                   `phD` = "PhD", 
                                   `Bachelor's` = "Bachelor's Degree", 
                                   `Master's` = "Master's Degree")

# Contar los valores en la columna 'education_level'
table(emp_data$education_level)

# Visualizar la cuenta de valores en la columna 'education_level'
ggplot(data = as.data.frame(table(emp_data$education_level)), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Frecuencia de Niveles Educativos", x = "Nivel Educativo", y = "Frecuencia") +
  theme_minimal()

# Contar los valores en la columna 'gender'
table(emp_data$gender)

# Visualizar la cuenta de valores en la columna 'gender'
ggplot(data = as.data.frame(table(emp_data$gender)), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Frecuencia de Género", x = "Género", y = "Frecuencia") +
  theme_minimal()

# Contar los valores en la columna 'job_title' en R
table(emp_data$job_title)

# -------------------------------
# Procesamiento de los Datos
# -------------------------------
# Variables dependientes e independientes en R
x <- emp_data[, !names(emp_data) %in% c("salary")]
y <- emp_data$salary

set.seed(42)  # Fijar la semilla para reproducibilidad
train_index <- createDataPartition(y, p = 0.8, list = FALSE)  # Crear índices para el conjunto de entrenamiento
x_train <- x[train_index, ]
y_train <- y[train_index]
x_test <- x[-train_index, ]
y_test <- y[-train_index]







