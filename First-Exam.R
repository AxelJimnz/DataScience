# Cargar librerías necesarias
# install.packages("GGally")
# install.packages("caret")
# install.packages("purrr")
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

# Llenar los valores faltantes en "age" con la mediana
df$age[is.na(df$age)] <- median(df$age, na.rm = TRUE)

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
  scale_fill_gradient2(low = "violet", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlación") +
  theme_minimal() +
  geom_text(aes(label = round(value, 2)), color = "black") +
  labs(title = "Mapa de Calor de la Matriz de Correlación", x = "", y = "")

# Gráfico de dispersión para 'experience_years' por 'age'
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

# ------------------------
# 5. Preprocesamiento de Datos
# ------------------------
# Definir la variable dependiente y las variables independientes
x <- emp_data %>% select(-salary)  # Excluir la variable dependiente
y <- emp_data$salary

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(42)  #
train_index <- sample(seq_len(nrow(emp_data)), size = 0.8 * nrow(emp_data))  # Índices para el conjunto de entrenamiento
x_train <- x[train_index, ]  # Datos de entrenamiento
y_train <- y[train_index]  # Variable dependiente del conjunto de entrenamiento
x_test <- x[-train_index, ]  # Datos de prueba
y_test <- y[-train_index]  # Variable dependiente del conjunto de prueba

# Preprocesadores para datos numéricos
num_preprocessor <- preProcess(x_train %>% select(age, experience_years), method = c("center", "scale"))

# Preprocesador para datos categóricos
cat_preprocessor <- dummyVars(~ gender + education_level + job_title, data = x_train)

# Preprocesar datos
preprocess_data <- function(data) {
  num_transformed <- predict(num_preprocessor, data)  # Transformación numérica
  cat_transformed <- predict(cat_preprocessor, data)  # Transformación categórica
  
  # Combinar los resultados
  transformed_data <- cbind(num_transformed, cat_transformed)
  
  # Asegurarse de que los nombres de las columnas sean únicos
  colnames(transformed_data) <- make.unique(colnames(transformed_data))
  
  return(transformed_data)
}


# Crear la pipeline para regresión lineal
linear_regression_pipeline <- function(train_data, y_train) {
  train_transformed <- preprocess_data(data.frame(train_data, y_train))  # Preprocesar solo las variables independientes
  model <- lm(y_train ~ ., data = as.data.frame(train_transformed))  # Ajustar el modelo
  return(model)
}

# Validación cruzada para regresión lineal
set.seed(42)  # Para reproducibilidad
control <- trainControl(method = "cv", number = 5)

# Crear datos preprocesados para el entrenamiento
train_transformed <- preprocess_data(data.frame(x_train, y_train))

# Ajustar el modelo de regresión lineal usando validación cruzada
linear_model <- train(y_train ~ ., 
                      data = as.data.frame(train_transformed), 
                      method = "lm", 
                      trControl = control)

# Obtener los puntajes de RMSE
linear_scores <- linear_model$results$RMSE
cat("Linear Regression cross-validation scores are:", linear_scores, "\n")

# Ajustar el modelo de regresión lineal a los datos de entrenamiento
selected_model <- linear_regression_pipeline(x_train, y_train)# Definir la variable dependiente y las variables independientes
x <- emp_data %>% select(-salary)  # Excluir la variable dependiente
y <- emp_data$salary


# ----------------------
# 6. Evaluación del Modelo
# ----------------------
# Predecir salarios en los datos de entrenamiento y calcular precisión
y_train_predict <- predict(selected_model, newdata = preprocess_data(data.frame(x_train)))  # Predicciones en datos de entrenamiento
mse_train <- mean((y_train - y_train_predict) ^ 2)  # Calcular MSE
r2_train <- summary(selected_model)$r.squared  # Calcular R²
rmse_train <- sqrt(mse_train)

cat(sprintf("Root Mean Square of training data: %.2f\n", rmse_train))
cat(sprintf("R square of training data: %.4f\n", r2_train))

# Predecir salarios en los datos de prueba y calcular precisión
y_test_predict <- predict(selected_model, newdata = preprocess_data(data.frame(x_test)))  # Predicciones en datos de prueba
mse_test <- mean((y_test - y_test_predict) ^ 2)  # Calcular MSE
r2_test <- 1 - (sum((y_test - y_test_predict)^2) / sum((y_test - mean(y_test))^2))  # Calcular R² para datos de prueba
rmse_test <- sqrt(mse_test)

cat(sprintf("Root Mean Square of testing data: %.2f\n", rmse_test))
cat(sprintf("R square of testing data: %.4f\n", r2_test))