# Cargar bibliotecas
install.packages(geom)
library(tidyverse)
library(plotly)
library(caret)
library(geom_boxplot)

# Leer el archivo CSV
df <- read.csv("D:/ICO - AXEL JIMENEZ PEREZ/8.OCTAVO SEMESTRE/CIENCIA DE DATOS/FIRST EXAM/Salary_Data.csv")

# Ver las primeras filas
head(df)

# Información sobre el dataframe
str(df)

# Contar valores faltantes
sum(is.na(df))
df[is.na(df$Salary), ]

# Eliminar filas con valores faltantes
df <- na.omit(df)

# Comprobar duplicados
sum(duplicated(df))
df[duplicated(df), ]
df <- df[!duplicated(df), ]

# Restablecer índices
row.names(df) <- NULL

colnames(df) <- c('age', 'gender', 'education_level', 'job_title', 'experience_years', 'salary')

# Visualizar conteos de género y nivel educativo
fig <- df %>%
  ggplot(aes(x = gender)) +
  geom_bar() +
  labs(title = "Conteo de Género")
ggplotly(fig)

fig <- df %>%
  ggplot(aes(x = `education_level`)) +
  geom_bar() +
  labs(title = "Conteo de Nivel Educativo") +
  theme(axis.text.x = element_text(angle = 90))
ggplotly(fig)

# Visualizar años de experiencia
fig <- df %>%
  ggplot(aes(x = `experience_years`)) +
  geom_bar() +
  labs(title = "Conteo de Años de Experiencia") +
  theme(axis.text.x = element_text(angle = 90))
ggplotly(fig)

# Visualizar estadísticas descriptivas
summary(df)

# Visualizar relación entre Años de Experiencia y Salario
fig <- df %>%
  ggplot(aes(x = `experience_years`, y = salary)) +
  geom_bar(stat = "identity") +
  labs(title = "Salario por Años de Experiencia")
ggplotly(fig)

# Visualizar 10 trabajos con más salario
top_jobs <- df %>%
  count(`job_title`) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(`job_title`, n), y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Trabajos") +
  theme(axis.text.x = element_text(angle = 90))
ggplotly(top_jobs)

# Análisis de correlación
correlation <- cor(df %>% select_if(is.numeric))
fig <- plot_ly(z = correlation, colors = colorRamp(c("blue", "white", "red")), type = "heatmap")
fig <- fig %>% layout(title = "Correlaciones entre los datos")
ggplotly(fig)

correlation <- cor(df %>% select_if(is.numeric))
print(correlation)

# Visualizar distribución de edades
fig <- df %>%
  ggplot(aes(y = `age`)) +
  geom_boxplot() +
  labs(title = "Distribución de Edades")
ggplotly(fig)

# Cálculo del IQR
Q1 <- quantile(df$Salary, 0.25)
Q3 <- quantile(df$Salary, 0.75)
IQR <- Q3 - Q1
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

# Preparación de datos para modelado
# Codificación de variables categóricas
df$`education_level` <- as.numeric(factor(df$`education_level`))
df$gender <- as.numeric(factor(df$gender))
dummies <- model.matrix(~ `job_title` - 1, data = df)
df <- cbind(df, dummies)
df$`job_title` <- NULL


# Calcular la Matriz de Correlacion
correlation_matrix <- cor(df %>% select_if(is.numeric))
print(correlation_matrix)

# Identificar variables correlacionadas con el Salario
correlation_with_salary <- correlation_matrix["salary",]
str(correlation_with_salary)

# Filtrar variables con una correlación absoluta mayor a 0.3 (puedes ajustar este umbral según sea necesario)
selected_variables <- names(correlation_with_salary[abs(correlation_with_salary) > 0.3])
selected_variables

# Crear un nuevo dataframe solo con las variables seleccionadas
df_selected <- df %>% select(all_of(selected_variables))

# Dividir datos en conjunto de entrenamiento y prueba
set.seed(90)
trainIndex <- createDataPartition(df_selected$salary, p = .8, 
                                  list = FALSE, 
                                  times = 1)
X_train <- df_selected[trainIndex, ]
X_test <- df_selected[-trainIndex, ]
y_train <- X_train$salary
y_test <- X_test$salary
X_train$salary <- NULL
X_test$salary <- NULL

# Ajustar el modelo de regresión múltiple
lm_model <- lm(y_train ~ ., data = X_train)
# Resumen del modelo
summary(lm_model)

# Predicciones en el conjunto de prueba
predictions_lm <- predict(lm_model, newdata = X_test)
# Calcular métricas de error
mse_lm <- mean((y_test - predictions_lm)^2)
mae_lm <- mean(abs(y_test - predictions_lm))
rmse_lm <- sqrt(mse_lm)
r_squared_lm <- summary(lm(lm_model))$r.squared

cat(sprintf("Mean Squared Error (LM): %.2f\n", mse_lm))
cat(sprintf("Mean Absolute Error (LM): %.2f\n", mae_lm))
cat(sprintf("Root Mean Squared Error (LM): %.2f\n", rmse_lm))
cat(sprintf("R-squared (LM): %.2f\n", r_squared_lm))

# Comparar salarios reales y predichos
predicted_df_lm <- data.frame(
  Actual_Salary = y_test,
  Predicted_Salary = predictions_lm
)

# Gráfico de salarios reales vs. predichos para el modelo de regresión múltiple
fig_lm <- plot_ly(data = predicted_df_lm, x = ~Actual_Salary, y = ~Predicted_Salary, type = 'scatter', mode = 'markers', text = ~Actual_Salary - Predicted_Salary)
fig_lm <- fig_lm %>% layout(title = "Salario Predicho vs. Salario Real (Regresión Múltiple)")
ggplotly(fig_lm)

# Gráfico de salarios reales vs. predichos para el modelo de regresión múltiple
predictions <- predict(lm_model, newdata = X_test)

# Combina los datos reales y predichos
comparison_df <- data.frame(
  Actual_Salary = y_test,
  Predicted_Salary = predictions
)

fig2 <- ggplot(comparison_df, aes(x = Actual_Salary, y = Predicted_Salary)) +
  geom_point(aes(color = abs(Actual_Salary - Predicted_Salary)), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Salarios Reales vs. Predichos",
       x = "Salario Real",
       y = "Salario Predicho") +
  scale_color_gradient(low = "green", high = "red") +
  theme_minimal()

print(fig2)
