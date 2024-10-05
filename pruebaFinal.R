# Cargar bibliotecas
library(tidyverse)
library(plotly)
library(caret)

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

#División de datos y modelado
# Dividir datos en conjunto de entrenamiento y prueba
set.seed(90)
trainIndex <- createDataPartition(df$salary, p = .8, 
                                  list = FALSE, 
                                  times = 1)
X_train <- df[trainIndex, ]
X_test <- df[-trainIndex, ]
y_train <- X_train$salary
y_test <- X_test$salary
X_train$salary <- NULL
X_test$salary <- NULL






