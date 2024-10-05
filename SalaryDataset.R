#SALARY EDA and PREDICTION
# Cargar librerías
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(broom)
library(forcats)
library(gridExtra)

# Leer el archivo CSV
df <- read.csv("D:/ICO - AXEL JIMENEZ PEREZ/8.OCTAVO SEMESTRE/CIENCIA DE DATOS/FIRST EXAM/Salary_Data.csv")
View(df)

# Descubrimient de los datos
# Imprimir las primeras dos filas del dataframe
head(df, 2)
cat('----------------------------\n')
# Imprimir información sobre el dataframe
str(df)
cat('----------------------------\n')
# Resumen estadístico del dataframe
summary(df)
# Datos Faltantes
colSums(is.na(df))

# -----------------
# EDA (LIMPIEZA)
# -----------------
# Renombrar todas las columnas
colnames(df) <- c("age", "gender", "education", "job", "years_exp", "salary")
colnames(df)
# Valores Faltantes
# Calcular el porcentaje de valores faltantes
missing_percentage <- sum(is.na(df)) / nrow(df) * 100
cat(sprintf("Number of missing values is %.2f%%\n\n", missing_percentage))

# Filtrar las filas que tienen valores faltantes
missing_values_rows <- df[rowSums(is.na(df)) > 0, ]

# Mostrar las filas con valores faltantes
missing_values_rows

# Eliminar filas con valores faltantes
df <- na.omit(df)
dim(df)

# Calcular la edad promedio y el salario promedio por educación y género
avg_values <- df_outliers %>%
  group_by(education, gender) %>%
  summarize(avg_age = mean(age, na.rm = TRUE), 
            avg_salary = mean(salary, na.rm = TRUE))

# Crear gráficos de barras
p1 <- ggplot(avg_values, aes(x = education, y = avg_age, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle('Outliers in Education vs Avg Age') +
  theme_minimal()

p2 <- ggplot(avg_values, aes(x = education, y = avg_salary, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle('Outliers in Education vs Avg Salary') +
  theme_minimal()

# Combinar los gráficos en un solo layout
grid.arrange(p1, p2, ncol = 2)

# Filtrar el dataframe para obtener solo las filas con educación "High School"
high_school_outliers <- df_outliers[df_outliers$education == "High School", ]

# Mostrar el resultado
high_school_outliers
