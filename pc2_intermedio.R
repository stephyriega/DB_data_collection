# Práctica Calificada 02 ----
### Integrantes:
# 1.
# 2.
# 3.

## Librerias requeridas

library(readr)
library(solitude)
library(pacman)
p_load(tictoc, tidyverse, data.table, outliers, caret,
       caTools, pROC)


## 1. Base de datos ----
### a.Importamos la base de datos de github ----

url <- "https://raw.githubusercontent.com/ChristianChiroqueR/banco_de_datos/main/PaySim.csv"
base <- read_csv(url) 
View(base)

### b. Identificamos variable objetivo "class" ----
addmargins(table(base$class))

round(prop.table(table(base$class)) * 100, 2)

# El 99.87% (99874 registros) de los registros no son fraudulentas; mientras 
# que el 0.13% (126 registros) son fraudulentas.


## 2. Preprocesamiento ----

### a. Division en data de entrenamiento y data de testeo ----

# Fijamos la semilla
set.seed(2023) 

# El 20% de la data será de evaluación

index         <- createDataPartition(base$class, 
                                     p = 0.8, 
                                     list = FALSE)
head(index)
train   <- base[ index, ]  # 80001 registros 
View(train)
testing <- base[-index, ]  # 19999  registros
View(testing)


# Cantidad de registros de data original
table(base$class)
round(prop.table(table(base$class)) * 100, 2)

# Los datos de entrenamiento
table(train$class)
round(prop.table(table(train$class)) * 100, 2)

# Los datos de evaluación
table(testing$class)
round(prop.table(table(testing$class)) * 100, 2)

# Tanto en la data de entrenamiento como en la de evaluación se mantiene la 
# proprocionalidad con la data oiginal (0.13% y 99.87%).


## 3. Algoritmo Isolation Forest ----

### a.Condiciones para el bosque

# Establecemos los hiperparámetros

isoforest <- isolationForest$new(
  sample_size = 1000, # Tamaño de la submuestra para construir cada árbol
  num_trees   = 100,  # Número de árboles
  replace     = TRUE, # Pido que si reemplace
  seed        = 2023  # Semilla
)

### b. Pedimos al algoritmo  que se ajuste a la data ----

# Ajustamos  
tic()
isoforest$fit(dataset = train |> select(-class))
toc()
# 3.84 segundos en demorarse 


### c. Predicciones de aislamiento ----
# Predecir la clase con la data de evaluacion

tic()
predicciones <- isoforest$predict(
  data = testing |> select(-class))
toc()
# 1.14 segundos en demorarse 

# Revisamos las predicciones, 
head(predicciones)

### d. Calculo del anomaly_score ----
summary(predicciones)
quantile(predicciones$anomaly_score, seq(0, 1, 0.05))

# El mínimo del anomaly_score es 0.586 y el máximo es 0.8119. Se concentra la 
# mayoría de los casos con una aproximación a cero del anomaly_Score. 
# El percentil 95 tiene 0.65 de anomaly_score y en el percentil 100 encontramos
# un 0.81 de anomaly_score, este último siendo próximo a uno e indicando anomalías.


# Visualizamos los resultados

ggplot(data = predicciones, aes(x = anomaly_score)) +
  geom_histogram(color = "gray40") +
  geom_vline(
    xintercept = quantile(predicciones$anomaly_score, seq(0, 1, 0.05)),
    color      = "red",
    linetype   = "dashed") +
  labs(
    title = "Distribución del anomaly_score del Isolation Forest",
    subtitle = "Cuantiles marcados en rojo"  ) +
  theme_bw()


## 4. Evaluar performance del IF ----

testing$anomaly_score = predicciones$anomaly_score

# Debemos corroborar diferencia y recordar la interpretación del anomaly_score
ggplot(testing, aes(class, anomaly_score)) + 
  geom_boxplot(fill = c("cadetblue", "firebrick1")) 

# El gráfico nos indica que los valores de anomalia de los casos de fraude son 
# más altos que los casos que no son fraude. Si tenemos outliers para los casos
# no fraude.
# Si es propicio aplicar el algoritmo.


## 
