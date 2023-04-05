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
## Importamos la base de datos de github

url <- "https://raw.githubusercontent.com/ChristianChiroqueR/banco_de_datos/main/PaySim.csv"
base <- read_csv(url) 
View(base)

## Variable objetivo "class"
addmargins(table(base$class))

round(prop.table(table(base$class)) * 100, 2)

## El 99.87% (99874 registros) de los registros no son fraudulentas; mientras 
## que el 0.13% (126 registros) son fraudulentas.



## 2. Preprocesamiento ----

### a. Division en data de entrenamiento y data de testeo ----

set.seed(1000) 

# Creamos una data teniendo en cuenta la variable class.

index         <- createDataPartition(base$class, 
                                     p = 0.8, 
                                     list = FALSE)
head(index)
train   <- base[ index, ]  # 80001 registros
View(train)
testing <- base[-index, ]  # 19999  registros
View(testing)
# Varia las frecuencias

# Los datos originales
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


## Paso 3. ¿Algoritmo IF es propicio? ----

### a. Establecemos los hiperparámetros----

isoforest <- isolationForest$new(
  sample_size = 1000, # Tamaño de la submuestra para construir cada árbol
  num_trees   = 100,  # Número de árboles
  replace     = TRUE, # 
  seed        = 2000  # Semilla
)

### b. Pedimos al algoritmo  que se ajuste ----

tic()
isoforest$fit(dataset = train %>% select(-class))
toc()

# 3.95 segundos en demorarse 


### c. Predicciones de aislamiento ----

tic()
predicciones <- isoforest$predict(
  data = testing %>% select(-class))
toc()





## Paso 4. Clasificación ----



## Paso 5. Evaluar performance del IF ----



## Paso 6. Comparación con regresión logística ----


