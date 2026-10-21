# Introducción a Data Mining - Maestría en Ciencia de Datos - Universidad Austral 
# Clase 17-10-26
# EJERCICIO 1: PANADERIA

library(arules)
library(arulesViz)

#1) Analizar la distribución de la cantidad de productos por transacción.
# ¿Cuántos productos se adquieren habitualmente en una compra?

# Leer los datos y darle formato de transacciones
transacciones <- read.transactions(file = "datos_panaderia.csv",
                                   format = "single",
                                   sep = ";",
                                   header = TRUE,
                                   cols = c("Transaction", "Item"),
                                   rm.duplicates = TRUE)
transacciones

inspect(transacciones[1:5])
tamanios <- size(transacciones)
summary(tamanios) # en promedio hay 2 items por transaccion

# ------------------------------------------------------------------------------
# 2) Identificar los 10 conjuntos de ítems (itemsets) con mayor soporte, considerando: 
# a) todos los conjuntos 
##### Itemsets frecuentes #####
itemsets <- apriori(data = transacciones,
                    parameter = list(support = 0.01,
                                     minlen = 1,
                                     maxlen = 20,
                                     target = "frequent itemset"))

# Se muestran los top 10 itemsets de mayor a menor soporte
top_10_itemsets_todos <- sort(itemsets, by = "support", decreasing = TRUE)[1:10]
inspect(top_10_itemsets_todos)

# b) al menos 2 items 
##### Itemsets frecuentes #####
itemsets <- apriori(data = transacciones,
                    parameter = list(support = 0.01,
                                     minlen = 2,
                                     maxlen = 20,
                                     target = "frequent itemset"))

# Se muestran los top 10 itemsets de mayor a menor soporte
top_10_itemsets_dositem <- sort(itemsets, by = "support", decreasing = TRUE)[1:10]
inspect(top_10_itemsets_dositem)

# ------------------------------------------------------------------------------
# 3) Identificar todos los conjuntos de ítems que incluyan el producto Coffee. 
itemsets_filtrados_coffee <- arules::subset(itemsets,
                                            subset = items %ain% "Coffee")
itemsets_filtrados_coffee





# ------------------------------------------------------------------------------
# EJERCICIO 3: PELICULAS

# Se dispone de un conjunto de datos que contiene información sobre las películas 
# vistas por distintos usuarios. Se solicita resolver las siguientes consignas:

# 1. Exploración inicial: determinar 
# la cantidad de usuarios y de películas diferentes que contiene el dataset.

peliculas <- read.transactions(file = "datos_peliculas/datos_peliculas.csv",
                               format = "single",
                               sep = ",",
                               header = TRUE,
                               cols = c("userId", "title"),
                               rm.duplicates = TRUE)

peliculas # hacemos un resumen de las filas y columnas
# 138493 transactions (rows) equivale a los usuarios
# 26729 items (columns) equivale a las peliculas

# ------------------------------------------------------------------------------
# 2. Conjuntos de ítems frecuentes: identificar los conjuntos de películas con 
# un soporte mínimo del 10%. ¿Cuáles son las películas más vistas por los usuarios?

##### Itemsets frecuentes #####
itemsets <- apriori(data = peliculas,
                    parameter = list(support = 0.1,
                                     minlen = 1,
                                     maxlen = 20,
                                     target = "frequent itemset"))

summary(itemsets)

top_20_itemsets <- sort(itemsets, by = "support", decreasing = TRUE)[1:20]
inspect(top_20_itemsets)
