# EJERCICIO 3: PELICULAS

# Se dispone de un conjunto de datos que contiene información sobre las películas 
# vistas por distintos usuarios. Se solicita resolver las siguientes consignas:

# 1. Exploración inicial: determinar 
# la cantidad de usuarios y de películas diferentes que contiene el dataset.

library(arules)
library(arulesViz)
library(knitr)
library(ggplot2)

# datos <- read.csv(
#   "datos práctica/datos_peliculas.csv",
#   stringsAsFactors = FALSE
# )
# 
# peliculas <- as(split(datos$title, datos$userId), "transactions")

peliculas <- read.transactions(file = "datos práctica/datos_peliculas.csv",
                               format = "single",
                               sep = ",",
                               header = TRUE,
                               cols = c("userId", "title"),
                               rm.duplicates = TRUE) # La función agrupa las películas por usuario para construir las transacciones.

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

length(peliculas) # numero de usuarios
nitems(peliculas) # 409635

# Si el archivo tiene 138493 transacciones y el soporte es del 10%, entonces cada trasacción 
# debe aparecer al menos 138493x0.1 = 13849.3 veces

# En el archivo hay 4211 conjunto de películas que cumplen con el soporte mínimo.


top_10_itemsets <- sort(itemsets, by = "support", decreasing = TRUE)[1:10]
inspect(top_10_itemsets[1:10])


# {Shawshank Redemption, The (1994)}                                               0.4575592 63366
# {Jurassic Park (1993)}                                                           0.4311957 59715
# {Star Wars: Episode IV - A New Hope (1977)}                                      0.3935532 54502
# {Braveheart (1995)}                                                              0.3882603 53769
# {Matrix, The (1999)}                                                             0.3706774 51334
# {Toy Story (1995)}                                                               0.3588423 49695
# {Independence Day (a.k.a. ID4) (1996)}                                           0.3397286 47048
# {Usual Suspects, The (1995)}                                                     0.3394254 47006
# {Pulp Fiction (1994),                                                                           

# Para analizar cuáles son las películas más vistas por los usarios también puede usarse itemFrequent 

peliculas_mas_vistas <- itemFrequency(peliculas)

peliculas_mas_vistas <- sort(peliculas_mas_vistas, decreasing = TRUE)

peliculas_mas_vistas[1:10]


# 3. Reglas de asociación: generar reglas con una confianza mínima del 80%. 
# Ordenar las reglas obtenidas de mayor a menor lift y seleccionar las 10 primeras.

reglas <- apriori(data = peliculas,
                  parameter = list(support = 0.1,
                                   minlen = 1,
                                   maxlen = 20,
                                   confidence = 0.80,
                                   target = "rules"))
length(reglas)
summary(reglas)

top_10_reglas_lift <- sort(reglas, by = "lift", decreasing = TRUE)[1:10]
inspect(top_10_reglas_lift)


#lhs                                                                rhs                                                  support confidence  coverage     lift count
#[1]  {Independence Day (a.k.a. ID4) (1996),                                                                                                                             
#  Jurassic Park (1993),                                                                                                                                             
#  Matrix, The (1999),                                                                                                                                               
#  Star Wars: Episode IV - A New Hope (1977),                                                                                                                        
#   Star Wars: Episode V - The Empire Strikes Back (1980)}         => {Star Wars: Episode I - The Phantom Menace (1999)} 0.1024934  0.8356293 0.1226541 3.913025 14194
# [2]  {Independence Day (a.k.a. ID4) (1996),                                                                                                                             
#   Jurassic Park (1993),                                                                                                                                             
#   Matrix, The (1999),                                                                                                                                               
#   Star Wars: Episode V - The Empire Strikes Back (1980)}         => {Star Wars: Episode I - The Phantom Menace (1999)} 0.1061255  0.8234536 0.1288785 3.856009 14697
# [3]  {Independence Day (a.k.a. ID4) (1996),                                                                                                                             
#   Matrix, The (1999),                                                                                                                                               
#   Star Wars: Episode IV - A New Hope (1977),                                                                                                                        
#   Star Wars: Episode V - The Empire Strikes Back (1980)}         => {Star Wars: Episode I - The Phantom Menace (1999)} 0.1108191  0.8206074 0.1350452 3.842681 15347
# [4]  {Independence Day (a.k.a. ID4) (1996),                                                                                                                             
#   Jurassic Park (1993),                                                                                                                                             
#   Matrix, The (1999),                                                                                                                                               
#   Star Wars: Episode IV - A New Hope (1977)}                     => {Star Wars: Episode I - The Phantom Menace (1999)} 0.1067826  0.8106123 0.1317308 3.795877 14788
# [5]  {Finding Nemo (2003),                                                                                                                                              
#   Monsters, Inc. (2001)}                                         => {Shrek (2001)}                                     0.1022334  0.8714224 0.1173179 3.774574 14158
# [6]  {Independence Day (a.k.a. ID4) (1996),                                                                                                                             
#   Matrix, The (1999),                                                                                                                                               
#   Star Wars: Episode V - The Empire Strikes Back (1980)}         => {Star Wars: Episode I - The Phantom Menace (1999)} 0.1150577  0.8045443 0.1430098 3.767462 15934
# [7]  {Finding Nemo (2003),                                                                                                                                              
#   Toy Story (1995)}                                              => {Shrek (2001)}                                     0.1018146  0.8658806 0.1175850 3.750570 14100
# [8]  {Monsters, Inc. (2001),                                                                                                                                            
#   Toy Story (1995)}                                              => {Shrek (2001)}                                     0.1036271  0.8654043 0.1197441 3.748506 14351
# [9]  {Finding Nemo (2003),                                                                                                                                              
#   Pirates of the Caribbean: The Curse of the Black Pearl (2003)} => {Shrek (2001)}                                     0.1019446  0.8626421 0.1181772 3.736542 14118
# [10] {Lord of the Rings: The Two Towers, The (2002),                                                                                                                    
#   Monsters, Inc. (2001)}                                         => {Shrek (2001)}                                     0.1046091  0.8573204 0.1220187 3.713491 14487
# 


# 4. Visualización: representar gráficamente las 10 reglas seleccionadas en el punto anterior. 
#    Interpretar las principales asociaciones observadas.

plot(top_10_reglas_lift)

##Plot referenciando las reglas

# Convertir las reglas a data.frame

tabla <- as(top_10_reglas_lift[1:10], "data.frame")

# Agregar identificador

tabla$id <- seq_len(nrow(tabla))

ggplot(tabla, aes(x = support, y = confidence, color = lift)) +
  geom_point(size = 3) +
  geom_text(aes(label = id), vjust = -0.7, size = 3) +
  scale_color_gradient(
    low = "lightblue",
    high = "darkblue"
  ) +
  labs(
    x = "Soporte",
    y = "Confianza",
    color = "Lift"
  ) +
  theme_minimal()
#Correspondencia entre regla y punto
tabla.df <- tabla[, c("id", "rules", "support", "confidence", "lift")]

tabla.df_ordenados <- tabla.df[order(-tabla.df$lift), ]

rules
# {Independence Day (a.k.a. ID4) (1996),Jurassic Park (1993),Matrix, The (1999),Star Wars: Episode IV - A New Hope (1977),Star Wars: Episode V - The Empire Strikes Back (1980)} => {Star Wars: Episode I - The Phantom Menace (1999)}
# {Independence Day (a.k.a. ID4) (1996),Jurassic Park (1993),Matrix, The (1999),Star Wars: Episode V - The Empire Strikes Back (1980)} => {Star Wars: Episode I - The Phantom Menace (1999)}
# {Independence Day (a.k.a. ID4) (1996),Matrix, The (1999),Star Wars: Episode IV - A New Hope (1977),Star Wars: Episode V - The Empire Strikes Back (1980)} => {Star Wars: Episode I - The Phantom Menace (1999)}

plot(top_10_reglas_lift, method = "graph")



plot(top_10_reglas_lift, method = "paracoord", control = list(reorder = F))

# 5. Sistema de recomendación: 
# a) se desea recomendar Forrest Gump a usuarios que 
# todavía no la vieron. ¿Qué otras películas deberían haber visto estos usuarios para 
# que la recomendación tenga mayor respaldo en las reglas de asociación? 
# Identificar las tres asociaciones más relevantes y justificar la selección. 

filtrado_reglas_Forrest_Gump <- subset(x = reglas,
                                       subset = rhs %in% "Forest Gump")


length(reglas)


# uusamos esta función para ver qué nombre tiene exactamente en el archivo
etiquetas <- itemLabels(peliculas)

etiquetas[grepl("Gump", etiquetas, ignore.case = TRUE)]


############################################
# PENDIENTE - NO ENCONTRAMOS FORREST GUMP
###########################################


# b) considerando que Pulp Fiction es la película más vista, 
# identificar otras películas que podrían recomendarse a un usuario que ya la vio. 
# Justificar las recomendaciones a partir de las reglas de asociación obtenidas y 
# sus medidas de interés.

reglas_Pulp_Fiction <- apriori(data = peliculas,
                              parameter = list(support = 0.1,
                                               minlen = 1,
                                               maxlen = 20,
                                               confidence = 0.5,
                                               target = "rules"),
                              appearance = list(lhs = "Pulp Fiction (1994)"))
summary(reglas_Pulp_Fiction)

top_5_reglas_Pulp_Fiction_lift <- sort(reglas_Pulp_Fiction, by = "lift", decreasing = TRUE)[1:5]
inspect(top_5_reglas_Pulp_Fiction_lift)


# {Pulp Fiction (1994)} => {Seven (a.k.a. Se7en) (1995)}      0.2609776 0.5369484  0.4860384 1.719355 36142
# {Pulp Fiction (1994)} => {Usual Suspects, The (1995)}       0.2657434 0.5467538  0.4860384 1.610822 36802
# {Pulp Fiction (1994)} => {Shawshank Redemption, The (1994)} 0.3305003 0.6799881  0.4860384 1.486121 45770
# {Pulp Fiction (1994)} => {Braveheart (1995)}                0.2800263 0.5761402  0.4860384 1.483902 38780
# {Pulp Fiction (1994)} => {Jurassic Park (1993)}             0.3003820 0.6180211  0.4860384 1.433273 41599

# En todos los casos, la probabilidad de que el usuario mire la película recomendada, dado que ha visto Pulp Fiction es superior al 50%
# En todos los casos los valores del lift es mayor a 1, es decir frecuencia de ocurrencia de ambas películas es mayor que si la eleccion de las 
# películas fuera independiente (lift = 1)

##############################################################################################
# QUEDA PENDIENTE:
# - PUNTO 5 - A)
# - CHEQUEAR INTERPRETACIONES
# - INTENTAR MEJORAR EL GRÁFICO REALIZADO CON method = "graph" (VER SI SE PUEDEN PONER LAS FLECHAS MÁS VISIBLES)
# DEFINIR CÓMO LO VAMOS A PRESENTAR (R MARKDOWN U OTRO FORMATO O SOLO EL CÓDIGO DE R)
#################################################################################################
