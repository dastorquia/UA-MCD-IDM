# ------------------------------------------------------------------------------
# INTRODUCCION A DATA MINING - UNIVERSIDAD AUSTRAL
# GRUPO 2
# TP 1 - EJERCICIO 3: PELICULAS

# Se dispone de un conjunto de datos que contiene información sobre las películas 
# vistas por distintos usuarios. Se solicita resolver las siguientes consignas:

# 1. Exploración inicial: determinar 
# la cantidad de usuarios y de películas diferentes que contiene el dataset.

library(arules)
library(arulesViz)
library(knitr)
library(ggplot2)

datos <- read.csv("datos_peliculas.csv", stringsAsFactors = FALSE)

peliculas <- as(split(datos$title, datos$userId),"transactions")

peliculas # hacemos un resumen de las filas y columnas

# 138493 transactions (rows) equivale a los usuarios
# 26729 items (columns) equivale a las peliculas

# ------------------------------------------------------------------------------
# 2. Conjuntos de ítems frecuentes: identificar los conjuntos de películas con 
# un soporte mínimo del 10%. ¿Cuáles son las películas más vistas por los usuarios?

# Itemsets frecuentes #
itemsets <- apriori(data = peliculas,
                    parameter = list(support = 0.10,
                                     minlen = 1,
                                     maxlen = 20,
                                     target = "frequent itemset"))

summary(itemsets)

# Hay 41283 itemsets que tienen un soporte mínimo del 10%
# Los conjuntos de items tienen, en promedio, 3 películas:

# set of 41283 itemsets
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.000   4.000   3.428   4.000   4.00

top_5_itemsets <- sort(itemsets, by = "support", decreasing = TRUE)[1:5]
inspect(top_5_itemsets)

# items                              support   count
# [1] {Pulp Fiction (1994)}              0.4860173 67310
# [2] {Forrest Gump (1994)}              0.4778003 66172
# [3] {Shawshank Redemption, The (1994)} 0.4575394 63366
# [4] {Silence of the Lambs, The (1991)} 0.4570556 63299
# [5] {Jurassic Park (1993)}             0.4311770 59715

# ------------------------------------------------------------------------------
# 3. Reglas de asociación: generar reglas con una confianza mínima del 80%. Ordenar
# las reglas obtenidas de mayor a menor lift y seleccionar las 10 primeras.

top_10_reglas_lift <- apriori(data = peliculas,
                  parameter = list(support = 0.1,
                                   minlen = 1,
                                   maxlen = 20,
                                   confidence = 0.80,
                                   target = "rules"))

summary(top_10_reglas_lift)

# Ordenamos los resultados por lift

top_10_reglas_lift <- sort(x = top_10_reglas_lift, decreasing = TRUE, by = "lift")[1:10]
inspect(sort(x = top_10_reglas_lift, decreasing = TRUE, by = "lift")[1:10])

# lhs                                                         rhs                          support confidence  coverage     lift count
# [1]  {Fight Club (1999),                                                                                                                 
#   Kill Bill: Vol. 1 (2003)}                               => {Kill Bill: Vol. 2 (2004)} 0.1009582  0.8113503 0.1244323 6.014041 13982
# [2]  {Kill Bill: Vol. 1 (2003),                                                                                                          
#   Pulp Fiction (1994)}                                    => {Kill Bill: Vol. 2 (2004)} 0.1032038  0.8077880 0.1277610 5.987635 14293
# [3]  {Fight Club (1999),                                                                                                                 
#   Kill Bill: Vol. 2 (2004)}                               => {Kill Bill: Vol. 1 (2003)} 0.1009582  0.9223564 0.1094568 5.874179 13982
# [4]  {Kill Bill: Vol. 2 (2004),                                                                                                          
#   Matrix, The (1999)}                                     => {Kill Bill: Vol. 1 (2003)} 0.1064314  0.9196406 0.1157315 5.856884 14740
# [5]  {Kill Bill: Vol. 2 (2004),                                                                                                          
#   Pulp Fiction (1994)}                                    => {Kill Bill: Vol. 1 (2003)} 0.1032038  0.9181602 0.1124028 5.847455 14293
# [6]  {Kill Bill: Vol. 2 (2004)}                               => {Kill Bill: Vol. 1 (2003)} 0.1210169  0.8970242 0.1349093 5.712847 16760
# [7]  {Alien (1979),                                                                                                                      
#   Star Wars: Episode VI - Return of the Jedi (1983),                                                                                 
#   Terminator, The (1984)}                                 => {Aliens (1986)}            0.1127783  0.8583283 0.1313929 4.369347 15619
# [8]  {Alien (1979),                                                                                                                      
#   Star Wars: Episode VI - Return of the Jedi (1983),                                                                                 
#   Terminator 2: Judgment Day (1991)}                      => {Aliens (1986)}            0.1140780  0.8546468 0.1334797 4.350606 15799
# [9]  {Alien (1979),                                                                                                                      
#   Star Wars: Episode V - The Empire Strikes Back (1980),                                                                             
#   Terminator, The (1984)}                                 => {Aliens (1986)}            0.1172406  0.8530076 0.1374438 4.342262 16237
# [10] {Alien (1979),                                                                                                                      
#   Terminator 2: Judgment Day (1991),                                                                                                 
#   Terminator, The (1984)}                                 => {Aliens (1986)}            0.1192696  0.8519703 0.1399926 4.336982 16518

# ------------------------------------------------------------------------------
# 4. Visualización: representar gráficamente las 10 reglas seleccionadas en el 
# punto anterior. Interpretar las principales asociaciones observadas.

# A continuación, presentamos los gráficos interactivos para una mejor interpretación.
# De todas formas, en modo comentario queda la resolución clásica según lo visto en clase.

# Grafico de nodos
plot(top_10_reglas_lift,
     method = "graph",
     engine = "htmlwidget",
     control = list(edgeCol = "black"))

# Grafico paracoord
plot(top_10_reglas_lift, method = "paracoord", control = list(reorder = F))


# ---------------------------------------------------------------------------------
# - forma de gráficos alternativa, se puede omitir y pasar a las interpretaciones -

# La forma alternativa de resolverlo es pasando las reglas a data.frame
# y eventualmente realizar un gráfico.

# Convertimos las reglas a data.frame
# tabla <- as(top_10_reglas_lift[1:10], "data.frame")

# Agregamos los identificadores
# tabla$id <- seq_len(nrow(tabla))
# 
# ggplot(tabla, aes(x = support, y = confidence, color = lift)) +
#   geom_point(size = 3) +
#   geom_text(aes(label = id), vjust = -0.7, size = 3) +
#   scale_color_gradient(
#     low = "lightblue",
#     high = "darkblue"
#   ) +
#   labs(
#     x = "Soporte",
#     y = "Confianza",
#     color = "Lift"
#   ) +
#   theme_minimal()

# Tabla para analizar cada punto en el gráfico realizado. En tabla.df_ordenados,
# quedan según lift para interpretar los datos más relevantes primero.

# tabla.df <- tabla[, c("id", "rules", "support", "confidence", "lift")]
# tabla.df_ordenados <- tabla.df[order(-tabla.df$lift),]
# ---------------------------------------------------------------------------------

# INTERPRETACION DE RESULTADOS: 
# Segun el ejercicio 3, donde ordenamos en orden decreciente el lift obtenemos
# una serie de peliculas denominadas con lhs (antecedente) y un rhs (consecuente)
# La interpretación es que los usuarios que vieron las peliculas de la  
# columna lhs, tienden a haber visto la pelicula en rhs. 

# Ejemplo la regla 1, quienes vieron 
# Fight Club (1999) y Kill Bill: Vol. 1 (2003), tienden a haber visto Kill Bill: Vol. 2 (2004).

# Analizando nuestros resultados, podemos destacar que de 10 peliculas como 
# antecedentes tenemos otras 3 peliculas como consecuentes. 

# Por un lado, tenemos las reglas 1, 2 que llevan al rhs de Kill Bill: Vol. 2 (2004),
# por otro lado las reglas 3, 4, 5 y 6 que llevan al rhs de Kill Bill: Vol. 1 (2003), 
# y por último las reglas 7, 8, 9 y 10 que llevan al rhs de Aliens (1986).

# La regla 1 es la que cuenta con un lift más alto y la que tiene una mayor tendencia de
# cumplirse dada que se vieron las peliculas del lhs. O sea, el lift de la regla 1 es
# 6.01, lo que significa que quienes vieron las peliculas del lhs de la regla 1 tienen 
# 6.01 más chances de haber visto Kill Bill: Vol. 2 (2004).

# Por otro lado, observamos que en la regla 3 presenta una confianza de 0.92 
# que es el valor más alto de todas las otras reglas. 
# La confianza dice qué tan seguido se cumple la regla, pero ignora la popularidad
# del rhs. Es decir, el 92% de las personas que vieron Fight Club y Kill Bill Vol. 2
# tienden a haber visto Kill Bill vol. 1.

# Continuando con las interpretaciones, de acuerdo a la confianza de la regla 1 
# el 81% de las personas que vieron Fight Club y Kill Bill Vol. 1, tienden a haber 
# visto Kill Bill vol. 2. Por lo que podria interpretarse que hay un 19% (100-81) 
# que vió KB1 y no KB2, por lo que tiene sentido recomendarles KB2 a esté grupo. 
# Esto también tiene sentido, ya que es la regla con el lift más elevado.

# ------------------------------------------------------------------------------
# 5. Sistema de recomendación: 
# a) se desea recomendar Forrest Gump a usuarios que 
# todavía no la vieron. ¿Qué otras películas deberían haber visto estos usuarios para 
# que la recomendación tenga mayor respaldo en las reglas de asociación? 
# Identificar las tres asociaciones más relevantes y justificar la selección. 

# Para este caso usamos una confianza de 0.5
reglas_Forrest <- apriori(
  data = peliculas,
  parameter = list(
    support = 0.1,
    confidence = 0.5,
    target = "rules"
  ),
  appearance = list(
    rhs = c("Forrest Gump (1994)")
  )
)

# Ordenamos por lift y pedimos los primeros 3 valores
inspect(head(sort(reglas_Forrest,
                  by = "lift"), 3)
        )

# lhs                                     rhs                     support confidence  coverage     lift count
# [1] {Apollo 13 (1995),                                                                                         
#   Mrs. Doubtfire (1993),                                                                                    
#   Seven (a.k.a. Se7en) (1995)}        => {Forrest Gump (1994)} 0.1034421  0.9653639 0.1071534 2.020434 14326
# [2] {Ace Ventura: Pet Detective (1994),                                                                        
#   Pretty Woman (1990),                                                                                      
#   Shawshank Redemption, The (1994)}   => {Forrest Gump (1994)} 0.1010665  0.9638480 0.1048573 2.017261 13997
# [3] {Ace Ventura: Pet Detective (1994),                                                                        
#   Apollo 13 (1995),                                                                                         
#   Pretty Woman (1990)}                => {Forrest Gump (1994)} 0.1043446  0.9635285 0.1082943 2.016592 14451

# ------------------------------------------------------------------------------

# b) considerando que Pulp Fiction es la película más vista, 
# identificar otras películas que podrían recomendarse a un usuario que ya la vio. 
# Justificar las recomendaciones a partir de las reglas de asociación obtenidas y 
# sus medidas de interés.

# Realizamos las reglas de asociación con una confianza de 0.5
reglas_Pulp_Fiction <- apriori(data = peliculas,
                               parameter = list(support = 0.1,
                                                confidence = 0.5,
                                                target = "rules"),
                               appearance = list(lhs = "Pulp Fiction (1994)"))

summary(reglas_Pulp_Fiction)

top_5_reglas_Pulp_Fiction_lift <- sort(reglas_Pulp_Fiction, by = "lift", decreasing = TRUE)[1:5]
inspect(top_5_reglas_Pulp_Fiction_lift)

# lhs                      rhs                           support   confidence coverage  lift     count
# [1] {Pulp Fiction (1994)} => {Seven (a.k.a. Se7en) (1995)} 0.2609663 0.5369484  0.4860173 1.719429 36142
# [2] {Pulp Fiction (1994)} => {Usual Suspects, The (1995)}  0.2657318 0.5467538  0.4860173 1.610892 36802
# [3] {Pulp Fiction (1994)} => {Batman (1989)}               0.2580852 0.5310207  0.4860173 1.596879 35743
# [4] {Pulp Fiction (1994)} => {Dances with Wolves (1990)}   0.2432470 0.5004903  0.4860173 1.567915 33688
# [5] {Pulp Fiction (1994)} => {Apollo 13 (1995)}            0.2614284 0.5378993  0.4860173 1.559229 36206

# De acuerdo a los resultados la pelicula que debería recomendarse en primer lugar es Seven (a.k.a. Se7en) (1995),
# que cuenta con el valor de lift más elevado. Y respecto a la confianza, podemos decir que
# de los que vieron Pulp Fiction, alrededor de la mitad también vio cada una de estas películas. 
# Visto al revés, entre el 45–50 % todavía no vio dichas peliculas (100% - confianza).
# Ese es el público al que se le pueden recomendar las películas.
