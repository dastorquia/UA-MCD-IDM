# =============================================================
# INTRODUCCIÓN A DATA MINING - UNIVERSIDAD AUSTRAL
# GRUPO 2 - TP 2: DAMASCOS
# Análisis de Componentes Principales
# =============================================================

knitr::opts_chunk$set(echo = TRUE)

# -------------------------------------------------------------
# 0. Paquetes
# -------------------------------------------------------------
listofpackages <- c("psych", "ggplot2", "knitr", "pastecs", "FactoMineR",
                    "grid", "gridExtra", "ggfortify", "factoextra",
                    "corrplot", "dplyr", "plotly")

newPackages <- listofpackages[!(listofpackages %in% installed.packages()[, "Package"])]
if (length(newPackages)) install.packages(newPackages)

for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}

# -------------------------------------------------------------
# Lectura de datos
# -------------------------------------------------------------
setwd("~/UA-MCD-IDM/TP2")

# Se usan las variedades como nombres de fila y se quita la columna VAR,
# para quedarnos solo con las 13 variables numéricas.
dfDamas <- read.table(paste0("damas.txt"),
                      sep = "\t",
                      dec = ",",
                      header = TRUE,
                     # row.names = 1,  
                      fileEncoding = "UTF-8")

knitr::kable(head(dfDamas))   # primeras filas del dataset

# Los datos están formados por 18 variedades distintas de damascos. Con el fin de caracterizar la colección de
# variedades se tomaron muestras de flores, hojas y frutos registrándose los principales caracteres
# cuantitativos de estos órganos.
# Las flores se recogieron en plena floración, eligiéndose aquellas totalmente desarrolladas y abiertas. Los
# caracteres medidos fueron : tamaño de la flor (TAMFLOR), longitud de pétalo (LONGPET) y ancho de pétalo
# (ANCHOPET).
# Las hojas fueron muestreadas en el mes de mayo, se eligieron hojas adultas procedentes de ramos mixtos,
# aproximadamente de la parte central de éstos, y se seleccionaron aquellas en buen estado fisiológico y
# sanitario. Los caracteres medidos fueron: superficie de hoja (SUPHOJA), relación longitud y ancho de hoja
# (LONANCHO), relación peciolo-limbo (PECLIMBO).
# Por otra parte, los frutos se recolectaron maduros, tomando como referencia la desaparición total del color
# verde, se muestrearon al azar de todas las caras del árbol. Los caracteres registrados fueron: peso de fruto
# (PESOF), longitud de fruto (LONGF), ancho de fruto (ANCHOF), espesor de fruto (ESPESORF), peso del
# endocarpio (PESOEND), longitud del endocarpio (LONGEND) y ancho del endocarpio (ANCHOEND).
# El tamaño de la muestra para cada órgano y variedad fue de 25 repeticiones.
# Cada fila representa una variedad y cada valor corresponde al resumen (promedio) de esas 25 repeticiones.
# Todas los registros de peso están expresados en gramos, los de longitud en milímetros y superficie en
# centímetros cuadrados.

# -------------------------------------------------------------
# Resumen de variables
# -------------------------------------------------------------
# VAR: Tipo de variedad de damasco - variable cualitativa
# TAMFLOR: Tamaño de la flor | Unidad: milímetros (mm)
# LONGPET: Longitud del pétalo | Unidad: milímetros (mm)
# ANCHOPET: Ancho del pétalo | Unidad: milímetros (mm)
# SUPHOJA: Superficie de hoja | Unidad: centímetros cuadrado (cm^2)
# LONANCHO: Relación longitud y ancho de hoja | Unidad: [Adimensional]
# PECLIMBO: Relación peciolo-limbo | Unidad: [Adimensional]
# PESOF: Peso del fruto | Unidad: gramos (g)
# LONGF: Longitud del fruto | Unidad: milímetros (mm)
# ANCHOF: Ancho del fruto | Unidad: milímetros (mm)
# ESPESORF: Espesor del fruto | Unidad: milímetros (mm)
# PESOEND: Peso del endocarpio | Unidad: gramos (g)
# LONGEND: Longitud del endocarpio | Unidad: milímetros (mm)
# ANCHOEND: Ancho del endocarpio | Unidad: milímetros (mm)

# -------------------------------------------------------------
# 1. EDA - Análisis Exploratorio de Datos
# -------------------------------------------------------------
# Para las variables del data set, se analizan las métricas estadísticas

res <- pastecs::stat.desc(dfDamas[,-1]) %>% as.matrix %>% as.data.frame %>% round(2)
res <- format(res, scientific = FALSE)
knitr::kable(data.frame(res), digits = 2)

# Podemos observar que no hay valores faltantes.
# Se puede observar que en su mayoría la media y la mediana se aproximan, lo que
# sugiere que las distribuciones son aproximadamente simetricas.

# Se observan valores de varianza con una gran variabilidad (de 0 a 144), esto 
# se debe a que estamos analizando datos con distintas escalas y unidades.

# El coeficiente de variación nos permite comparar la variabilidad de los
# distintos atributos, aún cuando estén medidos en distintas unidades o la
# magnitud que tomen cada uno de ellos varie.
# Para este conjunto de datos podemos ver que son bastante homogéneos
# (CV entre 6 a 14%). Las excepciones son el peso del endocarpio 
# (PESOEND, CV = 42 %), el peso del fruto (PESOF, CV = 26 %) y la longitud del 
# endocarpio (LONGEND, CV 21 %).

# -------------------------------------------------------------
# Boxplots
# -------------------------------------------------------------
# Se realizan los boxplots para ver descriptivamente las variables y detectar outliers. 

dfBox <- stack(dfDamas[,-1])
names(dfBox) <- c("valor", "variable")
ggplot(dfBox, aes(x = variable, y = valor)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Tal como se mencionó en el análisis exploratorio, es dificil comparar las variables,
# ya que poseen distintas magnitudes y unidades. 
# Sin embargo, lo que llama la atención, son las figuras de LONANCHO (relación longitud y ancho de hoja)
# y PECLIMBO (relación peciolo-limbo), que presentan un rango muy bajo lo que provoca que 
# el boxplot se vea más estrecho.

# A su vez, notamos que las variables SUPHOJA, PESOF, ANCHOF y ESPESORF presentan outliers. 

# Vamos a utilizar el conjunto completo del dataset, incluyendo los outliers. 
# No hizo falta descartar ningún registro por ausencia de datos faltantes.

# -------------------------------------------------------------
# Correlaciones
# -------------------------------------------------------------
# Se realiza una matriz de correlación, dónde en la diagonal se
# ven las variables con su histograma y correspondiente 
# curva de densidad. Por debajo de la misma, los distintos
# gráficos de dispersión y por encima los coeficientes de correlación.

pairs.panels(dfDamas[,-1], 
             method = "pearson", # Método de correlación
             hist.col = "#00AFBB",
             density = TRUE,  # Muestra los gráficos de densidad
             ellipses = F) # Si es T muestra elipses de correelación

# Los coeficiente de correlación más altos (0,87 - 0,99) se observan
# en las variables relacionadas al tamaño del fruto (PESOF, ANCHOF, LONGF, ESPESORF).
# Tiene sentido ya que un fruto más grande tiende a ser más pesado.
# Asimismo, se presenta valor de coeficiente de correlación elevado entre el 
# la longitud y el ancho del pétalo (LONGPET y ANCHOPET), como también en
# la longitud del endocarpio y el ancho del endocarpio (LONGEND y ANCHOEND).

# Con respecto a los gráficos de dispersión que se presentan debajo de la
# diagonal del gráfico, hay algunas relaciones que no son lineales, por ejemplo
# PECLIMBO con PESOF y,  PESOEND con ANCHOPE.

# -------------------------------------------------------------
# 2. PCA - Análisis de Componentes Principales
# -------------------------------------------------------------
# Dado que en el EDA se observaron varias relaciones lineales entre las variables
# con valores de coeficientes de correlación altos, se decide continuar con un 
# PCA para reducir la dimensionalidad, capturando la mayor variabilidad posible
# del conjunto de datos. Queremos mencionar que somos conscientes que existen 
# algunas relaciones no lineales y que PCA no logra captar esta 
# información al momento de reducir la variabilidad.

# El PCA puede hacerse sobre la matriz de varianzas y covarianzas o sobre la 
# matriz de correlaciones. En este análisis se usa la matriz de CORRELACIONES porque
# las variables están medidas en distintas unidades (g, mm, cm2 y cocientes
# adimensionales) y con escalas muy diferentes: las varianzas van de 0,001
# (PECLIMBO) a 144 (PESOF).
# Con la matriz de covarianzas, las variables de mayor varianza dominarían las
# componentes solo por su escala y magnitud. 

X <- dfDamas[,-1]

damaPca <- prcomp(as.matrix(X),center = T, scale. = T) 

summary(damaPca)

#-----------------------------------------------------------------------------
# Autovalores y proporción de variancia explicadada
#------------------------------------------------------------------------------
eig <- get_eigenvalue(damaPca)
knitr::kable(round(eig, 2))   # autovalor, % varianza, % acumulado

# eigenvalue: el autovalor de cada componente (7,13; 2,06; 1,23; 1,01...)
# variance.percent: el % de varianza que explica esa componente (54,8; 15,9; 9,4; 7,8...)
# cumulative.variance.percent: el % acumulado (54,8; 70,7; 80,2; 87,9...)

# Para nuestro análisis, debemos elegir cuantas componentes retener. 
# A medida que se agreguen más componentes, la complejidad del análisis aumenta 
# ya que se deben empezar a considerar más combinaciones entre las componentes. 

# Criterios para elegir el número de componentes principales

# Criterio 1: Seleccionar los autovalores cuyo porcentaje de variabilidad explicada 
# acumule entre 70-80%
# Se observa que la componente PC1 explica el 54,8 % de la variabilidad total y la PC2 un
# 15,9 % extra, dando un acumulado de 70,7 %. Para nuestro caso, consideramos 
# que es suficiente porque llegamos al umbral de 70-80 % de variabilidad total. 

# Criterio 2: Criterio de Kaiser
# Según este criterio (autovalores > 1), podríamos incluir hasta la PC4,
# cuyo autovalor es 1,01 y su variabilidad acumulada es 87,9 %. 
# Sin embargo decidimos quedarnos hasta la PC2/PC3 porque la variabilidad
# acumulada es de 80,1 %, la cual es suficiente. Además, sumar una componente más,
# agregaría un grado de complejidad extra al análisis.


# Criterio 3: Scree plot 
# Por último, otro criterio para elegir la cantidad de componentes es visualizar los 
# valores de las componentes a elegir de manera
# descriptiva con un scree plot. Puede observarse una fuerte caida desde PC1 a PC2,
# y luego la curva se aplana a partir de PC3-PC4.

fviz_eig(damaPca, addlabels = TRUE, ylim = c(0, 60))

#-----------------------------------------------------------------------
# ¿Cuántas componentes retener?
#-----------------------------------------------------------------------

# A partir del análisis de los 3 criterios mencionados puede pensarse que es 
# recomendable retener las primeras 3 componentes principales.

var <- get_pca_var(damaPca)          

round(var$cos2[, 1:4], 2)            # (a) calidad de representación por componente
round(rowSums(var$cos2[, 1:2]), 2)   # (b) cos2 acumulado en PC1+PC2, dice qué proporción de la variabilidad de una variable queda capturada.
round(var$contrib[, 1:4], 1)         # (c) contribución (%) de cada variable a cada PC
round(damaPca$rotation[, 3:4], 2)    # (d) cargas de PC3 y PC4

# PECLIMBO tiene cos² = 0,10 en PC1–PC2. Es el resultado de (b).
# Quiere decir que con solo dos componentes se pierde el 90 % de lo que varía PECLIMBO.

# PC3 está mayormente conformada por PECLIMBO. Es el resultado de (c) y (d). 
# En PC3 la carga de PECLIMBO es 0,76, y contribuye 57,4 % de esa componente. 
# Al agregar PC3, pasamos a capturar el 80,15% de la variabilidad de las componentes.

#--------------------------------------------------------------------------------
# Correlaciones entre cada variable y cada componente
#--------------------------------------------------------------------------------

# La correlación entre cada variable estandarizada y las componentes principales permite evaluar qué tan
# bien representada está cada variable por cada componente principal.

X.est <- scale(dfDamas[-1])
cor(X.est, damaPca$x)

# La interpretación de las correlaciones es análoga a la matriz de cargas presentada a continuación.

# -------------------------------------------------------------
# MATRIZ DE CARGAS
# -------------------------------------------------------------
# Matriz de los vectores propios
knitr::kable(as.data.frame(unclass(damaPca$rotation)))

# Matriz de puntuaciones (scores) 
knitr::kable(damaPca$x)

# Gráfico de scores para PC1, PC2 y PC3
fviz_contrib(damaPca, choice = "var", axes = 1, top = 10)
fviz_contrib(damaPca, choice = "var", axes = 2, top = 10)
fviz_contrib(damaPca, choice = "var", axes = 3, top = 10)

# La línea punteada, es la línea 1/n, expresada en porcentaje. 
# Donde n es la cantidad de observaciones. En este caso 1/13 *100, igual a 7,69%.

# Gráfico variables - PCA
var <- get_pca_var(damaPca)
tabla_cos2 <- knitr::kable(var$cos2)
tabla_contrib <- knitr::kable(var$contrib)
tabla_coord <- knitr::kable(var$coord)
corrplot(var$contrib, is.corr=FALSE)

# A partir del análisis del gráfico de variables - PCA y de la tabla tabla_contrib puede 
# observarse que las variables que describen las características del fruto y del endocarpio tiene alto aporte a la componente CP1
# Mientras que las variables que describen las características de la flor tienen un aporte moderado a dicha componente. 
# Además, observando el ángulo formado por los vectores que representan dichas variables se observa una alta correlación 
# entre las variables que miden cada una de las características antes mencionadas. Por ejemplo, en el segundo cuadrante 
# del plano cartesiano se observa que las variables que caracterizan el tamaño de la flor (TAMFLOR, LONGPET y ANCHOPET) están altamente correlacionadas 
# entre si como también lo están las variables que describen las características del endocarpio (PESOEND, ANCHOEND, LONGEND). Por otro lado, en el tercer
# cuadrante se observa que están altamente correlacionadas todas las variables que describen las características del
# fruto (ANCHOF, PESO F, ESPESORF, LONANCHO y LONGF) .
# Analizando la PC2, puede observarse que el mayor aporte a dicha componente está dado por la variable SUPHOJA seguida por las variables
# ANOHOPET, LONGPET y en menor medida tamaño de la flor. Es por ello que puede pensarse que la PC2 tomará valores altos cuando
# las variedades tengan flores y hojas grandes. Además, dado que el aporte de las variables relacionadas el fruto es moderado y, observando que los
# pesos toman valores negativos, se esperaría que las variedades que están ubicadas en los valores más extremos positivos de la PC2, tengan frutos pequeños.

# Graficos e interpretaciones PC1 y PC2
# Biplot PC1-PC2
p12 <- autoplot(
  damaPca,
  data = dfDamas,
  colour = 'VAR',
  loadings = TRUE,
  loadings.label = TRUE,
  loadings.label.size = 3,
  scale = 0)

ggplotly(p12)

# Interpretaciones Biplot PC1-PC2
# Observando el vector propio de carga para la componente principal 1 se observa que hay
# un ordenamiento natural de las observaciones ya que la mayoría de las variables tienen asociado un signo
# negativo y, las que tienen signo positivo, la magnitud es muy pequeña. Estarán más a la izquierda del gráfico
# aquellas variedades que presenten valores grandes en las variables relacionadas con el tamaño tanto de la 
# flor como del fruto y del endocarpio. Es decir, la PC1 es un eje de tamaño.
# En cuanto a la CP2 tomará valores altos positivos cuando las variedades tengan superficie de hoja grande y también valores
# altos en las variables relacionadas al tamaño de la flor y tomará valores grandes negativos con respecto a esa componente
# cuando los frutos sean grandes. Es decir, la PC2 es un eje de forma.
# Resumiendo, en el segundo cuadrante estarán las variedades que presenten valores altos en 
# las variables relacionadas con el tamaño de la flor y presenten hojas grandes. En el tercer cuadrante estarán
# aquellas con valores altos en el tamaño del fruto y en el endorcarpio.
# La variedad BLANCO se caracteriza por tener flores y hojas grandes
# LaS variedades TADEO y CANINO tiene frutos grandes
# La variedad GABACHET tiene frutos y hojas pequeñas.Este es una variedad que suele usarse de forma decorativa.
# Las variedades GANDIA, GINESTA y PALABRAS se caracterizan por tener flores pequeñas.
# Las variedades BULIDA, CHIRIVEL y R. CARLET, que se encuentran más cerca del centro
# del gráfico, poseen variables con caracteristicas promedios. Es decir, no tienen una
# una característica distintiva. Ejemplo, no tienen ni frutos grandes, ni hojas grandes, etc.

# Gráfico de scores para PC1-PC2
fviz_contrib(damaPca, choice = "var", axes = 1:2)

# Circulo de correlaciones PC1-PC2
fviz_pca_var(damaPca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

# El círculo de correlaciones aporta la calidad de representación de cada variable 
# en el plano PC1–PC2 (cos2). La mayoría de las variables de tamaño 
# (fruto, endocarpio, pétalo) están muy bien representadas, con flechas largas, cercanas 
# al borde y en naranja fuerte, por lo que su interpretación es confiable.
# En cambio, PECLIMBO queda menos representada, con una flecha corta y en color celeste, cos2 bajo.
# Su variación no está capturada en este plano, por lo que no conviene interpretarla acá.

# Graficos e interpretaciones PC2 y PC3
# Biplot PC2-PC3
p23 <- autoplot(
  damaPca,
  x = 2,
  y = 3,
  data = dfDamas,
  colour = 'VAR',
  loadings = TRUE,
  loadings.label = TRUE,
  loadings.label.size = 3,
  scale = 0)

ggplotly(p23)

# Interpretaciones Biplot PC2-PC3
# Se siguen observando las variables relacionadas al tamaño de la flor en un cuadrante 
# y las relacionadas al fruto en otro.A diferencia de PC1-PC2 donde ocurria en el cuadrante 2 y 3, 
# ahora al graficar PC2-PC3 esta diferencia se nota en los cuadrantes 3 y 4, 
# dado que ahora PC2 es la ordenada.
# En conclusión, PC2 es un eje de forma ya que distingue entre fruto y flor.
# Notamos tambien, como era de esperarse luego de haber realizado los analisis de la 
# importancia de la tercer componente, que PECLIMBO toma un protagonismo mayor.
# Esto se aprecia en el gráfico ya que es el vector más largo.
# Tambien, puede observarse que aparece LONANCHO que no estaba representada anteriormente.
# PECLIMBO y LONANCHO apuntan en sentido opuesto, lo que indica una correlación 
# negativa (o inversa) entre ambas.

# Gráfico de scores para PC2-PC3
fviz_contrib(damaPca, choice = "var", axes = 2:3, top = 10)

# Circulo de correlaciones PC2-PC3
fviz_pca_var(damaPca, axes = c(2, 3), col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)
# A modo breve, podemos observar que PECLIMBO está muy bien representada y que 
# cuenta con una flecha larga y cos2 alto. Las demás variables, no están tan bien
# representadas como si lo están en PC1-PC2.

# Graficos e interpretaciones PC1 y PC3
# Biplot PC1-PC3
p13 <- autoplot(
  damaPca,
  x = 1,
  y = 3,
  data = dfDamas,
  colour = 'VAR',
  loadings = TRUE,
  loadings.label = TRUE,
  loadings.label.size = 3,
  scale = 0)

ggplotly(p13)

# Interpretaciones Biplot PC1-PC3
# Como se menciono anteriormente, PC1 es el eje de tamaño. Las que se encuentren
# del lado izquierdo son las que presentan un tamaño de fruto o flor más grande.
# Caso contrario, las variedades del lado derecho presentan caracteristicas opuestas.
# A su vez, aparecen los vectores PECLIMBO y LONGANCHO, que antes estaban ocultas
# en el análisis de PC1-PC2.
# Como variedad a destacar, la variedad MARTINET (segundo cuadrante), es la que
# presenta el mayor endocarpio de todas las variedades (PESOEND, LONGEND, ANCHOEND).

# Gráfico de scores para PC1-PC3
fviz_contrib(damaPca, choice = "var", axes = c(1, 3), top = 10)

# Circulo de correlaciones PC1-PC3
fviz_pca_var(damaPca, axes = c(1, 3), col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)
# A primera vista en el biplot se observaba un vector muy grande para PECLIMBO,
# sin embargo al ver el circulo de correlaciones podemos ver la calidad de representación,
# y en este caso son el PESOEND, LONGEND Y ANCHOEND las mejores representadas.

#--------------------------------------------------------------------------------
# 3. CONCLUSIONES
#--------------------------------------------------------------------------------
# Tras haber analizado el dataset de damascos, primero con un análisis exploratorio
# de datos (EDA) y luego con un análisis de componentes principales (PCA), fue posible
# visualizar otro tipo de información que no puede observarse a simple vista con
# medidas estadísticas (media, mediana, varianza).

# El EDA mostró que las variables estaban medidas en escalas y unidades muy distintas
# (g, mm, cm2 y cocientes adimensionales), con varianzas que iban de 0,001 (PECLIMBO)
# a 144 (PESOF). Por ese motivo el PCA se realizó sobre la matriz de correlaciones,
# evitando que las variables de mayor escala dominaran las componentes solo por su
# magnitud.
#
# El PCA permitió resumir las 13 variables originales en 3 componentes que, en
# conjunto, explican el 80,2 % de la variabilidad total. Esto confirma que gran parte
# de la información estaba redundante y podía comprimirse sin pérdidas relevantes:
#
#   - PC1 (54,8 %) es un EJE DE TAMAÑO: todas las variables de tamaño cargan con el
#     mismo signo, de modo que separa variedades grandes (en flor, fruto y endocarpio
#     a la vez) de variedades chicas.
#   - PC2 (15,9 %) es un EJE DE FORMA: contrasta el desarrollo de flor y hoja
#     (SUPHOJA, ANCHOPET, LONGPET) frente al desarrollo del fruto. A igual tamaño,
#     distingue variedades con mayor flor de las de mayor fruto.
#   - PC3 (9,4 %) está dominada casi exclusivamente por PECLIMBO, una variable 
#     que en PC1–PC2 quedaba oculta. La misma, junto con LONANCHO, son variaciones 
#     independientes del tamaño.

# A su vez, el biplot muestra la posición de variedades y variables, pero es recien
# al cruzarlo con el circulo de correlaciones que podemos observar la calidad de representación.
# De esta forma, detectamos que PECLIMBO está menos representada en PC1–PC2 y 
# mejor representada en PC2–PC3, lo que confirma la necesidad de sumar una tercer componente al análisis.

# En síntesis, el PCA cumplió el objetivo de caracterizar el conjunto de datos. 
# Se redujo de 13 variables a 3 componentes interpretables (tamaño, forma de flor y fruto y relación
# peciolo-limbo), explicando el 80,2 % de la variabilidad total y permitiendo 
# identificar las características más distintivas de cada variedad de damasco.