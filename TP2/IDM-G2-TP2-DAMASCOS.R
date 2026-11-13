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
                    "corrplot", "dplyr", "ggplotly")

newPackages <- listofpackages[!(listofpackages %in% installed.packages()[, "Package"])]
if (length(newPackages)) install.packages(newPackages)

for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}

# -------------------------------------------------------------
# 1. Lectura de datos
# -------------------------------------------------------------
masterPath <- "~/MCD-IDM/UA-MCD-IDM/TP2/"

# Se usan las variedades como nombres de fila y se quita la columna VAR,
# para quedarnos solo con las 13 variables numéricas.
dfDamas <- read.table(paste0("damas.txt"),
                      sep = "\t",
                      dec = ",",
                      header = TRUE,
                      row.names = 1,
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
# centímetros cuadrados



# -------------------------------------------------------------
# 2. EDA - Análisis Exploratorio de Datos
# -------------------------------------------------------------
# Para las variables del data set, se analizan las métricas estadísticas

res <- pastecs::stat.desc(dfDamas) %>% as.matrix %>% as.data.frame %>% round(2)
res <- format(res, scientific = FALSE)
knitr::kable(data.frame(res), digits = 2)


# Podemos observar que no hay valores faltantes.
# Se puede observar que en su mayoría la media y la mediana se aproximan, lo que
# sugiere que las distribuciones son aproximadamente simetricas.
# Se observan valores de varianza con una gran dispersión (de 0 a 144), esto 
# se debe a que estamos analizando datos con distintas escalas y unidades.

# El coeficiente de variación nos permite comparar la variabilidad de los
# distintos atributos, aun cuando este medidos en distintas unidades o la
# magnitud que tome cada uno de ellos varie.
# Para este conjunto de datos podemos ver que son bastante homogeneos
# (CV entre 6 a 14%). Las excepciones son el peso del endocarpio 
# (PESOEND, CV = 42 %), el peso del fruto (PESOF, CV = 26 %) y la longitud del 
# endocarpio (LONGEND, CV 21 %), que son las que más diferencian a las variedades.

# -------------------------------------------------------------
# 3. Correlaciones
# -------------------------------------------------------------
# Se realiza una matriz de correlación, dónde en la diagonal se
# ven las variables con la distribución. Por debajo de la misma, los distintos
# gráficos de dispersión y por encima los coeficientes de correlación.

pairs.panels(dfDamas, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = F) # show correlation ellipses

# Los coeficiente de correlación más altos (0,87 - 0,99) se observan
# en las variables relacionadas al tamaño del fruto (PESOF, ANCHOF, LONGF, ESPESORF).
# Tiene sentido ya que un fruto más grande tiende a ser más pesado.
# Asimismo, hay un valor elevado de coeficiente de correlación entre el 
# la longitud y el ancho del pétalo (LONGPET y ANCHOPET), como también en
# la longitud del endocarpio y el ancho del endocarpio (LONGEND y ANCHOEND).

# Con respecto a los gráficos de dispersión que se presentan debajo de la
# diagonal del gráfico, hay algunas relaciones que no son lineales, por ejemplo
# PECLIMBO con PESOF, o como PESOEND con ANCHOPE.


# -------------------------------------------------------------
# 4. Boxplots
# -------------------------------------------------------------
# Se realizan los boxplots para ver descriptivamente las variables y detectar outliers. 
dfBox <- stack(dfDamas)
names(dfBox) <- c("valor", "variable")
ggplot(dfBox, aes(x = variable, y = valor)) +
  geom_boxplot()+  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Tal como se mencionó en el análisis exploratorio, es dificil comparar las variables,
# ya que poseen distintas dimensiones y unidades. 
# Sin embargo, lo que llama la atención, son las figuras de LONANCHO (relación longitud y ancho de hoja)
# y PECLIMBO (relación peciolo-limbo), que presentan un rango muy bajo lo que provoca que 
# el boxplot se vea más estrecho.

# A su vez, notamos que las variables SUPHOJA, PESOF, ANCHOF y ESPESORF presentan outliers. 

# -------------------------------------------------------------
# 5. PCA - Análisis de Componentes Principales
# -------------------------------------------------------------
# Dado que en el EDA se observaron varias relaciones lineales entre las variables
# con valores de coeficientes de correlación altos, se decide avanzar con el 
# PCA para reducir la dimensionalidad, capturando la mayor variabilidad posible
# del conjunto de datos. Queremos mencionar que somos  conscientes que existen 
# algunas relaciones no lineales y somos conscientes que ACP no logra captar esta 
# información al momento de reducir la variabilidad.

# COMPARAR CON DIAPOSITIVAS ANTES DE CONCLUIR
# El PCA puede hacerse sobre la matriz de varianzas y covarianzas o sobre la 
# matriz de correlaciones. En este análisis se usa la matriz de CORRELACIONES porque
# las variables están medidas en distintas unidades (g, mm, cm2 y cocientes
# adimensionales) y con escalas muy diferentes: las varianzas van de 0,001
# (PECLIMBO) a 144 (PESOF).
# Con la matriz de covarianzas, las variables de mayor varianza dominarían las
# componentes solo por su escala. 

X <- dfDamas 

damaPca <- prcomp(as.matrix(X),center = T, scale. = T) 

summary(damaPca)


# Para nuestro análisis, debemos elegir cuantas componentes retener. 
# A medida que se agreguen más componentes, la complejidad del análisis aumenta 
# ya que se deben empezar a considerar más combinaciones entre las componentes. 

# Se observa que la componente PC1 explica el 54,8 % de la variabilidad total y la PC2 un
# # 15,9 % extra, dando un acumulado de 70,7 %. Para nuestro caso, consideramos 
# que es suficiente porque llegamos al umbral de 70-80 % de variabilidad total. 

# A su vez, según el criterio de Kaiser (autovalores > 1), podríamos incluir hasta la PC4,
# cuyo autovalor es 1,01 y su variabilidad acumulada es 87,9 %. 
# Sin embargo decidimos quedarnos hasta la PC2/PC3 (a chequear) porque la variabilidad
# acumulada es de 80,1 %, la cual es suficiente. Además, sumar una componente más,
# agregaría un grado de complejidad extra al análisis.

# Por último, otro criterio para elegir la cantidad de componentes es visualizar los 
# valores de las componentes a elegir de manera
# descriptiva con un scree plot. Puede observarse una fuerte caida desde PC1 a PC2,
# y luego luego la curva se aplana a partir de PC3-PC4. Por lo considerar hasta
# PC2/PC3 (a chequar) sería suficiente para nuestro análisis.

plot(damaPca) # a este gráfico estaría bueno ponerle los valores sobre las barras

fviz_eig(damaPca, addlabels = TRUE, ylim = c(0, 60))

# Tabla con los autovalores
# eigenvalue: el autovalor de cada componente (7,13; 2,06; 1,23; 1,01...)
# variance.percent: el % de varianza que explica esa componente (54,8; 15,9; 9,4; 7,8...)
# cumulative.variance.percent: el % acumulado (54,8; 70,7; 80,2; 87,9...)
eig <- get_eigenvalue(damaPca)
knitr::kable(round(eig, 2))   # autovalor, % varianza, % acumulado

# Se descarta PC4 ya que apenas supera Kaiser (1,011), valor observado en la tabla (Dim.4 eigenvalue 1.01)
# Kaiser dice “retené autovalores > 1”, y 1,011 lo cumple por un margen mínimo.

# Para terminar de definir si usar PC3

var <- get_pca_var(damaPca)          # funciona con el objeto de prcomp

round(var$cos2[, 1:4], 2)            # (a) calidad de representación por componente
round(rowSums(var$cos2[, 1:2]), 2)   # (b) cos2 acumulado en PC1+PC2, dice qué proporción de la variabilidad de una variable queda capturada.
round(var$contrib[, 1:4], 1)         # (c) contribución (%) de cada variable a cada PC
round(damaPca$rotation[, 3:4], 2)    # (d) cargas de PC3 y PC4

# “PECLIMBO tiene cos² = 0,10 en PC1–PC2”. Es el resultado de (b).
# Sus correlaciones son −0,13 con PC1 y −0,28 con PC2, así que 0,13² + 0,28² ≈ 0,10.
# Quiere decir que con solo dos componentes se pierde el 90 % de lo que varía PECLIMBO.
# Se ve también en el gráfico de variables: su flecha es muy corta.

# “PC3 está dominada por PECLIMBO”. Sale de (c) y (d). 
# En PC3 la carga de PECLIMBO es 0,76, y contribuye 57,4 % de esa componente 
# (el resto está repartido: LONANCHO 13 %, PESOEND 10 %). 
# Su correlación con PC3 es 0,84, o sea cos² = 0,71.
# Agregar PC3 tiene sentido: PECLIMBO pasa de 0,10 a 0,81 de representación acumulada.
# Sin PC3, esa variable queda casi fuera del análisis.


# -------------------------------------------------------------
# 6. MATRIZ DE CARGAS
# -------------------------------------------------------------

# matriz de los vectores propios
knitr::kable(as.data.frame(unclass(damaPca$rotation)))

# INTERPRETACIÓN: Observando el vector propio de carga para la componente principal 1 se observa que hay
# un ordenamiento natural de las observaciones ya que la mayoría de las variables tienen asociado un signo
# negativo y, las que tienen signo positivo, la magnitud es muy pequeña. Estarán más a la izquierda del gráfico
# aquellas variedades que presenten valores grandes en las variables relacionadas con el tamaño tanto de la 
# flor como del fruto y del endocarpio.
# En cuanto a la CP2 tomará valores altos cuando las variedades tengan superficie de hoja grande y también valores
# altos en las variables relacionadas al tamaño de la flor y tomará valores grandes negativos con respecto a esa componente
# cuando los frutos sean grandes. 
# Resumiendo en el segundo cuadrante estarán las variaedades que presenten valores altos en 
# las variables relacionadas con el tamaño de la flor y presenten hojas grandes. En el tercer cuadrante estarán
# aquellas con valores altos del tamaño en el tamaño del fruto y en el endorcarpio.
# La variedad BLANCO se caracteriza por tener flores y hojas grandes
# La variedad TADEO tiene frutos grandes
# La variedad GABACHET tiene frutos y hojas pequenas.
# Las variedades GANDIA, GINESTA y PALABRAS se caracterizan por tener flores pequeñas.
# La variedad CANINO tiene hojas pequeñas y flores pequeñas.
# CHEQUEAR LAS INTERPRETACIONES. CORREGIR

# FALTA MENCIONAR EL DAMASCO DE JARDÍN QUE ESTÁ EN EL PRIMER CUADRANTE. 
knitr::kable(damaPca$x)

library(ggfortify)
library(plotly)

p <- autoplot(damaPca,
              data = damas, 
              colour = 'VAR', ### PUEDE NO TENER MUCHO SENTIDO COLOREAR POR VAR PORQUE HAY UNA OBS POR VAR 
              loadings = TRUE,
              loadings.label = TRUE,
              loadings.label.size = 3)

ggplotly(p)

##################### PCA con factoMineR ############ 
# NOTA: El resultado es el mismo (gráficos más lindos que prcomp) pero tener en cuenta que los resultados dan rotados 180|C
# PODEMOS PRESENTARLO EXPLICANDO ESTO O PRESENTAR LA SALIDA DE UNO. TENER EN CUENTA QUE LAS OBSERVACIONES CAMBIAN DE CUADRANTE AUNQUE
# EL LA RELACIÓN ENTRE LAS VARIABLES ES LA MISMA
#################################################

X <- dfDamas
damaPca <- FactoMineR::PCA(X = X, scale.unit = T, ncp = ncol(X), graph = T)

# Matriz de vectores propios

damaPca$svd$V

# ind <- get_pca_ind(damaPca)
# ind
# 
# p.1 <- fviz_pca_ind(damaPca, col.ind = "coord", 
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE # Avoid text overlapping (slow if many points)
# )



# Interpretación del gráfico variables - PCA

var <- get_pca_var(damaPca)

tabla_cos2 <- knitr::kable(var$cos2)

tabla_contrib <- knitr::kable(var$contrib)

tabla_coord <- knitr::kable(var$coord)

corrplot(var$contrib, is.corr=FALSE)  # Otra forma de visualizar la contribución



fviz_pca_var(damaPca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

# A partir del análisis del gráfico de variables - PCA y de la tabla tabla_contrib puede 
# observarse que las variables que describen las características de la flor,
# del fruto y del endocarpio tiene alto aporte a la componente CP1. Además observando el ángulo
# formado por los vectores que representan dichas variables se observa una alta correlación 
# entre los distintos grupos de variables mencionadas. En el segundo cuadrante del plano cartesiano se 
# encuentran altamente correlacionadas todas las variables que describen los atributos de la flor 
# (ANCHOF, PESO F, ESPESORF, LONANCHO y LONGF) y se observan dos grupos de variables muy correlacionadas
# uno que describen las características del endocarpio (PESOEND, ANCHOEND, LONGEND) y otro las de los pétalos
# (LONGPET y ANCHOPET). 
# Analizando la PC2 puede observarse que el mayor aporte a dicha componente está dado por las variables 
# LONGPET, ANCHOPET Y SUPHOJA. 
# cuadrante aquellas relacionadas con las características de los pétalos. Mientas que puede observarse un tercer grupo de
# variables altamente correlacionadas que son aquellas que se observa que 
# están altamente correlaciondas (los ángulos formados por los respetivos vectores son pequeños) y tienen 
# una alta contribución a la dirección de la PC1 ya que el cos2 es alto. Por otra parte, las variables  
# PESOEND, LONGEND Y ANCHOEND también se están altamente correlacionadas tienen una alta 
# VER SI AGREGAR PC3 QUE TIENE DONDE PECLIMBO TIENE MUCHA CONTRIBUCIÓN.

