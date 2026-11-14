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
# TAMFLOR: Tamaño de la flor | Unidad: [mm]
# LONGPET: Longitud del pétalo | Unidad: [mm]
# ANCHOPET: Ancho del pétalo | Unidad: [mm]
# SUPHOJA: Superficie de hoja | Unidad: [cm^2]
# LONANCHO: Relación longitud y ancho de hoja | Unidad: [Adimensional]
# PECLIMBO: Relación peciolo-limbo | Unidad: [Adimensional]
# PESOF: Peso del fruto | Unidad: [g]
# LONGF: Longitud del fruto | Unidad: [mm]
# ANCHOF: Ancho del fruto | Unidad: [mm]
# ESPESORF: Espesor del fruto | Unidad: [mm]
# PESOEND: Peso del endocarpio | Unidad: [g]
# LONGEND: Longitud del endocarpio | Unidad: [mm]
# ANCHOEND: Ancho del endocarpio | Unidad: [mm]

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

# Se observan valores de varianza con una gran dispersión (de 0 a 144), esto 
# se debe a que estamos analizando datos con distintas escalas y unidades.

# El coeficiente de variación nos permite comparar la variabilidad de los
# distintos atributos, aun cuando esten medidos en distintas unidades o la
# magnitud que tomen cada uno de ellos varie.
# Para este conjunto de datos podemos ver que son bastante homogeneos
# (CV entre 6 a 14%). Las excepciones son el peso del endocarpio 
# (PESOEND, CV = 42 %), el peso del fruto (PESOF, CV = 26 %) y la longitud del 
# endocarpio (LONGEND, CV 21 %), que son las que más diferencian a las variedades.

# -------------------------------------------------------------
# Boxplots
# -------------------------------------------------------------
# Se realizan los boxplots para ver descriptivamente las variables y detectar outliers. 
dfBox <- stack(dfDamas[,-1])
names(dfBox) <- c("valor", "variable")
ggplot(dfBox, aes(x = variable, y = valor)) +
  geom_boxplot()+  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Tal como se mencionó en el análisis exploratorio, es dificil comparar las variables,
# ya que poseen distintas dimensiones y unidades. 
# Sin embargo, lo que llama la atención, son las figuras de LONANCHO (relación longitud y ancho de hoja)
# y PECLIMBO (relación peciolo-limbo), que presentan un rango muy bajo lo que provoca que 
# el boxplot se vea más estrecho.

# A su vez, notamos que las variables SUPHOJA, PESOF, ANCHOF y ESPESORF presentan outliers. 

# Vamos a utilizar el conjunto completo del dataset, incluyendo los outliers, y
# a su vez, no hizo falta descartar ningún registro por ausencia de datos faltantes.

# -------------------------------------------------------------
# Correlaciones
# -------------------------------------------------------------
# Se realiza una matriz de correlación, dónde en la diagonal se
# ven las variables con su histograma y correspondiente 
# curva de densidad. Por debajo de la misma, los distintos
# gráficos de dispersión y por encima los coeficientes de correlación.

pairs.panels(dfDamas[,-1], 
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
# 2. PCA - Análisis de Componentes Principales
# -------------------------------------------------------------
# Dado que en el EDA se observaron varias relaciones lineales entre las variables
# con valores de coeficientes de correlación altos, se decide avanzar con el 
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
# componentes solo por su escala. 

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

# Criterio 1: Seleccionar los autovalores cuyo porsentaje de variabilidad explicada 
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

# -------------------------------------------------------------
# Scree plot - Gráfico de sedimentación
# -------------------------------------------------------------
fviz_eig(damaPca, addlabels = TRUE, ylim = c(0, 60))

# Criterio 3: Scree plot 
# Por último, otro criterio para elegir la cantidad de componentes es visualizar los 
# valores de las componentes a elegir de manera
# descriptiva con un scree plot. Puede observarse una fuerte caida desde PC1 a PC2,
# y luego la curva se aplana a partir de PC3-PC4.

#------------------------------------------------------------------------
# ¿Cuántas componentes retener?
#-----------------------------------------------------------------------

# A partir del análisis de los 3 criterios mencionados puede pensarse que es 
# recomendable retener las primeras 3 componentes principales.

var <- get_pca_var(damaPca)          # funciona con el objeto de prcomp

round(var$cos2[, 1:4], 2)            # (a) calidad de representación por componente
round(rowSums(var$cos2[, 1:2]), 2)   # (b) cos2 acumulado en PC1+PC2, dice qué proporción de la variabilidad de una variable queda capturada.
round(var$contrib[, 1:4], 1)         # (c) contribución (%) de cada variable a cada PC
round(damaPca$rotation[, 3:4], 2)    # (d) cargas de PC3 y PC4

# PECLIMBO tiene cos² = 0,10 en PC1–PC2. Es el resultado de (b).
# Quiere decir que con solo dos componentes se pierde el 90 % de lo que varía PECLIMBO.

# PC3 está mayormente conformada por PECLIMBO. Es el resultado de (c) y (d). 
# En PC3 la carga de PECLIMBO es 0,76, y contribuye 57,4 % de esa componente. 
# Al agregar PC3, pasamos a capturar el 80,15% de la variabilidad de las componentes.

####################################################################################################################
# NOTA: SI AGREGAMOS LA TERCERA COMPONENTE, TENEMOS QUE HACER LAS INTERPRETACIONES DEL BIPLOT PARA CP1-CP3 Y CP2-CP3+
# SI NO LA INCLUIMOS PODEMOS JUSTIFICAR QUE NO LO HACEMOS PORQUE SOLO AGREGA UN 10% A LA EXPLICACIÓN DE LA VARIABILIDAD
# QUE PARA UN CONJUNTO DE DATOS CON ESTAS CARACTERÍSTICAS, NO LO CONSIDERAMOS NECESARIO
####################################################################################################################



# -------------------------------------------------------------
# 6. MATRIZ DE CARGAS
# -------------------------------------------------------------
# Matriz de los vectores propios

knitr::kable(as.data.frame(unclass(damaPca$rotation)))

# INTERPRETACIÓN: Observando el vector propio de carga para la componente principal 1 se observa que hay
# un ordenamiento natural de las observaciones ya que la mayoría de las variables tienen asociado un signo
# negativo y, las que tienen signo positivo, la magnitud es muy pequeña. Estarán más a la izquierda del gráfico
# aquellas variedades que presenten valores grandes en las variables relacionadas con el tamaño tanto de la 
# flor como del fruto y del endocarpio.
# En cuanto a la CP2 tomará valores altos positivos cuando las variedades tengan superficie de hoja grande y también valores
# altos en las variables relacionadas al tamaño de la flor y tomará valores grandes negativos con respecto a esa componente
# cuando los frutos sean grandes. 
# Resumiendo en el segundo cuadrante estarán las variaedades que presenten valores altos en 
# las variables relacionadas con el tamaño de la flor y presenten hojas grandes. En el tercer cuadrante estarán
# aquellas con valores altos en el tamaño del fruto y en el endorcarpio.
# La variedad BLANCO se caracteriza por tener flores y hojas grandes
# La variedad TADEO tiene frutos grandes
# La variedad GABACHET tiene frutos y hojas pequeñas.
# Las variedades GANDIA, GINESTA y PALABRAS se caracterizan por tener flores pequeñas.
# La variedad CANINO tiene hojas pequeñas y flores pequeñas.



# La primera componente principal está representada por:
# PC1 = -0.24391480*TAMFLOR   - 0.28037433*LONGPET - 0.26695115*ANCHOPET + 0.03766820*SUPHOJA 
#     -0.2026726*LONANCHO + 0.04781008*PECLIMBO - 0.32258043*PESOF - 0.33558338*LONGF + 0.32154381*ANCHOF -
#      -0.31241703*ESPESORF - 0.32701658*PESOEND - 0.33506257*LONGEND - 0.33011523*ANCHOEND

# Para la variedad BLANCO la coordenada de la primera componente es:

y <-  (-0.24391480*1.37295522   - 0.28037433*0.9433209 - 0.26695115 * 1.57464408 + 0.03766820*0.54920637   
       -0.2026726*0.55099718 + 0.04781008*0.46377296 - 0.32258043*0.21244507 - 0.33558338*0.72594876  
       -0.32154381*0.25055593 - 0.31241703*0.34705401 - 0.32701658*1.8432897 - 0.33506257*1.9402104  
       -0.33011523*1.8006967)

# o, lo que es equivalente

sum(damaPca$rotation[,1] * std_X[1,])

# Esto puede verificarse citando a x del objeto damaPca

damaPca$x

# donde a primera fila de damaPca$x es la primera variedad (BLANCO) valorizada en las distintas componentes de
# damaPca$rotation

 
knitr::kable(damaPca$x)


p <- autoplot(
  damaPca,
  data = dfDamas,
  colour = 'VAR',
  loadings = TRUE,
  loadings.label = TRUE,
  loadings.label.size = 3,
  scale = 0
)

ggplotly(p)


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
# observarse que las variables que describen las características del fruto y del endocarpio tiene alto aporte a la componente CP1
# Mientras que las variables que describen las características de la flor tienen un aporte moderado a dicha componente. 
# Además, observando el ángulo formado por los vectores que representan dichas variables se observa una alta correlación 
# entre las variables que miden cada una de las características antes mencionadas. Por ejemplo, en el segundo cuadrante 
# del plano cartesiano se observa que las variables que caracterizan el tamaño de la flor (TAMFLOR, LONGPET y ANCHOPET) están altamente correlacionadas 
# entre si como también lo están las variables que describen las características del endocarpio (PESOEND, ANCHOEND, LONGEND). Por otro lado, en el tercer
# cuadrante se observa que están altamente correlacionadas todas las variables que describen las características del
# fruto (ANCHOF, PESO F, ESPESORF, LONANCHO y LONGF) .
# Analizando la PC2 puede observarse que el mayor aporte a dicha componente está dado por la variable SUPHOJA seguida por las variables
# ANOHOPET, LONGPET y en menor medida tamaño de la flor. Es por ello que puede pensarse que la PC2 tomará valores altos cuando
# las variedades tengan flores y hojas grandes. Además, dado que el aporte de las variables relacionadas el fruto es moderado y, observando que los
# pesos toman valores negativos, se esperaría que las variedades que están ubicadas en los valores más extremos positivos de la PC2, tengan frutos pequeños


#--------------------------------------------------------------------------------
# Correlaciones entre cada variable y cada componente
#--------------------------------------------------------------------------------

# La correlación entre cada variable estandarizada y las componentes principales permite evaluar qué tan
# bien representada está cada variable por cada CP

X.est <- scale(dfDamas[-1])
cor(X.est, damaPca$x)

# La interpretación de las correlaciones es análoga a la de la matriz de cargas presentada anteriormente