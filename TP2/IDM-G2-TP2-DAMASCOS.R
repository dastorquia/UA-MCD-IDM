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
                    "corrplot", "dplyr")

newPackages <- listofpackages[!(listofpackages %in% installed.packages()[, "Package"])]
if (length(newPackages)) install.packages(newPackages)

for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}

# -------------------------------------------------------------
# 1. Lectura de datos
# -------------------------------------------------------------
masterPath <- "~/MCD-IDM/UA-MCD-IDM/TP2/"

dfDamas <- read.table(paste0(masterPath, "damas.txt"),
                      sep = "\t",
                      dec = ",",
                      header = TRUE,
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
# Todas los registros de peso están expresados en gramos, los de longitud en milímetros y superficie en
# centímetros cuadrados


dfDamas<-dfDamas[,-1] 

# Se usan las variedades como nombres de fila y se quita la columna VAR,
# para quedarnos solo con las 13 variables numéricas.

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
# El PCA puede hacerse sobre la matriz de varianzas y covarianzas (datos solo
# centrados) o sobre la matriz de correlaciones (datos centrados y
# estandarizados). En este análisis se usa la matriz de CORRELACIONES porque
# las variables están medidas en distintas unidades (g, mm, cm2 y cocientes
# adimensionales) y con escalas muy diferentes: las varianzas van de 0,001
# (PECLIMBO) a 144 (PESOF).
# Con la matriz de covarianzas, las variables de mayor varianza dominarían las
# componentes solo por su escala: en estos datos la primera componente explicaría
# el 78 % de la variabilidad y estaría formada casi exclusivamente por PESOF,
# dejando sin peso a las variables de flor y hoja. Al estandarizar, cada
# variable aporta varianza 1 y todas participan en igualdad de condiciones;
# lo que define las componentes pasa a ser la correlación entre variables.

X <- dfDamas 

damaPca <- prcomp(as.matrix(X),center = T, scale. = T) 

summary(damaPca)

# Para nuestro análisis, debemos elegir cuantas componentes retener. 
# A medida que se agreguen más componentes, la complejidad del análisis aumenta 
# ya que se deben empezar a considerar más combinaciones entre las componentes. 

# Se observa que la componente PC1 acumula el 54,8 % de la variabilidad total y la PC2 un
# # 15,9 % extra, dando un acumulado de 70,7 %. Para nuestro caso, consideramos 
# que es suficiente porque llegamos al umbral de 70-80 % de variabilidad total. 

# A su vez, según el criterio de Kaiser (autovalores > 1), podríamos incluir hasta la PC4,
# cuyo autovalor es 1,01 y su variabilidad acumulada es 87,9 %. 
# Sin embargo decidimos quedarnos hasta la PC2/PC3 (a chequear) porque la variabilidad
# acumulada es de 80,1 %, la cual es suficiente. Además, sumar una componente más,
# agregaría un grado de complejidad extra al análisis.

# Por último, podemos visualizar los valores de las componentes a elegir de manera
# descriptiva con un scree plot. Puede observarse una fuerte caida desde PC1 a PC2,
# y luego luego la curva se aplana a partir de PC3-PC4. Por lo considerar hasta
# PC2/PC3 (a chequar) sería suficiente para nuestro análisis.

plot(damaPca) # a este gráfico estaría bueno ponerle los valores sobre las barras

# Terminar de definir si usamos 2 o 3 componentes.

# -------------------------------------------------------------
# 6. MATRIZ DE CARGAS
# -------------------------------------------------------------

# matriz de los vectores propios
knitr::kable(as.data.frame(unclass(damaPca$rotation)))

# son las observaciones valorizadas 
knitr::kable(damaPca$x)

library(ggfortify)
library(plotly)

p <- autoplot(damaPca,
              data = dfDamas, ### ACA HAY UN ERROR Y NO LEE LA DATA.
              colour = 'VAR',
              loadings = TRUE,
              loadings.label = TRUE,
              loadings.label.size = 3)

ggplotly(p)

# --------
# Prueba
