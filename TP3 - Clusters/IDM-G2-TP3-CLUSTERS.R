library(dplyr)
library(factoextra)
library(ggdendro)
library(cowplot)
library(tidyr)
library(knitr)

##################################
#1.LECTURA Y PREPARACIÓN DE DATOS
##################################

# Leer datos
df <- read.csv("paises.csv", sep=",")

# Variables a considerar (solo numericas, eliminas la col países)
vars_to_cluster<- names(df)[-1]

# Seleccionar variables 
df_r <- df %>%
  select(all_of(vars_to_cluster))

# Estandarizar
df_std <- scale(df_r)

summary(df_std)

# 1) Antes de realizar los agrupamientos, evaluar la necesidad de 
# estandarizar las variables y justificar la decisión adoptada.

# Estandarizamos los datos porque las unidades son distintas (dolares, 
# porcentajes, etc) y el otro motivo es que las magnitudes de una misma unidad 
# tome valores muy distintos


# 2) Aplicar un método de agrupamiento jerárquico.
#--------------------
library(readxl)
nacimientos <- read_excel("nacimientos.xls") 
str(nacimientos)

library(janitor)

tabyl (nacimientos, corti)

tabyl (nacimientos, centro)
tabyl (nacimientos, eg_menor_30)

# Instalar y cargar los paquetes necesarios
install.packages("gtsummary")
install.packages("dplyr") # Para seleccionar variables si es necesario
library(gtsummary)
library(dplyr)

datos <- nacimientos

# Supongamos que tu dataset se llama "datos"
# 1. Asegúrate de que la variable 'centro' sea un Factor para que se describa bien
datos$centro <- as.factor(datos$centro)

# 2. Generar la tabla descriptiva básica de todo el dataset
tabla1 <- datos %>%
  # Excluimos el 'id' si no aporta información estadística (opcional)
  select(-id) %>% 
  tbl_summary(
    by = NULL, # Puedes poner by = centro si quieres ver la descripción dividida por centro
    statistic = list(
      all_categorical() ~ "{n} ({p}%)" # Muestra frecuencias y porcentajes
    ),
    label = list(
      centro ~ "Centro del parto",
      eg_menor_30 ~ "Edad Gestacional < 30 semanas",
      ematerna_mayor_40 ~ "Edad Materna > 40 años",
      hta ~ "Hipertensión Arterial",
      sexo_masculino ~ "Sexo del RN Masculino",
      corti ~ "Aplicación de Corticoides",
      cesarea ~ "Parto por Cesárea",
      peso_menor_1000 ~ "Peso del RN < 1000g"
    )
  ) %>%
  bold_labels() # Pone los nombres de las variables en negrita

# Mostrar la tabla en la consola/visor de RStudio
tabla1

# ---- 2) Agrupamiento jerárquico con disimilitud de Jaccard ----

# 1. Me quedo solo con las variables binarias
datos_bin <- nacimientos %>% select(-id, -centro)

# 2. Calculo la matriz de disimilitud de Jaccard ("binary" en R)
d_jaccard <- dist(datos_bin, method = "binary")

# 3. Aplico el agrupamiento jerárquico con el método de Ward
hc_jaccard <- hclust(d_jaccard, method = "ward.D2")

# 4. Grafico el dendrograma marcando 3 clusters
fviz_dend(hc_jaccard, k = 3, rect = TRUE, cex = 0.5,
          main = "Dendrograma - Jaccard + Ward")
# El dendrograma muestra 3 grupos bien diferenciados: el mayor salto de altura
# está entre 3 y 2 clusters, por eso se corta en k = 3.

# 5. Asigno a cada nacimiento su cluster
nacimientos <- nacimientos %>%
  mutate(cluster_jaccard = cutree(hc_jaccard, k = 3))

# 6. Cantidad de nacimientos por cluster
nacimientos %>% count(cluster_jaccard) %>% kable()
# Se obtienen 3 grupos de tamaño similar (aprox. 14, 18 y 19 nacimientos).

# | cluster_jaccard|  n|
# |---------------:|--:|
# |               1| 19|
# |               2| 14|
# |               3| 18|
