library(dplyr)
library(factoextra)
library(ggdendro)
library(cowplot)
library(tidyr)
library(knitr)
library(cluster)
library(janitor)

##################################
# LECTURA Y PREPARACIÓN DE DATOS
##################################

#Leer datos
pinguinos <- read.csv("pinguinos.csv", sep = ";")
pinguinos <- select(pinguinos, -species, - island, -sex)

# Estandarizar
pinguinos_std <- scale(pinguinos)

summary(pinguinos_std)

# 1) Aplicar un método de agrupamiento jerárquico considerando sólo las características 
# numéricas. 

# Matriz de distancias euclídeas
d <- dist(pinguinos_std, method = "euclidean")

# Agrupamiento jerárquico
hc <- hclust(d, method = "ward.D2")

# Graficar el dendograma
plot(hc)

# oculatando etiquetas
plot(hc, labels = FALSE, hang = -1,
     main = "Clustering jerárquico - Pinguinos",
     xlab = "", sub = "")

#gráfico coloreando los clusters

fviz_dend(
  hc,
  k = 2,
  rect = TRUE,
  rect_fill = FALSE,
  show_labels = FALSE,
  cex = 0.7,
  color_labels_by_k = TRUE,
  main = "Clustering jerárquico - Pinguinos")

grupos_hc <- cutree(hc, k = 2)

# Incorporar al dataframe original
pinguinos$cluster_hc <- factor(grupos_hc)

# Medias de las variables por cluster
vars_to_cluster <- names(pinguinos_std)

tabla_resumen <- pinguinos %>%
  group_by(cluster_hc) %>%
  summarise(
    n = n(),
    across(all_of(vars_to_cluster), mean),
    .groups = "drop"
  )

kable(tabla_resumen, digits = 2)

# 2) ¿Qué características tienen los grupos formados? 

# 3) ¿Qué relación hay entre los clusters obtenidos y las especies? 
  
pinguinos_completo <- read.csv("pinguinos.csv", sep = ";")
pinguinos_completo$cluster_hc <- factor(grupos_hc)

tabyl(pinguinos_completo, species, cluster_hc)

# 4) ¿Cómo se distribuyen las variables categóricas ‘island’, y ’sex’ en los clusters 
# encontrados? ¿Qué se puede interpretar? 

tabyl(pinguinos_completo, island, cluster_hc)
tabyl(pinguinos_completo, sex, cluster_hc)




