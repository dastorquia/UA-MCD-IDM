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
