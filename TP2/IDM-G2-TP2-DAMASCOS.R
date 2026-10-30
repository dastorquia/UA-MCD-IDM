# IDM
# TP 2 - DAMASCOS

damas <- read.delim2("~/MCD-IDM/UA-MCD-IDM/TP2/damas.txt")
View(damas)

knitr::opts_chunk$set(echo = TRUE)

masterPath <- "~/MCD-IDM/UA-MCD-IDM/TP2/"

listofpackages <- c( "psych",     "ggplot2",    "knitr",
                     "pastecs",   "FactoMineR", "grid",
                     "gridExtra", "ggfortify",  "factoextra",
                     "corrplot",  "dplyr")
newPackages <- listofpackages[ !(listofpackages %in% installed.packages()[, "Package"])]
if(length(newPackages)) install.packages(newPackages)
for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}


# -----------------
# Análisis de los datos


dfEpf <- read.table(paste0(masterPath,"damas.txt"),
                    sep="\t",
                    dec=",",
                    fileEncoding = "utf8",
                    header = T)

knitr::kable(head(dfEpf)) # head es para traer los principales

# 18 variedades distintas de damasco
# 13 columnas con variables

dfEpf<-dfEpf[,-1] #quitA la columna de VAR para quedarme solo con columnas numéricas 

# -----------------
# EDA

res<-pastecs::stat.desc(dfEpf) %>% as.matrix %>% as.data.frame %>% round(2)
str(res)

res = format(res, scientific = FALSE)

knitr::kable(data.frame(res),digits = 2)

#coef.var es el coeficiente de variación. Es el devío estandar sobre la media



#-----
# correlaciones

pairs.panels(dfEpf, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)


#-----
# boxplot

dfInd<-stack(dfEpf)
names(dfInd)<-c("valor","variable")
ggplot(dfInd, aes(x = variable, y = valor)) +
  geom_boxplot()+  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#----
# pca 

X <- dfEpf 

mediPca2 <- prcomp(as.matrix(X),center = T, scale. = T) # al usar la T en center y scale estamos usando la matriz R
# la matriz R es la matriz de correlación y es la que tenemos que usar. Evitar usar en esta materia la de 
# variancia y covariancia

summary(mediPca2)

plot(mediPca2)
class(mediPca2)

# usamos las primeras 3

#----
# matriz de cargas

# matriz de los vectores propios
knitr::kable(as.data.frame(unclass(mediPca2$rotation)))

# son las observaciones valorizadas 
knitr::kable(mediPca2$x)

autoplot(mediPca2, 
         data = damas, 
         colour = 'VAR', 
         loadings = TRUE, 
         loadings.label = TRUE, 
         loadings.label.size = 3)


#---
library(ggfortify)
library(plotly)

p <- autoplot(mediPca2,
              data = damas,
              colour = 'VAR',
              loadings = TRUE,
              loadings.label = TRUE,
              loadings.label.size = 3)

ggplotly(p)

# --------

asd <- dfMundo[,2:ncol(dfMundo)]
mundoPca <- FactoMineR::PCA(X = asd, scale.unit = T,ncp = ncol(X), graph = T)
