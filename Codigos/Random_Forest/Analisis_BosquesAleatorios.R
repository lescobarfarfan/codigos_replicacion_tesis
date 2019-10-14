# ==============================================================================
# ==============================================================================
# ==============================================================================
# =============== Código para analizar las variables de los ajustes ============
# ==============================================================================
# ==============================================================================
# ==============================================================================


# Directorio de trabajo.
setwd('Z:/LuisEscobar/Clusters/PrinComp_K-Means/RandomForest/Junio2017/Datos_DobleEscala/')
dir()

# De igual forma, el código funciona para todos los objetos creados, sólo es
# necesario modificar el nombre de los objetos que se están leyendo.


# Librerias
library(ggplot2)
library(data.table)
library(lubridate)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(party)
library(readr)
library(e1071)
library(caret)
library(dplyr)


# Empezamos a leer los ajustes.
load('AjusteRKM_SinCol_Nivel1_GINI.RData')
AjusteRandomForest <- lista_todo
rm(lista_todo)


# Inspeccionamos el objeot
str(AjusteRandomForest)
AjusteRandomForest$datosAll
AjusteRandomForest_VarImp <- AjusteRandomForest$ImportanciaVariables
AjusteRandomForest_VarImp <- AjusteRandomForest_VarImp[, c(3, 2)]
length(unique(AjusteRandomForest_VarImp$Conceptos))
ruta_escritura <- 'Z:/LuisEscobar/Clusters/PrinComp_K-Means/RandomForest/Junio2017/PorGini/DobleEscala/'
write.csv(AjusteRandomForest_VarImp,
          paste0(ruta_escritura, 'RKM_SinCol_Nivel1_VarImp_GINI.csv'),
          row.names = F)
rm(list = ls())
    