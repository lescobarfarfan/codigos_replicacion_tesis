# ==============================================================================
# ==============================================================================
# ==============================================================================
# ============= Bosques Aleatorios para Importancia de Variables ===============
# ============= utilizadas en la clusterizaci?n (Business Models)===============
# ==============================================================================
# ==============================================================================
# ==============================================================================

# ==============================================================================
# =============================== Marzo 2017 ===================================
# ==============================================================================


# Cambiamos la visualizaci?n de numeros grandes para evitar notaci?n cient?fica
options(scipen = 999)



# Bibliotecas -------------------------------------------------------------

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
library(readxl)
library(stringr)
library(magrittr)
library(tidyr)
library(randomcoloR)
library(stringi)
library(VSURF)
library(gghighlight)

# Directorios de trabajo --------------------------------------------------

# Ruta donde se escribiran los resultados
rutaPrin <- "Escriba la ruta donde se escribiran los resultados: \n" %>%
  readline()
rutaPrin %>% setwd()

# Ruta donde se encuentran los datos para los bosques aleatorios.
rutaAj <- "Escriba la ruta donde se encuentran los ajustes: \n" %>%
  readline()

# Ruta al catalogo con los nombres de las claves del catalogo minimo
rutaCat <- "Escriba la ruta al catalogo con nombres de las cuentas de balance: \n" %>%
  readline()

# Hacer el ajuste en paralelo o no?
paralelo <- "Desea hacer los ajustes en paralelo? (T o F):\n" %>%
    readline()

# Desea graficar la importancia de las variables?
graficar <- "Desea obtener graficas de la importancia para cada fecha?: (T o F)\n" %>%
    readline()

# Creamos una carpeta con el nombre general de los ajustes en la carpeta
# que contendra la salida del presente codigo
name_specific_adjust <- str_split(string = rutaAj,
                                  pattern = "/",
                                  simplify = T)
name_specific_adjust <- name_specific_adjust[, ncol(name_specific_adjust) - 1]
dir.create(path = name_specific_adjust)

# # Nivel de catalogo
# nivelCat <- "A que nivel del catalogo pertenecen los ajustes? \n" %>%
#   readline()

# COn o sin colineales?
# consinCol <- "Los ajustes son con o sin variables colineales? (SinCol o ConCol) \n" %>%
#   readline()

# Lectura de ajustes --------------------------------------------------------

# Obtenemos la lista de los archivos que contienen los ajustes.
files <- rutaAj %>%
  list.files(pattern = "RF.RData$")


# Definimos un vector de fechas, dado que no hay cambios
fechas <- as.Date(c("31/03/2007", "30/06/2007", "30/09/2007", "31/12/2007",
                    "31/03/2008", "30/06/2008", "30/09/2008" ,"31/12/2008",
                    "31/03/2009", "30/06/2009", "30/09/2009", "31/12/2009",
                    "31/03/2010", "30/06/2010", "30/09/2010", "31/12/2010",
                    "31/03/2011", "30/06/2011", "30/09/2011", "31/12/2011",
                    "31/03/2012", "30/06/2012", "30/09/2012", "31/12/2012",
                    "31/03/2013", "30/06/2013", "30/09/2013", "31/12/2013",
                    "31/03/2014", "30/06/2014", "30/09/2014", "31/12/2014",
                    "31/03/2015", "30/06/2015", "30/09/2015", "31/12/2015",
                    "31/03/2016", "30/06/2016", "30/09/2016", "31/12/2016",
                    "31/03/2017", "30/06/2017"), format = "%d/%m/%Y")


# Leemos el catalogo con los nombres de las cuentas del catalogo minimo
catalogo <- rutaCat %>%
  read_csv(col_names = T)
catalogo$Clave <- catalogo$Clave %>% as.character()
catalogo <- catalogo %>% mutate(Nombre = gsub(pattern = " ",
                                              replacement = ".",
                                              x = Nombre))


# Procesaremos uno a uno los archivos que se encuetran en el vector que
# obtuvimos en el paso anterior.

# Creamos data farmes donde almacenaremos los datos que salgan


for (i in 1:length(files)) {

  # Cargamos el ajuste
  load(paste0(rutaAj, files[i]))

  # Nombremos por fecha cada entrada de la lista
  names(Datos_RF) <- fechas

  # Creamos una ruta para la salida del ajuste
  name_specific_adjust_2 <- str_split(string = paste0(rutaAj, files[i]),
                                      pattern = "/", simplify = T)

  name_specific_adjust_2 <- name_specific_adjust_2[, ncol(name_specific_adjust_2)]

  dir.create(path = paste0(rutaPrin, name_specific_adjust,
                           "/", name_specific_adjust_2))

  # Creamos los data frame donde almacenaremos la salida para cada fecha del
  # ajuste
  importanciaTemporal <- data.frame() %>% tbl_df()
  # errRate <- data.frame() %>% tbl_df()



  # Ahora, recorreremos una a una las entradas de la lista, pues cada una
  # corresponde a una fecha de ajuste.
  for (j in 1:length(Datos_RF)) {

    # Extraemos la fecha
    nombre <- names(Datos_RF)[j]

    # Extraemos los datos de la j-esima fecha
    datostemp <- Datos_RF[[j]] %>% tbl_df()

    if (nrow(datostemp) == 0) {
      next(j)
    }


    # Agregamos la fecha para evitar posibles conflictos con las funciones
    # spread o gather que usaremos a continuacion.
    datostemp$Fecha <- nombre

    # Obtenemos los nombres de las cuentas
    datostemp <- datostemp %>%
      gather(key = Cuenta, value = Valor, -Institucion, -Cluster, -Fecha) %>%
      mutate(Clave = gsub(pattern = "X",replacement = "", x = Cuenta)) %>%
      left_join(catalogo) %>%
      select(-Cuenta) %>%
      mutate(Nombre = paste0(Clave,"-",Nombre)) %>%
      select(-Clave) %>%
      spread(key = Nombre, value = Valor)

    rownames(datostemp) <- datostemp$Institucion

    datostemp <- datostemp %>% select(-Institucion, -Fecha)

    target <- datostemp$Cluster
    datostemp <- datostemp %>% select(-Cluster)

    names(datostemp) <- make.names(names(datostemp))

    datostemp <- datostemp %>% as.data.frame()

    noPred <- ncol(datostemp)

    # Una vez que tenemos el directorio donde guardaremos la salida, hacemos
    # los ajustes de los random forest para cada entrada de la lista.
    # ajusteRF <- randomForest(formula = factor(Cluster) ~ .,  data = datostemp,
    #                          ntree = 1000, mtry = sqrt(noPred), importance = T)


    # Usamos el paquete VSURF para hacer multiples ajustes y evitar problemas
    # que pueden surgir de usar numeros aleatorios en una sola iteracion

    if (paralelo) {
        ajusteRF <- VSURF(x = datostemp, y = factor(target),
                          mtry = sqrt(noPred), parallel = T, ncores = 2,
                          clusterType = "FORK")
    } else {
        ajusteRF <- VSURF(x = datostemp, y = factor(target),
                          mtry = sqrt(noPred))
    }


    # Obtenemos las variables en el orden de importancia que nos arroja el
    # ajuste anterior
    variables <- names(datostemp)[ajusteRF$varselect.thres]

    # Extraemos la importancia y formamos un data frame
    importancia <- data.frame(Variable = variables,
                              Importancia = ajusteRF$imp.varselect.thres) %>%
      tbl_df()

    importancia$Fecha <- as.Date(nombre)

    importancia <- importancia %>% select(Fecha, Variable, Importancia)

    # Verificamos si se desea graficar
    if (graficar) {
        # Graficamos con ggplot
        ImportancePlot <- importancia %>%
            select(Variable, Importancia) %>%
            arrange(desc(Importancia)) %>%
            slice(1:20) %>%
            ggplot(aes(x = reorder(Variable, Importancia), y = Importancia)) +
            geom_col(fill = randomColor(luminosity = "bright"), colour = "black") +
            theme_bw() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text = element_text(colour = "black", size = 12),
                  plot.title = element_text(size = 14, colour = "black")) +
            coord_flip() +
            ggtitle(label = "Importancia de las variables de acuerdo a la disminucion media en la precisión",
                    subtitle = nombre) +
            NULL

        ggsave(filename = paste0(rutaPrin, name_specific_adjust,
                                 "/", name_specific_adjust_2, "/", "VarImpo_",
                                 nombre, ".tiff"),
               plot = ImportancePlot,
               device = "tiff", width = 35, height = 15,
               units = "cm", dpi = 300, limitsize = F)
    }

    # Creamos el rank de la importancia
    importancia <- importancia %>%
      arrange(desc(Importancia)) %>%
      mutate(Rank = row_number())

    # Pegamos la importancia al data frame que llevará
    importanciaTemporal <- list(importanciaTemporal, importancia) %>%
      bind_rows()

  }

  if (graficar) {
      # COn la informacion de la importancia de las variables, podemos hacer una
      # grafica historica de cómo se movio la importancia de las mismas en el
      # tiempo.
      ImportancePlot_hist <- importanciaTemporal %>%
          ggplot(aes(x = Fecha, y = Rank, group = factor(Variable))) +
          geom_smooth(aes(colour = factor(Variable)), se = F) +
          theme_bw() +
          theme(legend.position = "top",
                legend.title = element_blank()) +
          gghighlight(min(Rank) < 3) +
          ggtitle(label = "Evolucion de la importancia de las variables en el tiempo",
                  subtitle = "Rank")

      # Guardamos
      ggsave(filename = paste0(rutaPrin, name_specific_adjust,
                               "/", name_specific_adjust_2, "/", "VarImpo_Hist_",
                               nombre, ".tiff"),
             plot = ImportancePlot_hist,
             device = "tiff", width = 35, height = 15,
             units = "cm", dpi = 300, limitsize = F)
  }

  # Escribimos la salida.
  importanciaTemporal %>% write_csv(path = paste0(rutaPrin,
                                                  name_specific_adjust,
                                                  "/", name_specific_adjust_2,
                                                  "/", "EvoImpoHist.csv"))

}

# Libreamos memoria
rm(list = ls())
gc()
















# OLD VERSION -------------------------------------------------------------



# # Cargamos todos los ajustes
# # Leemos y acomodamos en una lista.
# lista_Ajustes <- list()
#
# # Dependiendo del nivel y de las variables contenidas es el nombre de los
# # objetos creados
# if (consinCol == "ConCol") {
#
#   # Recorremos los archivos
#   for (i in 1:length(files)) {
#
#     # Cargamos el ajuste.
#     load(paste0(rutaAj,files[i]))# %>% load()
#
#     # Creamos el nombre del objeto que se acaba de leer
#     name <- paste0("list_ajustes", nivelCat)
#
#     # guardamos en la lista
#     lista_Ajustes[[i]] <- eval(parse(text = name))
#
#   }
#
# } else if (consinCol == "SinCol") {
#
#   for (i in 1:length(files)) {
#
#     # Cargamos el ajuste
#     load(paste0(rutaAj, files[i]))
#
#     # Creamos nombres
#     name <- paste0("list_ajustes", nivelCat, "_SinCol")
#
#     lista_Ajustes[[i]] <- eval(parse(text = name))
#
#   }
#
# }
#
# # Damos nombres a cada miembro de la lista de acuerdo a su a?o
# anios <- seq(2007, 2017, 1)
# meses <- c("03-31", "06-30", "09-30", "12-31")
# names(lista_Ajustes) <- anios
#
# # Ahora, le agregamos nombres a las sublistas
# for (i in 1:length(lista_Ajustes)) {
#   for (j in 1:length(lista_Ajustes[[i]])) {
#     names(lista_Ajustes[[i]])[j] <- meses[j]
#   }
# }
#
#
# # Para facilitar el an?lisis, ya que cada objeto tiene nombre, desanidamos la
# # la lista.
# lista_Ajustes <- do.call(c, lista_Ajustes)
#
#
# # Procesamiento de los datos ----------------------------------------------
#
# # Para ajustar los bosques aleatorios, es necesario tener los datos en el
# # formato adecuado, preparamos los datos.
#
#
# # Para tener una idea de c?mo se mueven en el tiempo las variables importantes
# # creamos un data frame para ir siguiendo el movimiento de las mismas.
# importanciaTemporal <- data.frame() %>% tbl_df()
#
# # Tambien seguimos la calidad del ajuste
# errRate <- data.frame() %>% tbl_df()
#
# # recorreremos la lista
# for (i in 1:length(lista_Ajustes)) {
#
#   cat(paste0("Ajuste ", i, " de ", length(lista_Ajustes), "\n"))
#
#   ajtemp <- lista_Ajustes[[i]]
#
#   # Si el ajuste es nulo pasamos a la siguiente entrada de la lista
#   if(is.null(ajtemp)) {
#     next(i)
#   }
#
#   nombre <- names(lista_Ajustes)[i]
#   nombre <- gsub(pattern = "[.]", replacement = "-", x = nombre)
#
#   # Creamos el data frame con los datos necesarios para el ajuste de RF
#   datostemp <- ajtemp$Salida_obj$odata %>% tbl_df()
#   # Agregamos nombre de instituciones
#   datostemp$Institucion <- row.names(datostemp)
#   datostemp <- datostemp %>% select(Institucion, 2:(ncol(datostemp) - 1))
#
#   # Hacemos gather para cruzar con los nombres
#   datostemp <- datostemp %>%
#     gather(key = Clave, value = Valor, 2:ncol(datostemp))
#   # Quitamos la X de los nombres de las cuentas
#   datostemp$Clave <- gsub(pattern = "X", replacement = "", x = datostemp$Clave)
#
#   # Pegamos el nombre de la cuenta
#   datostemp <- left_join(datostemp, catalogo)
#   # Arreglamos nombres faltantes.
#   datostemp <- datostemp %>%
#     mutate(Nombre = case_when(!is.na(Nombre) ~ Nombre,
#                               is.na(Nombre) ~ Clave,
#                               TRUE ~ "Faltante"))
#
#   # Dado que hay mismos nombres para distintos niveles del catalogo m?nimo,
#   # agregamos el primer digito de la clave al nombre para hacerlos distinguibles
#   datostemp <- datostemp %>%
#     mutate(Nombre = case_when(nivelCat == "N1" ~ Nombre,
#                               nivelCat == "N2" ~ paste0(Nombre, substr(Clave, 1, 5)),
#                               nivelCat == "N3" ~ paste0(Nombre, substr(Clave, 1, 9))))
#
#   # Eliminamos la clave
#   datostemp <- datostemp %>% select(-Clave)
#
#   # Hacemos un spread de nuevo
#   datostemp <- datostemp %>%
#     spread(key = Nombre, value = Valor)
#
#   # Pegamos el cluster al que se asigno cada instituci?n
#   clustemp <- ajtemp$Ajuste %>% select(Institucion, Cluster)
#
#   datostemp <- left_join(datostemp, clustemp)
#
#   # Quitamos claves de las instituciones y las agregamos a los nombres de los
#   # renglones
#   row.names(datostemp) <- datostemp$Institucion
#   datostemp <- datostemp %>% select(-Institucion)
#
#   # Con los datos listos, hacemos el ajuste del random forest. Dado que no es
#   # de nuestro inter?s obtener un modelo para reclasificaci?n, si no solo
#   # nos interesa ver la importancia de las variables en la clasificacion, no
#   # dividimos los datos para obtener datos de entrenamiento y de prueba.
#
#   # Numero de predictores
#   noPred <- ncol(datostemp) - 1
#
#   # COnvertimos la columna Cluster a factor para ejecutar clasificaci?n
#   datostemp$Cluster <- as.factor(datostemp$Cluster)
#
#   # Solucionamos un problema con los nombres de las columnas
#   names(datostemp) <- make.names(names(datostemp))
#   # Ajustamos
#   ajusteRF <- randomForest(formula = Cluster ~ .,  data = datostemp,
#                            ntree = 1000, mtry = sqrt(noPred), importance = T)
#
#   # Obtenemos la importancia de las variables
#   importancia <- ajusteRF$importance %>% as.data.frame() %>% tbl_df()
#   importancia$Variable <- row.names(importancia)
#   importancia <- importancia %>% select(Variable,
#                                         MeanDecreaseAccuracy,
#                                         MeanDecreaseGini)
#
#   # Graficamos con ggplot
#   accuracyplot <- importancia %>%
#     select(1, 2) %>%
#     arrange(desc(MeanDecreaseAccuracy)) %>%
#     slice(1:20) %>%
#     ggplot(aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
#     geom_col(fill = randomColor(luminosity = "bright"), colour = "black") +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           axis.title.x = element_blank(),
#           axis.title.y = element_blank()) +
#     coord_flip() +
#     ggtitle(label = "Importancia de las variables de acuerdo a la disminuci?n media en la precisi?n",
#             subtitle = nombre) +
#     NULL
#
#   ggsave(filename = paste0("VarImpo_Accuracy_", nombre, ".tiff"),
#          plot = accuracyplot, device = "tiff", width = 35,
#          height = 15, units = "cm", dpi = 300, limitsize = F)
#
#   giniplot <- importancia %>%
#       select(1, 3) %>%
#       slice(1:20) %>%
#       arrange(desc(MeanDecreaseGini)) %>%
#       ggplot(aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
#       geom_col(fill = randomColor(luminosity = "bright"), colour = "black") +
#       theme_bw() +
#       theme(panel.grid.major = element_blank(),
#             panel.grid.minor = element_blank(),
#             axis.title.x = element_blank(),
#             axis.title.y = element_blank()) +
#       coord_flip() +
#       ggtitle(label = "Importancia de las variables de acuerdo a la disminuci?n en la pureza",
#               subtitle = nombre) +
#     NULL
#
#   ggsave(filename = paste0("VarImpo_Gini_", nombre, ".tiff"),
#          plot = giniplot,
#          device = "tiff", width = 35, height = 15,
#          units = "cm", dpi = 300, limitsize = F)
#
#   # Finalmente, agregamos la fecha al ajuste para pegar al data frame
#   # con la importancia de las variables en el tiempo.
#   importancia$Fecha <- as.Date(nombre)
#   importancia <- importancia %>% select(Fecha, 1:(ncol(importancia)-1))
#
#   importanciaTemporal <- list(importanciaTemporal, importancia) %>%
#     bind_rows()
#
#   # Finalmente, obtenemos la efectividad del modelo
#   errRatetemp <- as.data.frame(mean(ajusteRF$err.rate[, 1]))
#   errRatetemp$Fecha <- as.Date(nombre)
#   names(errRatetemp)[1] <- "TasaError"
#   errRatetemp <- errRatetemp %>% select(Fecha, TasaError)
#
#   errRate <- list(errRate, errRatetemp) %>% bind_rows()
#
# }
#
#
# # Con la importancia de las variables a lo largo de todo el tiempo, hacemos un
# # rank
# importanciaTemporal <- importanciaTemporal %>%
#   group_by(Fecha) %>%
#   mutate(RankAccuracy = order(order(MeanDecreaseAccuracy, decreasing = T))) %>%
#   mutate(RankGini = order(order(MeanDecreaseGini, decreasing = T)))
#
# # Escribimos la importancia de las variables en el tiempo
# importanciaTemporal %>% write_csv(path = "VarImpo_EnTiempo.csv", col_names = T)
#
#
# # Escribimos la tasa de error.
# errRate %>% write_csv(path = "ErrRate_EnTiempo.csv", col_names = T)
#
# rm(list = ls())
# gc()