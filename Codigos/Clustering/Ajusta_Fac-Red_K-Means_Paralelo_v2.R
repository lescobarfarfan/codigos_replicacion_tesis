# -------------------------------------------------------------------------
# Codigo para ajuste de Clusters con Factorial K-Means y Reduced K --------
# -------------------------------------------------------------------------


# Bibliotecas -------------------------------------------------------------

require(gdata)
require(ggplot2)
require(dplyr)
require(lubridate)
require(data.table)
require(Matrix)
require(readxl)
require(clustrd)
require(car)
#require(scatterplot3d)
#require(alluvial)
#require(randomForest)
#require(plotly)
#require(clusterCrit)
#require(useful)
require(readr)
#require(htmlwidgets)
require(magrittr)
require(tidyr)
require(doSNOW)
require(tictoc)

# Directorios -------------------------------------------------------------

# Ruta principal
ruta <- "Ingrese la ruta principal donde se encuentran las funciones aux: \n" %>%
  readline()

# Ruta donde se almacenaran los resultados
rutaPrin <- "Ingrese la ruta donde se almacenaran los ajustes: \n" %>%
  readline()
setwd(rutaPrin)


# Ruta al directorio donde se encuentran los datos fuente:
rutaDatos <- "Ingrese la ruta al directorio donde se encuetran los datos fuente: \n" %>%
  readline()

# Nombre del archivo a procesar
anioFile <- "Ingrese el anio a procesar: \n" %>%
  readline()

# Qu? datos se quieren usar? (Balance, Desempe?o, Ambos)
tipoDatos <- "Que datos quiere utilizar? (Balance, Desempenio, Ambos) \n" %>%
  readline()


# Leemos los datos de desempe?o s?lo si se requieren
if (tipoDatos == "Desempenio" | tipoDatos == "Ambos") {
  # Ruta al archivo de indicadores de desempe?o de los bancos.
  rutaIndi <- "Ingrese la ruta al archivo con los indicadores de desempenio: \n" %>%
    readline()
}


# Rango de clusters a buscar
clusMin <- "Ingrese el numero minimo de clusters que se quiere buscar: \n" %>%
  readline() %>% as.integer()
clusMax <- "Ingrese el numero maximo de clusters que se quiere buscar: \n" %>%
  readline() %>% as.integer()


rangoClus <- clusMin:clusMax
rm(clusMin, clusMax)

# Rango de dimensiones a buscar, recordemos que esta condicionado al numero de
# clusters de acuerdo a Farne
dimMin <- "Ingrese el numero minimo de dimensiones a explorar: \n" %>%
  readline() %>% as.integer()
dimMax <- "Ingrese el numero maximo de dimensiones a explorar: \n" %>%
  readline() %>% as.integer()


rangonDim <- dimMin:dimMax
rm(dimMax, dimMin)


# Escalar los datos de acuerdo al paper de Farne y Voldouis?
escalaManual <- 'Desea escalar los datos de acuerdo a Farne y Voldouis? \n' %>%
  readline() %>%
  as.logical()


# Escalar los datos con la funci?n de ajuste de clusters?
escalaFuncion <- "Desea escalar directamente con la funcion? \n" %>%
  readline() %>%
  as.logical()


# Centrar los datos con la funci?n de ajuste'
centrarFuncion <- "Desea centrar los datos con la funcion? \n" %>%
  readline() %>%
  as.logical()


# Quitar variables colineales de acuerdo a Farne y Voldouis?
quitarCol <- "Desea quitar variables colineales? \n" %>%
  readline() %>%
  as.logical()


# Con qu? nivel de correlaci?n se desean quitar las variables?
if (quitarCol == TRUE) {
  nivelCor <- "Con que nivel de correlacion? \n" %>%
    readline() %>%
    as.double()
}


# Que metodo se desea usar para el ajuste?
metodo <- "Que metodo desea utilizar para el ajuste? (FKM o RKM) \n" %>%
  readline()

# Desea ajustar en los datos completos o en los datos en el subespacio?
datos_FULL <- "Desea ajustar en los datos completos o en el subespacio? \n" %>%
    readline()


# Cuantas simulaciones por ajuste?
noSim <- "Cuantas simulaciones quiere hacer por ajuste? \n" %>%
  readline() %>%
  as.integer()


# Nivel de correlacion para eliminar variables colineales.
source(paste0(ruta, "obten_cluster.R"))
source(paste0(ruta, "elimina_colineales.R"))
source(paste0(ruta, "Elimina_Cols_Vacias.R"))

# Para tener una mejor visualizacion y manejo de las claves, cambiamos el
# formato de los numeros en notaci?n cientifica.
options(scipen = 999)



# --- ---------------------------------------------------------------------


# Procesamiento de datos --------------------------------------------------


# --- ---------------------------------------------------------------------
tic()
cat("Procesando datos... \n")



# S?lo leemos los datos de balance si el ajuste que se quiere hacer los contempla
if (tipoDatos != "Desempe?o") {
  # Si el usuario quiere escalar las variables de acuerdo al paper de Farne, hacemos
  # lo siguiente:
  if (isTRUE(escalaManual)) {

    # Primero, cargamos los datos de balance y los separamos por fechas en una
    # sobre la que iteraremos para palalelizar el proceso lista.
    datosBalanceTemp <- paste0(rutaDatos, anioFile, "_Scaled.csv") %>%
      read_csv(col_names = T)

    datosBalanceTemp$Fecha <- datosBalanceTemp$Fecha %>%
      parse_date_time(c("dmY", "Ymd")) %>% as.Date()

    # datosBalance$Fecha <- datosBalance$Fecha %>%
    #   dmy()

    fechas <- datosBalanceTemp$Fecha %>%
      unique() %>%
      sort()

    # Seleccionamos solo los niveles del catalogo que necesitamos
    datosBalanceTemp <- datosBalanceTemp %>%
      filter(Categoria %in% c(1, 2, 4))

  } else {
    # Primero, cargamos los datos de balance y los separamos por fechas en una
    # sobre la que iteraremos para palalelizar el proceso lista.
    datosBalanceTemp <- paste0(rutaDatos, anioFile, ".csv") %>%
      read_csv(col_names = T)

    datosBalanceTemp$Fecha <- datosBalanceTemp$Fecha %>%
      parse_date_time(c("dmY", "Ymd")) %>% as.Date()


    # datosBalance$Fecha <- datosBalance$Fecha %>%
    #   ymd()

    fechas <- datosBalanceTemp$Fecha %>%
      unique() %>%
      sort()

    # Seleccionamos solo los niveles del catalogo que necesitamos
    datosBalanceTemp <- datosBalanceTemp %>%
      filter(Categoria %in% c(1, 2, 4))

  }

  # Reordenamos las instituciones de acuerdo a la clave
  claves <- names(datosBalanceTemp)[9:length(names(datosBalanceTemp))] %>%
    as.integer() %>% sort() %>% as.character()

  aux1 <- datosBalanceTemp[9:dim(datosBalanceTemp)[2]]
  aux2 <- datosBalanceTemp[1:8]
  aux1 <- aux1[unlist(claves)]

  datosBalanceTemp <- bind_cols(aux2, aux1)
  rm(aux1, aux2)
}

# Ahora, es importante eliminar las instituciones para las que todas las cuentas
# son iguales a cero en una misma fecha, y las variables en las que todas las
# instituciones existentes tienen cero.

# Nuevo data frame donde almacenaremos los datos sin instituciones inexistentes
datosBalance <- data.frame() %>% tbl_df()

# Primero instituciones, recordemos que es por fecha
for (i in 1:length(fechas)) {

  # Filtramos la fecha
  aux <- datosBalanceTemp %>%
    filter(Fecha == fechas[i]) %>%
    gather(key = Institucion, value = Valor, 9:ncol(datosBalanceTemp))

  # Hacemos un gather para obtener la suma de todas las cuentas por institucion
  aux2 <- aux %>%
    group_by(Fecha, Institucion) %>%
    summarize(SumaCuentas = sum(Valor,na.rm = T)) %>%
    filter(SumaCuentas == 0)

  # Obtenemos las instituciones que son cero en todas las cuentas.
  inst_cero <- aux2$Institucion %>% unique() %>% sort()

  # Le quitamos las instituciones que son cero en todo a los datos, los
  # devolvemos a su forma original y los guardamos en el data frame nuevo.
  aux <- aux %>%
    filter(!(Institucion %in% inst_cero)) %>%
    spread(key = Institucion, value = Valor)

  # Ahora, queremos eliminar variables que sean cero para todas las instituciones
  aux3 <- aux[, 9:ncol(aux)]

  suma <- rowSums(aux3)

  aux <- cbind(aux, suma) %>% tbl_df()

  aux <- aux %>%
    filter(suma != 0) %>%
    select(-suma)

  # Ponemos en una forma que no de problemas cuando se agreguen nuevas
  # instituciones
  aux <- aux %>%
    gather(key = Institucion, value = Valor, 9:ncol(aux))

  # Pegamos al data frame con los datos buenos
  datosBalance <- list(datosBalance, aux) %>%
    bind_rows()

  # Eliminamos lo que no necesitamos
  rm(aux, aux2, aux3, suma, inst_cero)
}

# Borramos los datos con todo lo que acabamos de eliminar
rm(datosBalanceTemp)

# Una vez mas, solo leemos los datos de desempenio si es necesario
if (tipoDatos == "Desempenio" | tipoDatos == "Ambos") {
  # Ahora, juntamos con los datos de desempe?o de los bancos para agregar a las
  # variables independientes.
  datosDes <- rutaIndi %>%
    read_csv(col_names = T) %>%
    select(-c(Numero, Nombre)) %>%
    select(Fecha, Clave, IMOR_Consumo, IMOR_Vivienda,
           IMOR_ComercialEmpresas, ICOR_Comercial,
           ICOR_Consumo, ICOR_Vivienda, ICOR_ComercialEmpresas,
           ROAsobreActivo, ROEsobreCapitalContable,
           ROAsobreAPR, ROEsobreCapitalNeto, ICAP)

  # Modificamos la fecha
  datosDes$Fecha <- datosDes$Fecha %>%
    dmy()

  names(datosDes)[2] <- "Institucion"

  # ----- Falta pegar los datos de desempeño con los de balance ----
}


# Pegamos con los datos de balance, primero, necesitamos transformar los datos
# de balance a una forma adecuada, dado que son varias fechas, es necesario
# separar para dar la nueva forma, de lo contrario se tendr?n identificadores
# repetidos para las funciones gather y spread

# Acomodamos los datos en una lista separada por fechas
datosN1_lista <- list()
datosN2_lista <- list()
datosN3_lista <- list()

# Apicamos el acomodo de los datos de acuerdo a los conjutos que se necesitan
if (tipoDatos == "Balance") {
  for (i in 1:length(fechas)) {

    # La separacion se hara por fecha y por nivel de catalogo


    # Nivel 1
    aux1 <- datosBalance %>%
      filter(Fecha == fechas[i] & Nivel1 != 0 & Nivel2 == 0 & Nivel3 == 0) %>%
      select(-c(Concepto, Categoria, Nivel1, Nivel2, Nivel3, Nivel4)) %>%
      spread(key = Clave, value = Valor, fill = 0)

    aux1$Institucion <- aux1$Institucion %>% as.integer()
    # Quitamos la fecha y agregamos la institucion como nombre de renglon
    rownames(aux1) <- aux1$Institucion
    aux1 <- aux1 %>% select(-Fecha, -Institucion)

    # Acomodamos en la lista
    datosN1_lista[[i]] <- aux1


    # Nivel 2
    aux2 <- datosBalance %>%
      filter(Fecha == fechas[i] & Nivel1 != 0 & Nivel2 != 0 & Nivel3 == 0) %>%
      select(-c(Concepto, Categoria, Nivel1, Nivel2, Nivel3, Nivel4)) %>%
      spread(key = Clave, value = Valor, fill = 0)

    aux2$Institucion <- aux2$Institucion %>% as.integer()
    # Quitamos la fecha y agregamos la institucion como nombre de renglon
    rownames(aux2) <- aux2$Institucion
    aux2 <- aux2 %>% select(-Fecha, -Institucion)


    # Agregamos a la lista
    datosN2_lista[[i]] <- aux2



    # Nivel 3
    aux3 <- datosBalance %>%
      filter(Fecha == fechas[i] & Nivel1 != 0 & Nivel2 != 0 & Nivel3 != 0) %>%
      select(-c(Concepto, Categoria, Nivel1, Nivel2, Nivel3, Nivel4)) %>%
      spread(key = Clave, value = Valor, fill = 0)

    aux3$Institucion <- aux3$Institucion %>% as.integer()
    # Quitamos la fecha y agregamos la institucion como nombre de renglon
    rownames(aux3) <- aux3$Institucion
    aux3 <- aux3 %>% select(-Fecha, -Institucion)

    # Agregamos a la lista
    datosN3_lista[[i]] <- aux3

    rm(aux1, aux2, aux3)

  }
} else if (tipoDatos == "Ambos") {
  for (i in 1:length(fechas)) {

    # La separacion se hara por fecha y por nivel de catalogo

    # Nivel 1
    aux1 <- datosBalance %>%
      filter(Fecha == fechas[i] & Nivel1 != 0 & Nivel2 == 0 & Nivel3 == 0) %>%
      select(-c(Concepto, Categoria, Nivel1, Nivel2, Nivel3, Nivel4)) %>%
      spread(key = Clave, value = Valor, fill = 0)

    aux1$Institucion <- aux1$Institucion %>% as.integer()

    # Le pegamos los datos de balance
    aux1 <- left_join(aux1, datosDes, by = c("Fecha", "Institucion"))
    rownames(aux1) <- aux1$Institucion
    aux1[is.na(aux1)] <- 0
    aux1 <- aux1 %>% select(-Fecha, -Institucion)


    datosN1_lista[[i]] <- aux1




    # Nivel 2
    aux2 <- datosBalance %>%
      filter(Fecha == fechas[i] & Nivel1 != 0 & Nivel2 != 0 & Nivel3 == 0) %>%
      select(-c(Concepto, Categoria, Nivel1, Nivel2, Nivel3, Nivel4)) %>%
      spread(key = Clave, value = Valor, fill = 0)

    aux2$Institucion <- aux2$Institucion %>% as.integer()

    # Le pegamos los datos de balance
    aux2 <- left_join(aux2, datosDes, by = c("Fecha", "Institucion"))
    rownames(aux2) <- aux2$Institucion
    aux2[is.na(aux2)] <- 0
    aux2 <- aux2 %>% select(-Fecha, -Institucion)

    # Agregamos a la lista
    datosN2_lista[[i]] <- aux2




    # Nivel 3
    aux3 <- datosBalance %>%
      filter(Fecha == fechas[i] & Nivel1 != 0 & Nivel2 != 0 & Nivel3 != 0) %>%
      select(-c(Concepto, Categoria, Nivel1, Nivel2, Nivel3, Nivel4)) %>%
      spread(key = Clave, value = Valor, fill = 0)

    aux3$Institucion <- aux3$Institucion %>% as.integer()

    # Le pegamos los datos de balance
    aux3 <- left_join(aux3, datosDes, by = c("Fecha", "Institucion"))
    rownames(aux3) <- aux3$Institucion
    aux3[is.na(aux3)] <- 0
    aux3 <- aux3 %>% select(-Fecha, -Institucion)

    # Agregamos a la lista
    datosN3_lista[[i]] <- aux3

    rm(aux1, aux2, aux3)

  }
} else {
  # Insertar para datos sólo de desempenio.

}


names(datosN1_lista) <- fechas
names(datosN2_lista) <- fechas
names(datosN3_lista) <- fechas

# Quitamos las variables para las que todos los bancos reportaron cero
# de cada nivel.
# cat("Eliminando variables vacias... \n")
#
# datosN1_lista <- lapply(datosN1_lista, elimina_cols_vacias)
# datosN2_lista <- lapply(datosN2_lista, elimina_cols_vacias)
# datosN3_lista <- lapply(datosN3_lista, elimina_cols_vacias)


# En adelante, comenzamos con la parte paralela:
noCores <- parallel::detectCores()
cl <- makeCluster(noCores/2)

# registramos el cluster
registerDoSNOW(cl)
# clusterExport(cl, list = c("tbl_df", "mutate", "datosN1_lista"))


# Si queremos quitar las variables colineales:
nofechas <- length(fechas)
if (isTRUE(quitarCol)) {
  cat("Eliminando variables colineales... \n")
  datosN1_lista_NoCor <- foreach(j=1:nofechas, .packages = "dplyr") %dopar% {elimina_colineales(datosN1_lista[[j]], nivelCor)}
  names(datosN1_lista_NoCor) <- names(datosN1_lista)

  datosN2_lista_NoCor <- foreach(j=1:nofechas, .packages = "dplyr") %dopar% {elimina_colineales(datosN2_lista[[j]], nivelCor)}
  names(datosN2_lista_NoCor) <- names(datosN2_lista)

  datosN3_lista_NoCor <- foreach(j=1:nofechas, .packages = "dplyr") %dopar% {elimina_colineales(datosN3_lista[[j]], nivelCor)}
  names(datosN3_lista_NoCor) <- names(datosN3_lista)
}

# Detenemos el cluster
stopCluster(cl)


# Por ?ltimo, creamos de nuevo un cluster con mas nucleos para paralelizar
# el ajuste de los clusters
cl2 <- makeCluster(noCores - 1)
registerDoSNOW(cl2)

cat("Ajustando Clusters... \n")

list_ajustesN1 <- {foreach(i=1:nofechas, .packages = c("dplyr", "clustrd")) %dopar% obten_cluster(datos = datosN1_lista[[i]], no_clus = rangoClus, dime = rangonDim, metodo = metodo, centrar = centrarFuncion, escalar = escalaFuncion, no_inicial = noSim, datos_FULL = datos_FULL)}
save(list_ajustesN1, file = paste0("AjustesN1_ScaledPaper", escalaManual, "_",
                                   "ScaledFunc", escalaFuncion, "_",
                                   anioFile, "_", metodo, "_",
                                   nivelCor, "_Datos-", datos_FULL,  ".RData"))

list_ajustesN2 <- {foreach(i=1:nofechas, .packages = c("dplyr", "clustrd")) %dopar% obten_cluster(datos = datosN2_lista[[i]], no_clus = rangoClus, dime = rangonDim, metodo = metodo, centrar = centrarFuncion, escalar = escalaFuncion, no_inicial = noSim, datos_FULL = datos_FULL)}
save(list_ajustesN2, file = paste0("AjustesN2_ScaledPaper", escalaManual, "_",
                                   "ScaledFunc", escalaFuncion, "_",
                                   anioFile, "_", metodo, "_",
                                   nivelCor, "_Datos-", datos_FULL, ".RData"))

list_ajustesN3 <- {foreach(i=1:nofechas, .packages = c("dplyr", "clustrd")) %dopar% obten_cluster(datos = datosN3_lista[[i]], no_clus = rangoClus, dime = rangonDim, metodo = metodo, centrar = centrarFuncion, escalar = escalaFuncion, no_inicial = noSim,  datos_FULL = datos_FULL)}
save(list_ajustesN3, file = paste0("AjustesN3_ScaledPaper", escalaManual, "_",
                                   "ScaledFunc", escalaFuncion, "_",
                                   anioFile, "_", metodo, "_",
                                   nivelCor, "_Datos-", datos_FULL, ".RData"))

if (isTRUE(quitarCol)) {
cat("Ajustando sin Colineales... \n")
list_ajustesN1_SinCol <- {foreach(i=1:nofechas, .packages = c("dplyr", "clustrd")) %dopar% obten_cluster(datos = datosN1_lista_NoCor[[i]], no_clus = rangoClus, dime = rangonDim, metodo = metodo, centrar = centrarFuncion, escalar = escalaFuncion, no_inicial = noSim,  datos_FULL = datos_FULL)}
save(list_ajustesN1_SinCol, file = paste0("AjustesN1_SinCol_ScaledPaper", escalaManual, "_",
                                          "ScaledFunc", escalaFuncion, "_",
                                          anioFile, "_", metodo, "_",
                                          nivelCor, "_Datos-", datos_FULL, ".RData"))

list_ajustesN2_SinCol <- {foreach(i=1:nofechas, .packages = c("dplyr", "clustrd")) %dopar% obten_cluster(datos = datosN2_lista_NoCor[[i]], no_clus = rangoClus, dime = rangonDim, metodo = metodo, centrar = centrarFuncion, escalar = escalaFuncion, no_inicial = noSim,  datos_FULL = datos_FULL)}
save(list_ajustesN2_SinCol, file = paste0("AjustesN2_SinCol_ScaledPaper", escalaManual, "_",
                                          "ScaledFunc", escalaFuncion, "_",
                                          anioFile, "_", metodo, "_",
                                          nivelCor, "_Datos-", datos_FULL, ".RData"))

list_ajustesN3_SinCol <- {foreach(i=1:nofechas, .packages = c("dplyr", "clustrd")) %dopar% obten_cluster(datos = datosN3_lista_NoCor[[i]], no_clus = rangoClus, dime = rangonDim, metodo = metodo, centrar = centrarFuncion, escalar = escalaFuncion, no_inicial = noSim,  datos_FULL = datos_FULL)}
save(list_ajustesN3_SinCol, file = paste0("AjustesN3_SinCol_ScaledPaper", escalaManual, "_",
                                          "ScaledFunc", escalaFuncion, "_",
                                          anioFile, "_", metodo, "_",
                                          nivelCor, "_Datos-", datos_FULL, ".RData"))

}


stopCluster(cl2)

rm(list = ls())
gc()

cat("FIN! \n")
toc()