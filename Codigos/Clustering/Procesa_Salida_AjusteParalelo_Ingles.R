#   -----------------------------------------------------------------------

# Codigo para procesar salida de la funcion de ajuste en paralelo ---------

#   -----------------------------------------------------------------------



# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(ggrepel)
library(alluvial)
library(ggalluvial)
library(scales)


# Directorios -------------------------------------------------------------

# Ruta donde se escribira la salida
rutaPrin <- "Escriba la ruta donde se escribira la salida del programa: \n" %>%
  readline()
setwd(rutaPrin)

# Ruta donde se encuentran los ajustes.
rutaAjustes <- "Escriba la ruta donde se encuentran los ajustes: \n" %>%
  readline()

# NIvel del catalogo
nivelCat <- "A que nivel del catalogo pertenecen los ajustes? (N1, N2, N3, All) \n" %>%
  readline()

# COn o sin colineales?
consinCol <- "Los ajustes son con o sin variables colineales? (SinCol, ConCol, All) \n" %>%
  readline()

# Lectura y acomodo de ajustes --------------------------------------------

# Obtenemos los nombres de los archivos.
files <- rutaAjustes %>%
  list.files(pattern = "*.RData")


# Funcion auxiliar
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# En el caso donde tenemos todas las fechas en un solo archivo ------------

if ((nivelCat == "All") & (consinCol == "All")) {

  # Dado que cada archivo en el vector de nombres tiene propiedades distintas,
  # recorremos y hacemos análisis por cada uno. Es importante tener en cuenta
  # que el orden de los archivos en las carpetas siempre es el mismo.

  fechas <- as.Date(c("2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31",
                      "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31",
                      "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31",
                      "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31",
                      "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31",
                      "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31",
                      "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31",
                      "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31",
                      "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31",
                      "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31",
                      "2017-03-31", "2017-06-30"))

for (i in 1:length(files)) {

    # Cargamos el ajuste:
    ajuste <- loadRData(paste0(rutaAjustes, files[i]))

    # Agregamos nombres
    names(ajuste) <- fechas

    # Queremos obtener varios datos relevantes para el análisis
    agrupaciones <- data.frame() %>% tbl_df()
    # dimensiones <- data.frame() %>% tbl_df()
    # noClusters <- data.frame() %>% tbl_df()
    # criterioAjuste <- data.frame() %>% tbl_df()

    # Para guardar los datos para los bosques aleatorios
    # Datos_RF <- list()

    # Recorremos la lista
    for (j in 1:length(ajuste)) {

      # Checamos si el ajuste es Nulo
      if (is.null(ajuste[[j]])) {
        next(j)
      } else {
        # Extraemos lo necesario:
        # Agrupaciones
        temp <- ajuste[[j]]$Ajuste
        temp$Fecha <- as.Date(names(ajuste)[j])
        temp <- temp %>% select(Fecha, Institucion, Cluster)
        agrupaciones <- list(agrupaciones, temp) %>% bind_rows()
        rm(temp)

        # Dimensiones
        # temp <- ncol(ajuste[[j]]$Salida_obj$obscoord)
        # temp$Fecha <- as.Date(names(ajuste)[j])
        # temp <- as.data.frame(temp) %>% tbl_df()
        # names(temp) <- c("NoDim", "Fecha")
        # temp <- temp %>% select(Fecha, NoDim)
        # dimensiones <- list(dimensiones, temp) %>% bind_rows()
        # rm(temp)
        #
        # # Clusters
        # temp <- max(ajuste[[j]]$Salida_obj$cluster)
        # temp$Fecha <- as.Date(names(ajuste)[j])
        # temp <- as.data.frame(temp) %>% tbl_df()
        # names(temp) <- c("NoClusters", "Fecha")
        # temp <- temp %>% select(Fecha, NoClusters)
        # noClusters <- list(noClusters, temp) %>% bind_rows()
        # rm(temp)
        #
        # # Criterio de ajuste
        # temp <- ajuste[[j]]$Salida_obj$criterion
        # temp$Fecha <- as.Date(names(ajuste)[j])
        # temp <- as.data.frame(temp) %>% tbl_df()
        # names(temp) <- c("Criterio", "Fecha")
        # temp <- temp %>% select(Fecha, Criterio)
        # criterioAjuste <- list(criterioAjuste, temp) %>% bind_rows()
        # rm(temp)
        #
        # # Guardamos los objetos para bosques aleatorios
        # temp <- ajuste[[j]]$Salida_obj$odata
        # temp$Institucion <- rownames(temp)
        # temp <- temp %>% select(Institucion, 2:(ncol(temp) - 1))
        #
        # # Pegamos clusters
        # tempClus <- as.data.frame(ajuste[[j]]$Salida_obj$cluster)
        # tempClus$Institucion <- rownames(tempClus)
        # names(tempClus)[1] <- "Cluster"
        #
        # temp <- inner_join(temp, tempClus)
        #
        # Datos_RF[[j]] <- temp

        rm(temp, tempClus)
      }

    }

    # Guardamos lo creado
    nombres_parcial <- gsub(pattern = ".RData", replacement = "", x = files[i])

    # save(Datos_RF, file = paste0(rutaPrin, paste0(nombres_parcial, "_Datos_RF.RData")))
    #
    # write_csv(agrupaciones,
    #           paste0(rutaPrin, paste0(nombres_parcial, "_Agrupaciones.csv")),
    #           col_names = T)
    #
    # write_csv(criterioAjuste,
    #           paste0(rutaPrin, paste0(nombres_parcial, "_CritAjuste.csv")),
    #           col_names = T)
    #
    # write_csv(dimensiones,
    #           paste0(rutaPrin, paste0(nombres_parcial, "_Dimensiones.csv")),
    #           col_names = T)
    #
    # write_csv(noClusters,
    #           paste0(rutaPrin, paste0(nombres_parcial, "_NoClusters.csv")),
    #           col_names = T)
    # Graficamos y guardamos el movimiento de los clusters con un diagrama
    # aluvial.
    plotAlluvial <- agrupaciones %>%
      ggplot(aes(x = factor(Fecha), stratum = factor(Cluster),
                 alluvium = factor(Institucion),
                 fill = factor(Cluster),
                 label = factor(Cluster))) +
      geom_flow(stat = "alluvium") +
      geom_stratum() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90),
            axis.title.x = element_blank(),
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            title = element_text(face = "bold")) +#,
      #axis.text.y = element_blank()) +
      geom_text(stat = "stratum", size = 3) +
      ggtitle("Alluvial diagram of cluster changes across time")
    # Guardamos
    ggsave(filename = paste0(rutaPrin, paste0(nombres_parcial,
                                              "_PlotAlluvial.png")),
           plot = plotAlluvial, device = "png", width = 24, height = 16,
           units = "cm", dpi = 300)

    # next(i)

}

} else {
  # Leemos y acomodamos en una lista.
  lista_Ajustes <- list()

  # Dependiendo del nivel y de las variables contenidas es el nombre de los
  # objetos creados
  if (consinCol == "ConCol") {

    # Recorremos los archivos
    for (i in 1:length(files)) {

      # Cargamos el ajuste.
      load(paste0(rutaAjustes,files[i]))# %>% load()

      # Creamos el nombre del objeto que se acaba de leer
      name <- paste0("list_ajustes", nivelCat)

      # guardamos en la lista
      lista_Ajustes[[i]] <- eval(parse(text = name))

    }

  } else if (consinCol == "SinCol") {

    for (i in 1:length(files)) {

      # Cargamos el ajuste
      load(paste0(rutaAjustes, files[i]))

      # Creamos nombres
      name <- paste0("list_ajustes", nivelCat, "_SinCol")

      lista_Ajustes[[i]] <- eval(parse(text = name))

    }

  }

  # Damos nombres a cada miembro de la lista de acuerdo a su año
  anios <- seq(2007, 2017, 1)
  meses <- c("03-31", "06-30", "09-30", "12-31")
  names(lista_Ajustes) <- anios

  # Ahora, le agregamos nombres a las sublistas
  for (i in 1:length(lista_Ajustes)) {
    for (j in 1:length(lista_Ajustes[[i]])) {
      names(lista_Ajustes[[i]])[j] <- meses[j]
    }
  }



  # Obtención de datos relevantes -------------------------------------------
  agrupaciones <- tbl_df(data.frame())
  dimensiones <- tbl_df(data.frame())
  noClusters <- tbl_df(data.frame())
  criterioAjuste <- tbl_df(data.frame())

  # Recorremos la lista
  for (i in 1:length(lista_Ajustes)) {

    for (j in 1:length(lista_Ajustes[[i]])) {

      # Obtenemos la fecha
      fecha <- paste0(names(lista_Ajustes)[i], "-", names(lista_Ajustes[[i]])[j])

      # Checamos si el ajuste es NULL o no.
      if (is.null(lista_Ajustes[[i]][[j]])) {
        next(j)
      } else {
        # Agrupaciones
        temp <- lista_Ajustes[[i]][[j]]$Ajuste
        temp$Fecha <- fecha
        temp <- temp %>% select(Fecha, Institucion, Cluster)
        temp$Fecha <- temp$Fecha %>% as.Date()
        # Pegamos
        agrupaciones <-  list(agrupaciones, temp) %>% bind_rows()
        rm(temp)



        # Dimensiones
        temp <- ncol(lista_Ajustes[[i]][[j]]$Salida_obj$obscoord)
        temp$Fecha <- fecha
        temp <- tbl_df(as.data.frame(temp))
        names(temp) <- c("NoDim", "Fecha")
        temp <- temp %>% select(Fecha, NoDim)
        # Pegamos
        dimensiones <- list(dimensiones, temp) %>% bind_rows()
        rm(temp)



        # Clusters
        temp <- max(lista_Ajustes[[i]][[j]]$Salida_obj$cluster)
        temp$Fecha <- fecha
        temp <- tbl_df(as.data.frame(temp))
        names(temp) <- c("NoClusters", "Fecha")
        temp <- temp %>% select(Fecha, NoClusters)
        # Pegamos
        noClusters <- list(noClusters, temp) %>% bind_rows()
        rm(temp)




        # Criterio ajuste
        temp <- lista_Ajustes[[i]][[j]]$Salida_obj$criterion
        temp$Fecha <- fecha
        temp <- tbl_df(as.data.frame(temp))
        names(temp) <- c("Criterio", "Fecha")
        temp <- temp %>% select(Fecha, Criterio)
        # Pegamos
        criterioAjuste <- list(criterioAjuste, temp) %>% bind_rows()
        rm(temp)
      }
    }
  }

  # Guardamos el objeto con todas las fechas junta:
  listaAll <- do.call(c, lista_Ajustes)
  save(listaAll,
       file = paste0(rutaPrin, "Ajustes_Completos.RData"))


  # Obtenemos un objeto con todos los datos listos para hacer un ajuste de
  # RandomForest.
  datos_RF <- list()

  for (i in 1:length(listaAll)) {

    # Checamos por valores nulos
    if (is.null(listaAll[[i]])) {
      next(i)
    } else {
      # Extreamos los datos originales.
      temp <- listaAll[[i]]$Salida_obj$odata
      temp$Institucion <- rownames(temp)
      temp <- temp %>% select(Institucion, 2:(ncol(temp) - 1))

      # Extraemos los clusters
      tempClus <- as.data.frame(listaAll[[i]]$Salida_obj$cluster)
      tempClus$Institucion <- rownames(tempClus)
      names(tempClus)[1] <- "Cluster"

      # Pegamos con los datos completos
      temp <- inner_join(temp, tempClus)

      datos_RF[[i]] <- temp

      rm(temp, tempClus)
    }
  }

  # Guardamos el objeto con los datos para los bosques aleatorios
  save(datos_RF, file = paste0(rutaPrin, "Datos_RF.RData"))



  # Graficas ----------------------------------------------------------------

  # Alluvial
  plotAlluvial <- agrupaciones %>%
    ggplot(aes(x = factor(Fecha), stratum = factor(Cluster),
               alluvium = factor(Institucion),
               fill = factor(Cluster),
               label = factor(Cluster))) +
    geom_flow(stat = "alluvium") +
    geom_stratum() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          title = element_text(face = "bold")) +#,
    #axis.text.y = element_blank()) +
    geom_text(stat = "stratum", size = 3) +
    ggtitle("Diagrama alluvial de los movimientos de cluster")
  # Guardamos
  ggsave(filename = paste0(rutaPrin, "PlotAlluvial.tiff"), plot = plotAlluvial,
         device = "tiff", width = 24, height = 16, units = "cm", dpi = 300)






  # Numero de clusters
  plotNoClus <- noClusters %>%
    ggplot(aes(x = as.Date(Fecha), y = NoClusters)) +
    geom_line(colour = "darkblue", size = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_x_date(breaks = as.Date(sort(unique(noClusters$Fecha)))) +
    scale_y_continuous(breaks = seq(min(noClusters$NoClusters),
                                    max(noClusters$NoClusters),1)) +
    ggtitle(label = "Número de clusters")
  # Guardamos
  ggsave(filename = paste0(rutaPrin, "PlotNoClust.tiff"),
         plot = plotNoClus, device = "tiff", width = 18,
         height = 9, units = "cm", dpi = 300)





  # Numero de dimensiones
  plotNoDim <- dimensiones %>%
    ggplot(aes(x = as.Date(Fecha), y = NoDim)) +
    geom_line(colour = "red", size = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_x_date(breaks = as.Date(sort(unique(noClusters$Fecha)))) +
    scale_y_continuous(breaks = seq(min(dimensiones$NoDim),
                                    max(dimensiones$NoDim),1)) +
    ggtitle(label = "Número de dimensiones")
  # Guardamos
  ggsave(filename = paste0(rutaPrin, "PlotNoDim.tiff"),
         plot = plotNoDim, device = "tiff", width = 18,
         height = 9, units = "cm", dpi = 300)





  # Evolución de la calidad de los ajustes
  plotCritAj <- criterioAjuste %>%
    ggplot(aes(x = as.Date(Fecha), y = Criterio)) +
    geom_line(colour = "green", size = 1) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_x_date(breaks = as.Date(sort(unique(noClusters$Fecha)))) +
    # scale_y_continuous(breaks = seq(min(dimensiones$NoDim),
    #                                 max(dimensiones$NoDim),1)) +
    ggtitle(label = "Criterio de Ajuste (Average Silhoutte Width)")
  # Guardamos
  ggsave(filename = paste0(rutaPrin, "PlotCriterioAjuste.tiff"),
         plot = plotCritAj, device = "tiff", width = 18,
         height = 9, units = "cm", dpi = 300)





  # Escribimos todos los data frames otenidos
  write_csv(agrupaciones, paste0(rutaPrin, "Agrupaciones.csv"), col_names = T)
  write_csv(criterioAjuste, paste0(rutaPrin, "CritAjuste.csv"), col_names = T)
  write_csv(dimensiones, paste0(rutaPrin, "Dimensiones.csv"), col_names = T)
  write_csv(noClusters, paste0(rutaPrin, "NoClusters.csv"), col_names = T)


  # Eliminamos todo
  rm(list = ls())
  gc()
}

rm(list = ls())
gc()

