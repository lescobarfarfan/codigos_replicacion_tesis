#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# Codigo para graficar distribuciones de cuentas por grupo ----------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------


# Bibliotecas -------------------------------------------------------------
library(tidyverse)
library(ggridges)
library(ggbeeswarm)
library(ggpirate)
library(Hmisc)
library(cowplot)




#   -----------------------------------------------------------------------
# Ajustes con RKM ---------------------------------------------------------
#   -----------------------------------------------------------------------




# Directorios -------------------------------------------------------------

ruta_agrupacion_rkm_n1 <- "/Ruta/Usuario/Agrupaciones_Y_Datos_Balance/AjustesN1_SinCol_ScaledPaperTRUE_ScaledFuncFALSE_Total_RKM_0.75_Datos-low_Agrupaciones.csv"

ruta_vars_imp_rkm_n1 <- "/Ruta/Usuario/Cuentas_Mas_Importantes_RMK_N1_SinCol.csv"



ruta_agrupacion_rkm_n2 <- "/Ruta/Usuario/AjustesN2_SinCol_ScaledPaperTRUE_ScaledFuncFALSE_Total_RKM_0.75_Datos-low_Agrupaciones.csv"

ruta_vars_imp_rkm_n2 <- "/Ruta/Usuario/Cuentas_Mas_Importantes_RMK_N2_SinCol.csv"



ruta_agrupacion_rkm_n3 <- "/Ruta/Usuario/AjustesN3_SinCol_ScaledPaperTRUE_ScaledFuncFALSE_Total_RKM_0.75_Datos-low_Agrupaciones.csv"

ruta_vars_imp_rkm_n3 <- "/Ruta/Usuario/Cuentas_Mas_Importantes_RMK_N3_SinCol.csv"



ruta_datos_balance_escalados <- "/Ruta/Usuario/Total_Scaled.csv"

ruta_datos_balance_full <- "/Ruta/Usuario/Total.csv"





# Funcion auxiliar para partir titulos ------------------------------------
# Funcion auxiliar
parte_cadena <- function(cadena, largo = 12) {
    return(paste(strwrap(cadena, width = largo), collapse = "\n"))
}

parte_cadena_vec <- Vectorize(parte_cadena)

addline_format <- function(x,...){
    gsub('\\s','\n',x)
}



#   -----------------------------------------------------------------------
# Procesamiento de datos --------------------------------------------------
#   -----------------------------------------------------------------------



# Datos generales ---------------------------------------------------------


# Datos escalados
datos_escalados <- ruta_datos_balance_escalados %>%
    read_csv(col_names = T)

datos_escalados <- datos_escalados %>%
    select(Fecha, Clave, 9:ncol(datos_escalados))

datos_escalados <- datos_escalados %>%
    gather(key = Institucion, value = Valor_Escalado, -Fecha, -Clave)

datos_escalados$Institucion <- datos_escalados$Institucion %>% as.double()



# Datos completos
datos_full <- ruta_datos_balance_full %>%
    read_csv(col_names = T)

datos_full <- datos_full %>%
    select(Fecha, Clave, 9:ncol(datos_full))

datos_full <- datos_full %>%
    gather(key = Institucion, value = Valor_Total, -Fecha, -Clave)

datos_full$Institucion <- datos_full$Institucion %>% as.double()



# Datos RKM N1 SinCol -----------------------------------------

datos_agrupacion_rkm_n1 <- ruta_agrupacion_rkm_n1 %>%
    read_csv(col_names = T)
names(datos_agrupacion_rkm_n1)[3] <- "Cluster_RKM_N1"

datos_importantes_rkm_n1 <- ruta_vars_imp_rkm_n1 %>%
    read_csv(col_names = T)



# Datos RKM N2 SinCol -----------------------------------------------------

datos_agrupacion_rkm_n2 <- ruta_agrupacion_rkm_n2 %>%
    read_csv(col_names = T)
names(datos_agrupacion_rkm_n2)[3] <- "Cluster_RKM_N2"

datos_importantes_rkm_n2 <- ruta_vars_imp_rkm_n2 %>%
    read_csv(col_names = T)



# Datos RKM N3 SinCol -----------------------------------------------------

datos_agrupacion_rkm_n3 <- ruta_agrupacion_rkm_n3 %>%
    read_csv(col_names = T)
names(datos_agrupacion_rkm_n3)[3] <- "Cluster_RKM_N3"

datos_importantes_rkm_n3 <- ruta_vars_imp_rkm_n3 %>%
    read_csv(col_names = T)




# Datos finales Escalados ----------------------------------------------------

# Nivel 1
datos_rkm_n1_escalados <- datos_escalados %>%
    filter(Clave %in% datos_importantes_rkm_n1$Clave)

datos_rkm_n1_escalados <- datos_rkm_n1_escalados %>%
    left_join(select(datos_importantes_rkm_n1, Clave, Nombre), by = "Clave")

datos_rkm_n1_escalados <- datos_rkm_n1_escalados %>%
    left_join(datos_agrupacion_rkm_n1, by = c("Fecha", "Institucion"))

datos_rkm_n1_escalados$Nombre <- datos_rkm_n1_escalados$Nombre %>% tolower()
datos_rkm_n1_escalados$Nombre <- datos_rkm_n1_escalados$Nombre %>% capitalize()
datos_rkm_n1_escalados$NombreS <- datos_rkm_n1_escalados$Nombre %>% parte_cadena_vec(largo = 15)



# Nivel 2
datos_rkm_n2_escalados <- datos_escalados %>%
    filter(Clave %in% datos_importantes_rkm_n2$Clave)

datos_rkm_n2_escalados <- datos_rkm_n2_escalados %>%
    left_join(select(datos_importantes_rkm_n2, Clave, Nombre), by = "Clave")

datos_rkm_n2_escalados <- datos_rkm_n2_escalados %>%
    left_join(datos_agrupacion_rkm_n2, by = c("Fecha", "Institucion"))

datos_rkm_n2_escalados$Nombre <- datos_rkm_n2_escalados$Nombre %>% tolower()
datos_rkm_n2_escalados$Nombre <- datos_rkm_n2_escalados$Nombre %>% capitalize()
datos_rkm_n2_escalados$NombreS <- datos_rkm_n2_escalados$Nombre %>% parte_cadena_vec(largo = 15)



# Nivel 3
datos_rkm_n3_escalados <- datos_escalados %>%
    filter(Clave %in% datos_importantes_rkm_n3$Clave)

datos_rkm_n3_escalados <- datos_rkm_n3_escalados %>%
    left_join(select(datos_importantes_rkm_n3, Clave, Nombre), by = "Clave")

datos_rkm_n3_escalados <- datos_rkm_n3_escalados %>%
    left_join(datos_agrupacion_rkm_n3, by = c("Fecha", "Institucion"))

datos_rkm_n3_escalados$Nombre <- datos_rkm_n3_escalados$Nombre %>% tolower()
datos_rkm_n3_escalados$Nombre <- datos_rkm_n3_escalados$Nombre %>% capitalize()
datos_rkm_n3_escalados$NombreS <- datos_rkm_n3_escalados$Nombre %>% parte_cadena_vec(largo = 15)



# Datos finales completos -------------------------------------------------


# Nivel 1
datos_rkm_n1_full <- datos_full %>%
    filter(Clave %in% datos_importantes_rkm_n1$Clave)

datos_rkm_n1_full <- datos_rkm_n1_full %>%
    left_join(select(datos_importantes_rkm_n1, Clave, Nombre), by = "Clave")

datos_rkm_n1_full <- datos_rkm_n1_full %>%
    left_join(datos_agrupacion_rkm_n1, by = c("Fecha", "Institucion"))

datos_rkm_n1_full$Nombre <- datos_rkm_n1_full$Nombre %>% tolower()
datos_rkm_n1_full$Nombre <- datos_rkm_n1_full$Nombre %>% capitalize()
datos_rkm_n1_full$NombreS <- datos_rkm_n1_full$Nombre %>% parte_cadena_vec(largo = 15)



# Nivel 2
datos_rkm_n2_full <- datos_full %>%
    filter(Clave %in% datos_importantes_rkm_n2$Clave)

datos_rkm_n2_full <- datos_rkm_n2_full %>%
    left_join(select(datos_importantes_rkm_n2, Clave, Nombre), by = "Clave")

datos_rkm_n2_full <- datos_rkm_n2_full %>%
    left_join(datos_agrupacion_rkm_n2, by = c("Fecha", "Institucion"))

datos_rkm_n2_full$Nombre <- datos_rkm_n2_full$Nombre %>% tolower()
datos_rkm_n2_full$Nombre <- datos_rkm_n2_full$Nombre %>% capitalize()
datos_rkm_n2_full$NombreS <- datos_rkm_n2_full$Nombre %>% parte_cadena_vec(largo = 15)



# Nivel 3
datos_rkm_n3_full <- datos_full %>%
    filter(Clave %in% datos_importantes_rkm_n3$Clave)

datos_rkm_n3_full <- datos_rkm_n3_full %>%
    left_join(select(datos_importantes_rkm_n3, Clave, Nombre), by = "Clave")

datos_rkm_n3_full <- datos_rkm_n3_full %>%
    left_join(datos_agrupacion_rkm_n3, by = c("Fecha", "Institucion"))

datos_rkm_n3_full$Nombre <- datos_rkm_n3_full$Nombre %>% tolower()
datos_rkm_n3_full$Nombre <- datos_rkm_n3_full$Nombre %>% capitalize()
datos_rkm_n3_full$NombreS <- datos_rkm_n3_full$Nombre %>% parte_cadena_vec(largo = 15)



rm(list = setdiff(ls(), c("datos_rkm_n1_escalados", "datos_rkm_n1_full",
                          "datos_rkm_n2_escalados", "datos_rkm_n2_full",
                          "datos_rkm_n3_escalados", "datos_rkm_n3_full",
                          "datos_escalados", "datos_full",
                          "datos_agrupacion_rkm_n1",
                          "datos_agrupacion_rkm_n2",
                          "datos_agrupacion_rkm_n3", "parte_cadena",
                          "parte_cadena_vec", "addline_format")))


#   -----------------------------------------------------------------------
# Graficas ----------------------------------------------------------------
#   -----------------------------------------------------------------------

# Escalados ---------------------------------------------------------------

# Nivel 1
medidas_rkm_1 <- c(230000000000, 130000000000,
                   420000000000, 410000000000,
                   120000000000, 150000000000,
                   115000000000, 190000000000,
                   170000000000, 210000000000)


datos_rkm_n1_escalados %>%
    filter(Clave %in% medidas_rkm_1) %>%
    filter(!is.na(Cluster_RKM_N1)) %>%
    ggplot(aes(x = Fecha, y = Valor_Escalado,
               colour = factor(Cluster_RKM_N1),
               fill = factor(Cluster_RKM_N1),
               group = interaction(factor(Cluster_RKM_N1), Fecha))) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size = 15, face = "bold"),
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(size = 18, face = "bold")) +
    scale_y_continuous(labels = scales::percent) +
    # ggtitle(label = medidas[i]) +
    facet_wrap(~NombreS, scales = "free_y") +
    NULL



datos_rkm_n1_full %>%
    filter(Clave %in% medidas_rkm_1) %>%
    filter(!is.na(Cluster_RKM_N1)) %>%
    ggplot(aes(x = Fecha, y = Valor_Total,
               colour = factor(Cluster_RKM_N1),
               fill = factor(Cluster_RKM_N1),
               group = interaction(factor(Cluster_RKM_N1), Fecha))) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size = 15, face = "bold"),
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(size = 18, face = "bold")) +
    scale_y_continuous(labels = scales::dollar_format(accuracy = 1.0,
                                                      scale = 1/1000000000)) +
        # ggtitle(label = medidas[i]) +
    facet_wrap(~NombreS, scales = "free_y") +
    NULL



# Nivel 2

medidas_rkm_2 <- c(410100000000, 420100000000,
                   420200000000, 410300000000,
                   211100000000, 420400000000,
                   230300000000, 120100000000,
                   220900000000, 170100000000)

datos_rkm_n2_escalados %>%
    filter(Clave %in% medidas_rkm_2) %>%
    filter(!is.na(Cluster_RKM_N2)) %>%
    ggplot(aes(x = Fecha, y = Valor_Escalado,
               colour = factor(Cluster_RKM_N2),
               fill = factor(Cluster_RKM_N2),
               group = interaction(factor(Cluster_RKM_N2), Fecha))) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size = 15, face = "bold"),
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(size = 18, face = "bold")) +
    scale_y_continuous(labels = scales::percent) +
    # ggtitle(label = medidas[i]) +
    facet_wrap(~NombreS, scales = "free_y") +
    NULL



datos_rkm_n2_full %>%
    filter(Clave %in% medidas_rkm_2) %>%
    filter(!is.na(Cluster_RKM_N2)) %>%
    ggplot(aes(x = Fecha, y = Valor_Total,
               colour = factor(Cluster_RKM_N2),
               fill = factor(Cluster_RKM_N2),
               group = interaction(factor(Cluster_RKM_N2), Fecha))) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size = 15, face = "bold"),
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(size = 18, face = "bold")) +
    scale_y_continuous(labels = scales::dollar_format(accuracy = 1.0,
                                                      scale = 1/1000000000)) +
    # ggtitle(label = medidas[i]) +
    facet_wrap(~NombreS, scales = "free_y") +
    NULL



# Nivel 3

medidas_rkm_3 <- c(420301000000, 211101000000,
                   221406000000, 230203000000,
                   130107000000, 190302000000,
                   221406030100, 221406030000,
                   420302000000, 293101000000)

datos_rkm_n3_escalados %>%
    filter(Clave %in% medidas_rkm_3) %>%
    filter(!is.na(Cluster_RKM_N3)) %>%
    ggplot(aes(x = Fecha, y = Valor_Escalado,
               colour = factor(Cluster_RKM_N3),
               fill = factor(Cluster_RKM_N3),
               group = interaction(factor(Cluster_RKM_N3), Fecha))) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size = 15, face = "bold"),
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(size = 18, face = "bold")) +
    scale_y_continuous(labels = scales::percent) +
    # ggtitle(label = medidas[i]) +
    facet_wrap(~NombreS, scales = "free_y") +
    NULL



datos_rkm_n3_full %>%
    filter(Clave %in% medidas_rkm_3) %>%
    filter(!is.na(Cluster_RKM_N3)) %>%
    ggplot(aes(x = Fecha, y = Valor_Total,
               colour = factor(Cluster_RKM_N3),
               fill = factor(Cluster_RKM_N3),
               group = interaction(factor(Cluster_RKM_N3), Fecha))) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size = 15, face = "bold"),
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(size = 18, face = "bold")) +
    scale_y_continuous(labels = scales::dollar_format(accuracy = 1.0,
                                                      scale = 1/1000000000)) +
    # ggtitle(label = medidas[i]) +
    facet_wrap(~NombreS, scales = "free_y") +
    NULL





#   -----------------------------------------------------------------------
# Analisis para una sola fecha --------------------------------------------
#   -----------------------------------------------------------------------


fechas <- datos_rkm_n1_escalados$Fecha %>% unique() %>% sort()



# Nivel 1
# datos_rkm_n1_escalados %>%
#     filter(Clave %in% medidas_rkm_1) %>%
#     filter(Fecha == fechas[length(fechas)]) %>%
#     filter(!is.na(Cluster_RKM_N1)) %>%
#     ggplot(aes(x = Valor_Escalado, y = factor(NombreS),
#                fill = factor(Cluster_RKM_N1))) +
#     geom_density_ridges()
#
#
#
# datos_rkm_n1_full %>%
#     filter(Clave %in% medidas_rkm_1) %>%
#     filter(Fecha == fechas[length(fechas)]) %>%
#     filter(!is.na(Cluster_RKM_N1)) %>%
#     ggplot(aes(x = Valor_Total, y = factor(NombreS),
#                fill = factor(Cluster_RKM_N1))) +
#     geom_density_ridges() +
#     scale_x_continuous(labels = scales::dollar_format(accuracy = 1.0,
#                                                       scale = 1/1000000000))
#
#
#
# # Nivel 2
# datos_rkm_n2_escalados %>%
#     filter(Clave %in% medidas_rkm_2) %>%
#     filter(Fecha == fechas[length(fechas)]) %>%
#     filter(!is.na(Cluster_RKM_N2)) %>%
#     ggplot(aes(x = Valor_Escalado, y = factor(NombreS),
#                fill = factor(Cluster_RKM_N2))) +
#     geom_density_ridges()
#
#
#
# datos_rkm_n2_full %>%
#     filter(Clave %in% medidas_rkm_2) %>%
#     filter(Fecha == fechas[length(fechas)]) %>%
#     filter(!is.na(Cluster_RKM_N2)) %>%
#     ggplot(aes(x = Valor_Total, y = factor(NombreS),
#                fill = factor(Cluster_RKM_N2))) +
#     geom_density_ridges() +
#     scale_x_continuous(labels = scales::dollar_format(accuracy = 1.0,
#                                                       scale = 1/1000000000))
#
#
#
# # Nivel 3
# datos_rkm_n3_escalados %>%
#     filter(Clave %in% medidas_rkm_3) %>%
#     filter(Fecha == fechas[length(fechas)]) %>%
#     filter(!is.na(Cluster_RKM_N3)) %>%
#     ggplot(aes(x = Valor_Escalado, y = factor(NombreS),
#                fill = factor(Cluster_RKM_N3))) +
#     geom_density_ridges()
#
#
#
# datos_rkm_n3_full %>%
#     filter(Clave %in% medidas_rkm_3) %>%
#     filter(Fecha == fechas[length(fechas)]) %>%
#     filter(!is.na(Cluster_RKM_N3)) %>%
#     ggplot(aes(x = Valor_Total, y = factor(NombreS),
#                fill = factor(Cluster_RKM_N3))) +
#     geom_density_ridges() +
#     scale_x_continuous(labels = scales::dollar_format(accuracy = 1.0,
#                                                       scale = 1/1000000000))




#   -----------------------------------------------------------------------
# Analisis para una sola fecha --------------------------------------------
#   -----------------------------------------------------------------------



ruta_importancia_n1 <- "/Ruta/Usuario/EvoImpoHist_RKM_N1_SinCol.csv"

ruta_importancia_n2 <- "/Ruta/Usuario/EvoImpoHist_RKM_N2_SinCol.csv"

ruta_importancia_n3 <- "/Ruta/Usuario/EvoImpoHist_RKM_N3_SinCol.csv"



# Leemos las variables importantes por fecha ------------------------------

datos_importantes_rkm_n1_all <- ruta_importancia_n1 %>%
    read_csv(col_names = T)

datos_importantes_rkm_n2_all <- ruta_importancia_n2 %>%
    read_csv(col_names = T)

datos_importantes_rkm_n3_all <- ruta_importancia_n3 %>%
    read_csv(col_names = T)



# Pegamos la agrupacion de los tres modelos a los datos -------------------

# datos escalados
datos_escalados <- datos_escalados %>%
    left_join(datos_agrupacion_rkm_n1, by = c("Fecha", "Institucion"))

datos_escalados <- datos_escalados %>%
    left_join(datos_agrupacion_rkm_n2, by = c("Fecha", "Institucion"))

datos_escalados <- datos_escalados %>%
    left_join(datos_agrupacion_rkm_n3, by = c("Fecha", "Institucion"))


# datos full
datos_full <- datos_full %>%
    left_join(datos_agrupacion_rkm_n1, by = c("Fecha", "Institucion"))

datos_full <- datos_full %>%
    left_join(datos_agrupacion_rkm_n2, by = c("Fecha", "Institucion"))

datos_full <- datos_full %>%
    left_join(datos_agrupacion_rkm_n3, by = c("Fecha", "Institucion"))




#   -----------------------------------------------------------------------
# Analisis para la ultima fecha -------------------------------------------
#   -----------------------------------------------------------------------

# extraemos las fechas
fechas <- sort(unique(datos_escalados$Fecha))




# Nivel 1 -----------------------------------------------------------------

# datos escalados ---------------------------------------------------------
datos_escalados_ult_fecha <- datos_escalados %>%
    filter(Fecha == fechas[length(fechas)])

importantes_n1_ultima_fecha <- datos_importantes_rkm_n1_all %>%
    filter(Fecha == fechas[length(fechas)]) %>%
    select(-Variable)

# Pegamos los nombres, la importancia y el rank de las variables importantes
datos_escalados_ult_fecha_n1 <- datos_escalados_ult_fecha %>%
    inner_join(importantes_n1_ultima_fecha, by = c("Fecha", "Clave"))

cuentas_ult_fecha_n1 <- parte_cadena_vec(datos_escalados_ult_fecha_n1$Nombre)


# Visualizacion
graf_escalados_n1 <- datos_escalados_ult_fecha_n1 %>%
    ggplot(aes(x = Valor_Escalado, y = reorder(Nombre, desc(Rank)),
               fill = factor(Cluster_RKM_N1))) +
    geom_density_ridges() +
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 15),
          axis.text = element_text(face = "bold")) +
    scale_y_discrete(labels = cuentas_ult_fecha_n1) +
    ggtitle(label = "Distribucion por grupo de los datos escalados",
            subtitle = "Primer nivel de granularidad")



# # datos completos ---------------------------------------------------------
#
# datos_full_ult_fecha <- datos_full %>%
#     filter(Fecha == fechas[length(fechas)])
#
# datos_full_ult_fecha_n1 <- datos_full_ult_fecha %>%
#     inner_join(importantes_n1_ultima_fecha, by = c("Fecha", "Clave"))
#
# graf_full_n1 <- datos_full_ult_fecha_n1 %>%
#     ggplot(aes(x = Valor_Total, y = reorder(Nombre, desc(Rank)),
#                fill = factor(Cluster_RKM_N1))) +
#     geom_density_ridges() +
#     theme_bw() +
#     theme(axis.title = element_blank(),
#           legend.title = element_blank(),
#           legend.position = "bottom",
#           plot.title = element_text(size = 18, face = "bold"),
#           plot.subtitle = element_text(size = 12),
#           axis.text = element_text(face = "bold")) +
#     scale_y_discrete(labels = cuentas_ult_fecha_n1) +
#     scale_x_continuous(labels = scales::dollar_format(accuracy = 0.1,
#                                                       scale = 1/1000000000,
#                                                       prefix = "")) +
#     ggtitle(label = "Datos sin escalar",
#             subtitle = "Millones de pesos")
#
#
#
# # Acomodamos lado a lado con cowplot
# graf_grid_n1 <- plot_grid(graf_escalados_n1, graf_full_n1,
#                           labels = c("A", "B"))
#
# titulo <- ggdraw() +
#     draw_label("Distribucion por grupos para el primer nivel de granularidad",
#                fontface = "bold", size = 18)
# plot_grid(titulo, graf_grid_n1, ncol = 1, rel_heights = c(0.1, 1))






# Nivel 2 -----------------------------------------------------------------
# datos escalados ---------------------------------------------------------
datos_escalados_ult_fecha <- datos_escalados %>%
    filter(Fecha == fechas[length(fechas)])

importantes_n2_ultima_fecha <- datos_importantes_rkm_n2_all %>%
    filter(Fecha == fechas[length(fechas)]) %>%
    select(-Variable)

# Pegamos los nombres, la importancia y el rank de las variables importantes
datos_escalados_ult_fecha_n2 <- datos_escalados_ult_fecha %>%
    inner_join(importantes_n2_ultima_fecha, by = c("Fecha", "Clave"))

top10 <- datos_escalados_ult_fecha_n2 %>%
    select(Nombre, Rank) %>%
    arrange(Rank) %>%
    distinct() %>%
    slice(1:10)

cuentas_ult_fecha_n2 <- parte_cadena_vec(top10$Nombre)

# Visualizacion
graf_escalados_n2 <- datos_escalados_ult_fecha_n2 %>%
    filter(Nombre %in% top10$Nombre) %>%
    ggplot(aes(x = Valor_Escalado, y = reorder(Nombre, desc(Rank)),
               fill = factor(Cluster_RKM_N2))) +
    geom_density_ridges() +
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 15),
          axis.text = element_text(face = "bold")) +
    scale_y_discrete(labels = cuentas_ult_fecha_n2) +
    ggtitle(label = "Distribucion por grupo de los datos escalados",
            subtitle = "Segundo nivel de granularidad")


# Para obtener una visualizacion de que pasa con el cuarto grupo,
# obtenemos los cuantiles de las cuentas por grupo

# Primero, definimos que cuantiles queremos
# Definimos funciones auxiliares
p <- c(0.05, 0.5, 0.95)
p_names <- map_chr(p, ~paste0(.x*100, "%"))

p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = p_names)

# p_funs[[4]] <- partial(median, na.rm = T)
# names(p_funs)[4] <- "Median"


# Calcualmos los cuantiles
cuantiles <- datos_escalados_ult_fecha_n2 %>%
    select(Cluster_RKM_N2, Nombre, Valor_Escalado) %>%
    filter(Nombre %in% top10$Nombre) %>%
    group_by(Cluster_RKM_N2, Nombre) %>%
    summarize_at(vars(Valor_Escalado), p_funs) %>%
    ungroup() %>%
    filter(!is.na(Cluster_RKM_N2)) %>%
    select(-Cluster_RKM_N2)


# Obtenemos algo para ver qu? onda con el grupo 4
datos_escalados_ult_fecha_n2_gpo4 <- datos_escalados_ult_fecha_n2 %>%
    filter(Cluster_RKM_N2 == 4) %>%
    filter(Nombre %in% top10$Nombre)

# Cruzamos con los cuantiles
datos_escalados_ult_fecha_n2_gpo4 <- datos_escalados_ult_fecha_n2_gpo4 %>%
    left_join(cuantiles, by = "Nombre")

datos_escalados_ult_fecha_n2_gpo4 <- datos_escalados_ult_fecha_n2_gpo4 %>%
    mutate(Diferencia_5 = abs(Valor_Escalado - `5%`),
           Diferencia_50 = abs(Valor_Escalado - `50%`),
           Diferencia_95 = abs(Valor_Escalado - `95%`))

datos_escalados_ult_fecha_n2_gpo4 %>%
    # select(Nombre, Institucion, Diferencia_5, Diferencia_50, Diferencia_95) %>%
    # gather(key = Diferencia, value = Valor, 3:5) %>%
    ggplot(aes(y = Nombre, x = Valor_Escalado, group = factor(Institucion),
               colour = factor(Institucion))) +
    geom_point(size = 2) +
    geom_point(data = cuantiles %>% gather(key = Cuantil, value = Valor, 2:4),
               aes(x = Valor, y = Nombre, group = Cuantil, colour = Cuantil),
               size = 4)





# # datos completos ---------------------------------------------------------
#
# datos_full_ult_fecha <- datos_full %>%
#     filter(Fecha == fechas[length(fechas)])
#
# datos_full_ult_fecha_n1 <- datos_full_ult_fecha %>%
#     inner_join(importantes_n1_ultima_fecha, by = c("Fecha", "Clave"))
#
# graf_full_n1 <- datos_full_ult_fecha_n1 %>%
#     ggplot(aes(x = Valor_Total, y = reorder(Nombre, desc(Rank)),
#                fill = factor(Cluster_RKM_N1))) +
#     geom_density_ridges() +
#     theme_bw() +
#     theme(axis.title = element_blank(),
#           legend.title = element_blank(),
#           legend.position = "bottom",
#           plot.title = element_text(size = 18, face = "bold"),
#           plot.subtitle = element_text(size = 12),
#           axis.text = element_text(face = "bold")) +
#     scale_y_discrete(labels = cuentas_ult_fecha_n1) +
#     scale_x_continuous(labels = scales::dollar_format(accuracy = 0.1,
#                                                       scale = 1/1000000000,
#                                                       prefix = "")) +
#     ggtitle(label = "Datos sin escalar",
#             subtitle = "Millones de pesos")
#
#
#
# # Acomodamos lado a lado con cowplot
# graf_grid_n1 <- plot_grid(graf_escalados_n1, graf_full_n1,
#                           labels = c("A", "B"))
#
# titulo <- ggdraw() +
#     draw_label("Distribucion por grupos para el primer nivel de granularidad",
#                fontface = "bold", size = 18)
# plot_grid(titulo, graf_grid_n1, ncol = 1, rel_heights = c(0.1, 1))




# Nivel 3 -----------------------------------------------------------------

# datos escalados ---------------------------------------------------------
datos_escalados_ult_fecha <- datos_escalados %>%
    filter(Fecha == fechas[length(fechas)])

importantes_n3_ultima_fecha <- datos_importantes_rkm_n3_all %>%
    filter(Fecha == fechas[length(fechas)]) %>%
    select(-Variable)

# Pegamos los nombres, la importancia y el rank de las variables importantes
datos_escalados_ult_fecha_n3 <- datos_escalados_ult_fecha %>%
    inner_join(importantes_n3_ultima_fecha, by = c("Fecha", "Clave"))

top10 <- datos_escalados_ult_fecha_n3 %>%
    select(Nombre, Rank) %>%
    arrange(Rank) %>%
    distinct() %>%
    slice(1:10)

cuentas_ult_fecha_n3 <- parte_cadena_vec(top10$Nombre)

# Visualizacion
graf_escalados_n3 <- datos_escalados_ult_fecha_n3 %>%
    filter(Nombre %in% top10$Nombre) %>%
    ggplot(aes(x = Valor_Escalado, y = reorder(Nombre, desc(Rank)),
               fill = factor(Cluster_RKM_N3))) +
    geom_density_ridges() +
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 15),
          axis.text = element_text(face = "bold")) +
    scale_y_discrete(labels = cuentas_ult_fecha_n3) +
    ggtitle(label = "Distribucion por grupo de los datos escalados",
            subtitle = "Tercer nivel de granularidad")



# # datos completos ---------------------------------------------------------
#
# datos_full_ult_fecha <- datos_full %>%
#     filter(Fecha == fechas[length(fechas)])
#
# datos_full_ult_fecha_n1 <- datos_full_ult_fecha %>%
#     inner_join(importantes_n1_ultima_fecha, by = c("Fecha", "Clave"))
#
# graf_full_n1 <- datos_full_ult_fecha_n1 %>%
#     ggplot(aes(x = Valor_Total, y = reorder(Nombre, desc(Rank)),
#                fill = factor(Cluster_RKM_N1))) +
#     geom_density_ridges() +
#     theme_bw() +
#     theme(axis.title = element_blank(),
#           legend.title = element_blank(),
#           legend.position = "bottom",
#           plot.title = element_text(size = 18, face = "bold"),
#           plot.subtitle = element_text(size = 12),
#           axis.text = element_text(face = "bold")) +
#     scale_y_discrete(labels = cuentas_ult_fecha_n1) +
#     scale_x_continuous(labels = scales::dollar_format(accuracy = 0.1,
#                                                       scale = 1/1000000000,
#                                                       prefix = "")) +
#     ggtitle(label = "Datos sin escalar",
#             subtitle = "Millones de pesos")
#
#
#
# # Acomodamos lado a lado con cowplot
# graf_grid_n1 <- plot_grid(graf_escalados_n1, graf_full_n1,
#                           labels = c("A", "B"))
#
# titulo <- ggdraw() +
#     draw_label("Distribucion por grupos para el primer nivel de granularidad",
#                fontface = "bold", size = 18)
# plot_grid(titulo, graf_grid_n1, ncol = 1, rel_heights = c(0.1, 1))
