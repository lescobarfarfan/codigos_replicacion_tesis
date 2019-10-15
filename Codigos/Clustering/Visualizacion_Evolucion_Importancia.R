#   -------------------------------------------------------------------------
#   -------------------------------------------------------------------------
#   -------------------------------------------------------------------------
# Codigo para obtener graficas de la importancia de las variables RF  -------
#   -------------------------------------------------------------------------
#   -------------------------------------------------------------------------
#   -------------------------------------------------------------------------



# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(scales)
library(gghighlight)
library(Hmisc)



# Directorios -------------------------------------------------------------

ruta_datos_rkm_n1 <- "/Ruta/Usuario/EvoImpoHist_RKM_N1_SinCol.csv"


ruta_datos_rkm_n2 <- "/Ruta/Usuario/EvoImpoHist_RKM_N2_SinCol.csv"


ruta_datos_rkm_n3 <- "/Ruta/Usuario/EvoImpoHist_RKM_N3_SinCol.csv"

ruta_escritura <- "/Ruta/Usuario/"


# Procesamiento de datos --------------------------------------------------

datos_rkm_n1 <- ruta_datos_rkm_n1 %>%
    read_csv(col_names = T)

datos_rkm_n2 <- ruta_datos_rkm_n2 %>%
    read_csv(col_names = T)

datos_rkm_n3 <- ruta_datos_rkm_n3 %>%
    read_csv(col_names = T)


# Funcion auxiliar
parte_cadena <- function(cadena, largo = 12) {
    return(paste(strwrap(cadena, width = largo), collapse = "\n"))
}

parte_cadena_vec <- Vectorize(parte_cadena)




#   -----------------------------------------------------------------------
# Obtencion de graficas ---------------------------------------------------
#   -----------------------------------------------------------------------


# Nivel 1 -----------------------------------------------------------------
datos_rkm_n1$Nombre <- tolower(datos_rkm_n1$Nombre)
datos_rkm_n1$Nombre <- datos_rkm_n1$Nombre %>% capitalize()

datos_rkm_n1$Nombre_split <- parte_cadena_vec(datos_rkm_n1$Nombre)

datos_rkm_n1_conteo <- datos_rkm_n1 %>%
    group_by(Clave) %>%
    summarise(NoVeces = n())

datos_rkm_n1_prom <- datos_rkm_n1 %>%
    group_by(Clave) %>%
    summarise(PromRank = mean(Rank))


cuentas_rkm_n1_mas_frecuentes_y_top <- inner_join(datos_rkm_n1_conteo, datos_rkm_n1_prom)

cuentas_rkm_n1_mas_frecuentes_y_top <- cuentas_rkm_n1_mas_frecuentes_y_top %>%
    arrange(desc(NoVeces), PromRank) %>%
    slice(1:10)

# Escribimos
# cuentas_rkm_n1_mas_frecuentes_y_top %>%
#     write_csv(paste0(ruta_escritura, "Cuentas_Mas_Importantes_RMK_N1_SinCol.csv"),
#               col_names = T)

datos_rkm_n1 %>%
    filter(Clave %in% cuentas_rkm_n1_mas_frecuentes_y_top$Clave) %>%
    ggplot(aes(x = Fecha, y = Importancia, group = Nombre_split, colour = Nombre_split)) +
    geom_line(size = 1) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_text(size = 15, face = "bold"),
          axis.text.x = element_text(angle = 90),
          strip.text = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 18, face = "bold")) +
    facet_wrap(~Nombre_split) +
    ggtitle(label = "Cuentas mas importantes para el ajuste RKM_N1_SinCol",
            subtitle = "Decaimiento promedio en la precision") +
    NULL



# Nivel 2 -----------------------------------------------------------------
datos_rkm_n2$Nombre <- tolower(datos_rkm_n2$Nombre)
datos_rkm_n2$Nombre <- datos_rkm_n2$Nombre %>% capitalize()

datos_rkm_n2$Nombre_split <- parte_cadena_vec(datos_rkm_n2$Nombre)

datos_rkm_n2_conteo <- datos_rkm_n2 %>%
    group_by(Clave) %>%
    summarise(NoVeces = n())

datos_rkm_n2_prom <- datos_rkm_n2 %>%
    group_by(Clave) %>%
    summarise(PromRank = mean(Rank))


cuentas_rkm_n2_mas_frecuentes_y_top <- inner_join(datos_rkm_n2_conteo, datos_rkm_n2_prom)

cuentas_rkm_n2_mas_frecuentes_y_top <- cuentas_rkm_n2_mas_frecuentes_y_top %>%
    arrange(desc(NoVeces), PromRank) %>%
    slice(1:10)


# Escribimos
# cuentas_rkm_n2_mas_frecuentes_y_top %>%
#     write_csv(paste0(ruta_escritura, "Cuentas_Mas_Importantes_RMK_N2_SinCol.csv"),
#               col_names = T)


datos_rkm_n2 %>%
    filter(Clave %in% cuentas_rkm_n2_mas_frecuentes_y_top$Clave) %>%
    ggplot(aes(x = Fecha, y = Importancia, group = Nombre_split, colour = Nombre_split)) +
    geom_line(size = 1) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_text(size = 15, face = "bold"),
          axis.text.x = element_text(angle = 90),
          strip.text = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 18, face = "bold")) +
    facet_wrap(~Nombre_split) +
    ggtitle(label = "Cuentas mas importantes para el ajuste RKM_N2_SinCol",
            subtitle = "Decaimiento promedio en la precision") +
    NULL






# Nivel 3 -----------------------------------------------------------------
datos_rkm_n3$Nombre <- tolower(datos_rkm_n3$Nombre)
datos_rkm_n3$Nombre <- datos_rkm_n3$Nombre %>% capitalize()

datos_rkm_n3$Nombre_split <- parte_cadena_vec(datos_rkm_n3$Nombre)

datos_rkm_n3_conteo <- datos_rkm_n3 %>%
    group_by(Clave) %>%
    summarise(NoVeces = n())

datos_rkm_n3_prom <- datos_rkm_n3 %>%
    group_by(Clave) %>%
    summarise(PromRank = mean(Rank))


cuentas_rkm_n3_mas_frecuentes_y_top <- inner_join(datos_rkm_n3_conteo, datos_rkm_n3_prom)

cuentas_rkm_n3_mas_frecuentes_y_top <- cuentas_rkm_n3_mas_frecuentes_y_top %>%
    arrange(desc(NoVeces), PromRank) %>%
    slice(1:10)

# Escribimos
# cuentas_rkm_n3_mas_frecuentes_y_top %>%
#     write_csv(paste0(ruta_escritura, "Cuentas_Mas_Importantes_RMK_N3_SinCol.csv"),
#               col_names = T)


datos_rkm_n3 %>%
    filter(Clave %in% cuentas_rkm_n3_mas_frecuentes_y_top$Clave) %>%
    ggplot(aes(x = Fecha, y = Importancia, group = Nombre_split, colour = Nombre_split)) +
    geom_line(size = 1) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_text(size = 15, face = "bold"),
          axis.text.x = element_text(angle = 90),
          strip.text = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 18, face = "bold")) +
    facet_wrap(~Nombre_split) +
    ggtitle(label = "Cuentas mas importantes para el ajuste RKM_N3_SinCol",
            subtitle = "Decaimiento promedio en la precision") +
    NULL
