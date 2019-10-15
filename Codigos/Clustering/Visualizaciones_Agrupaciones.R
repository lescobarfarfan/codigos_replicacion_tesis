#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# Codigo para graficar visualizaciones de las agrupaciones ----------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------




# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(scatterplot3d)
library(randomcoloR)
library(RColorBrewer)
library(ggrepel)


# Directorios -------------------------------------------------------------

# aux_fkm_1 <- "/media/luis/LUISESCOBAR/Tesis_Datos/Clusters_FKM_RKM/"
# aux_fkm_2 <- "DatosDefinitivos/SoloBalance_DatosNotFull/FKM/"
# aux_fkm_3 <- "ScaledPaperTRUE_ScaledFunctionFALSE_CenterFALSE/"
# aux_fkm_4 <- "AjustesN1_ScaledPaperTRUE_ScaledFuncFALSE_Total_FKM_0.75_Datos-low.RData"



# Ruta al mapeo con nombres anonimos
ruta_nombres_anon <- "/Ruta/Usuario/Mapeo_claves_bancos_con_nombre_anonimo.csv"

# Para cada ajuste por metodo/nivel/Con-SinCorrelacionadas obtendremos
# una visualizacion solo de la ultima fecha.

# Cargamos mapeo
mapeo <- ruta_nombres_anon %>%
    read_csv(col_names = T)
mapeo <- rename(mapeo, Institucion = Clave)
mapeo$Institucion <- as.character(mapeo$Institucion)

# Ajustes con FKM ---------------------------------------------------------

# FKM N1
load("/Ruta/Usuario/AjustesN1_ScaledPaperTRUE_ScaledFuncFALSE_Total_FKM_0.75_Datos-low.RData")

Ajuste <- list_ajustesN1[[length(list_ajustesN1)]][[1]] %>% tbl_df()

# Graficamos en 3D
colores <- randomColor(count = length(unique(Ajuste$Cluster)),
                       luminosity = "bright")
colores_full <- data.frame(Cluster = sort(unique(Ajuste$Cluster)), Color = colores)
Ajuste <- Ajuste %>% inner_join(colores_full)

sp3d_FKM_N1 <- scatterplot3d(Ajuste[, 2:4], pch = 16, color = Ajuste$Color,
                             axis = T, x.ticklabs = NA, y.ticklabs = NA,
                             z.ticklabs = NA, xlab = NA, ylab = NA, zlab = NA)
legend("bottom", legend = levels(factor(Ajuste$Cluster)),
       col = colores, pch = 16, inset = -0.25, xpd = T, horiz = T)

rm(list = setdiff(ls(), "mapeo"))

# FKM N1 Sin Colineales
load("/Ruta/Usuario/AjustesN1_SinCol_ScaledPaperTRUE_ScaledFuncFALSE_Total_FKM_0.75_Datos-low.RData")

Ajuste <- list_ajustesN1_SinCol[[length(list_ajustesN1_SinCol)]][[1]] %>% tbl_df()

# Graficamos en 3D
colores <- randomColor(count = length(unique(Ajuste$Cluster)),
                       luminosity = "bright")
colores_full <- data.frame(Cluster = sort(unique(Ajuste$Cluster)), Color = colores)
Ajuste <- Ajuste %>% inner_join(colores_full)

sp3d_FKM_N1 <- scatterplot3d(Ajuste[, 2:4], pch = 16, color = Ajuste$Color,
                             axis = T, x.ticklabs = NA, y.ticklabs = NA,
                             z.ticklabs = NA, xlab = NA, ylab = NA, zlab = NA)
legend("bottom", legend = levels(factor(Ajuste$Cluster)),
       col = colores, pch = 16, inset = -0.25, xpd = T, horiz = T)


rm(list = setdiff(ls(), "mapeo"))

# FKM N2
load("/Ruta/Usuario/AjustesN2_ScaledPaperTRUE_ScaledFuncFALSE_Total_FKM_0.75_Datos-low.RData")

Ajuste <- list_ajustesN2[[length(list_ajustesN2)]][[1]] %>% tbl_df()

# Graficamos en 3D
colores <- randomColor(count = length(unique(Ajuste$Cluster)),
                       luminosity = "bright")
colores_full <- data.frame(Cluster = sort(unique(Ajuste$Cluster)), Color = colores)
Ajuste <- Ajuste %>% inner_join(colores_full)

sp3d_FKM_N1 <- scatterplot3d(Ajuste[, 2:4], pch = 16, color = Ajuste$Color,
                             axis = T, x.ticklabs = NA, y.ticklabs = NA,
                             z.ticklabs = NA, xlab = NA, ylab = NA, zlab = NA)
legend("bottom", legend = levels(factor(Ajuste$Cluster)),
       col = colores, pch = 16, inset = -0.25, xpd = T, horiz = T)


rm(list = setdiff(ls(), "mapeo"))

# FKM N2 Sin Col
load("/Ruta/Usuario/AjustesN2_SinCol_ScaledPaperTRUE_ScaledFuncFALSE_Total_FKM_0.75_Datos-low.RData")

Ajuste <- list_ajustesN2_SinCol[[length(list_ajustesN2_SinCol)]][[1]] %>% tbl_df()

# Graficamos en 3D
# colores <- randomColor(count = length(unique(Ajuste$Cluster)),
#                        luminosity = "bright")
# colores_full <- data.frame(Cluster = sort(unique(Ajuste$Cluster)), Color = colores)
# Ajuste <- Ajuste %>% inner_join(colores_full)
#
# sp3d_FKM_N1 <- scatterplot3d(Ajuste[, 2:4], pch = 16, color = Ajuste$Color,
#                              axis = T, x.ticklabs = NA, y.ticklabs = NA,
#                              z.ticklabs = NA, xlab = NA, ylab = NA, zlab = NA)
# legend("bottom", legend = levels(factor(Ajuste$Cluster)),
#        col = colores, pch = 16, inset = -0.25, xpd = T, horiz = T)

Ajuste <- Ajuste %>% inner_join(mapeo)

Ajuste %>%
    ggplot(aes(x = CoordA, y = CoordB, colour = factor(Cluster))) +
    geom_point(size = 5) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = "bottom",
          legend.title = element_blank()) +
    geom_label_repel(aes(label = Nombre), show.legend = F)


rm(list = setdiff(ls(), "mapeo"))

# FKM N3
load("/Ruta/Usuario/AjustesN3_ScaledPaperTRUE_ScaledFuncFALSE_Total_FKM_0.75_Datos-low.RData")

Ajuste <- list_ajustesN3[[length(list_ajustesN3)]][[1]] %>% tbl_df()

# Graficamos en 3D
colores <- randomColor(count = length(unique(Ajuste$Cluster)),
                       luminosity = "bright")
colores_full <- data.frame(Cluster = sort(unique(Ajuste$Cluster)), Color = colores)
Ajuste <- Ajuste %>% inner_join(colores_full)

sp3d_FKM_N1 <- scatterplot3d(Ajuste[, 2:4], pch = 16, color = Ajuste$Color,
                             axis = T, x.ticklabs = NA, y.ticklabs = NA,
                             z.ticklabs = NA, xlab = NA, ylab = NA, zlab = NA)
legend("bottom", legend = levels(factor(Ajuste$Cluster)),
       col = colores, pch = 16, inset = -0.25, xpd = T, horiz = T)

# Ajuste %>%
#     ggplot(aes(x = CoordA, y = CoordB, colour = factor(Cluster))) +
#     geom_point(size = 5) +
#     theme_bw() +
#     theme(axis.title = element_blank(),
#           axis.text = element_blank(),
#           legend.text = element_text(size = 18),
#           legend.position = "bottom",
#           legend.title = element_blank())

rm(list = setdiff(ls(), "mapeo"))




# FKM N3 SinCol
load("/Ruta/Usuario/AjustesN3_SinCol_ScaledPaperTRUE_ScaledFuncFALSE_Total_FKM_0.75_Datos-low.RData")

Ajuste <- list_ajustesN3_SinCol[[length(list_ajustesN3_SinCol)]][[1]] %>% tbl_df()

# Graficamos en 3D
# colores <- randomColor(count = length(unique(Ajuste$Cluster)),
#                        luminosity = "bright")
# colores_full <- data.frame(Cluster = sort(unique(Ajuste$Cluster)), Color = colores)
# Ajuste <- Ajuste %>% inner_join(colores_full)
#
# sp3d_FKM_N1 <- scatterplot3d(Ajuste[, 2:4], pch = 16, color = Ajuste$Color,
#                              axis = T, x.ticklabs = NA, y.ticklabs = NA,
#                              z.ticklabs = NA, xlab = NA, ylab = NA, zlab = NA)
# legend("bottom", legend = levels(factor(Ajuste$Cluster)),
#        col = colores, pch = 16, inset = -0.25, xpd = T, horiz = T)

Ajuste <- Ajuste %>% inner_join(mapeo)

Ajuste %>%
    ggplot(aes(x = CoordA, y = CoordB, colour = factor(Cluster))) +
    geom_point(size = 5) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = "bottom",
          legend.title = element_blank()) +
    geom_label_repel(aes(label = Nombre), show.legend = F)




rm(list = setdiff(ls(), "mapeo"))


# Ajustes con RKM ---------------------------------------------------------


# RKM N1
load("/Ruta/Usuario/AjustesN1_ScaledPaperTRUE_ScaledFuncFALSE_Total_RKM_0.75_Datos-low.RData")

Ajuste <- list_ajustesN1[[length(list_ajustesN1)]][[1]] %>% tbl_df()

# Graficamos en 3D
# colores <- randomColor(count = length(unique(Ajuste$Cluster)),
#                        luminosity = "bright")
# colores_full <- data.frame(Cluster = sort(unique(Ajuste$Cluster)), Color = colores)
# Ajuste <- Ajuste %>% inner_join(colores_full)
#
# sp3d_FKM_N1 <- scatterplot3d(Ajuste[, 2:4], pch = 16, color = Ajuste$Color,
#                              axis = T, x.ticklabs = NA, y.ticklabs = NA,
#                              z.ticklabs = NA, xlab = NA, ylab = NA, zlab = NA)
# legend("bottom", legend = levels(factor(Ajuste$Cluster)),
#        col = colores, pch = 16, inset = -0.25, xpd = T, horiz = T)

Ajuste <- Ajuste %>% inner_join(mapeo)


Ajuste %>%
    ggplot(aes(x = CoordA, y = CoordB, colour = factor(Cluster))) +
    geom_point(size = 5) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = "bottom",
          legend.title = element_blank()) +
    geom_label_repel(aes(label = Nombre), show.legend = F)


rm(list = setdiff(ls(), "mapeo"))

# FKM N1 Sin Colineales
load("/Ruta/Usuario/AjustesN1_SinCol_ScaledPaperTRUE_ScaledFuncFALSE_Total_RKM_0.75_Datos-low.RData")

Ajuste <- list_ajustesN1_SinCol[[length(list_ajustesN1_SinCol)]][[1]] %>% tbl_df()

# Graficamos en 3D
# colores <- randomColor(count = length(unique(Ajuste$Cluster)),
#                        luminosity = "bright")
# colores_full <- data.frame(Cluster = sort(unique(Ajuste$Cluster)), Color = colores)
# Ajuste <- Ajuste %>% inner_join(colores_full)
#
# sp3d_FKM_N1 <- scatterplot3d(Ajuste[, 2:4], pch = 16, color = Ajuste$Color,
#                              axis = T, x.ticklabs = NA, y.ticklabs = NA,
#                              z.ticklabs = NA, xlab = NA, ylab = NA, zlab = NA)
# legend("bottom", legend = levels(factor(Ajuste$Cluster)),
#        col = colores, pch = 16, inset = -0.25, xpd = T, horiz = T)

Ajuste <- Ajuste %>% inner_join(mapeo)

Ajuste %>%
    ggplot(aes(x = CoordA, y = CoordB, colour = factor(Cluster))) +
    geom_point(size = 5) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = "bottom",
          legend.title = element_blank()) +
    geom_label_repel(aes(label = Nombre), show.legend = F)



rm(list = setdiff(ls(), "mapeo"))

# FKM N2
load("/Ruta/Usuario/AjustesN2_ScaledPaperTRUE_ScaledFuncFALSE_Total_RKM_0.75_Datos-low.RData")

Ajuste <- list_ajustesN2[[length(list_ajustesN2)]][[1]] %>% tbl_df()

# Graficamos en 3D
# colores <- randomColor(count = length(unique(Ajuste$Cluster)),
#                        luminosity = "bright")
# colores_full <- data.frame(Cluster = sort(unique(Ajuste$Cluster)), Color = colores)
# Ajuste <- Ajuste %>% inner_join(colores_full)
#
# sp3d_FKM_N1 <- scatterplot3d(Ajuste[, 2:4], pch = 16, color = Ajuste$Color,
#                              axis = T, x.ticklabs = NA, y.ticklabs = NA,
#                              z.ticklabs = NA, xlab = NA, ylab = NA, zlab = NA)
# legend("bottom", legend = levels(factor(Ajuste$Cluster)),
#        col = colores, pch = 16, inset = -0.25, xpd = T, horiz = T)

Ajuste <- Ajuste %>% inner_join(mapeo)

Ajuste %>%
    ggplot(aes(x = CoordA, y = CoordB, colour = factor(Cluster))) +
    geom_point(size = 5) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = "bottom",
          legend.title = element_blank()) +
    geom_label_repel(aes(label = Nombre), show.legend = F)



rm(list = setdiff(ls(), "mapeo"))

# FKM N2 Sin Col
load("/Ruta/Usuario/AjustesN2_SinCol_ScaledPaperTRUE_ScaledFuncFALSE_Total_RKM_0.75_Datos-low.RData")

Ajuste <- list_ajustesN2_SinCol[[length(list_ajustesN2_SinCol)]][[1]] %>% tbl_df()

# Graficamos en 3D
# colores <- randomColor(count = length(unique(Ajuste$Cluster)),
#                        luminosity = "bright")
# colores_full <- data.frame(Cluster = sort(unique(Ajuste$Cluster)), Color = colores)
# Ajuste <- Ajuste %>% inner_join(colores_full)
#
# sp3d_FKM_N1 <- scatterplot3d(Ajuste[, 2:4], pch = 16, color = Ajuste$Color,
#                              axis = T, x.ticklabs = NA, y.ticklabs = NA,
#                              z.ticklabs = NA, xlab = NA, ylab = NA, zlab = NA)
# legend("bottom", legend = levels(factor(Ajuste$Cluster)),
#        col = colores, pch = 16, inset = -0.25, xpd = T, horiz = T)

Ajuste <- Ajuste %>% inner_join(mapeo)

Ajuste %>%
    ggplot(aes(x = CoordA, y = CoordB, colour = factor(Cluster))) +
    geom_point(size = 5) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = "bottom",
          legend.title = element_blank()) +
    geom_label_repel(aes(label = Nombre), show.legend = F)


rm(list = setdiff(ls(), "mapeo"))

# FKM N3
load("/Ruta/Usuario/AjustesN3_ScaledPaperTRUE_ScaledFuncFALSE_Total_RKM_0.75_Datos-low.RData")

Ajuste <- list_ajustesN3[[length(list_ajustesN3)]][[1]] %>% tbl_df()

# Graficamos en 3D
# colores <- randomColor(count = length(unique(Ajuste$Cluster)),
#                        luminosity = "bright")
# colores_full <- data.frame(Cluster = sort(unique(Ajuste$Cluster)), Color = colores)
# Ajuste <- Ajuste %>% inner_join(colores_full)
#
# sp3d_FKM_N1 <- scatterplot3d(Ajuste[, 2:4], pch = 16, color = Ajuste$Color,
#                              axis = T, x.ticklabs = NA, y.ticklabs = NA,
#                              z.ticklabs = NA, xlab = NA, ylab = NA, zlab = NA)
# legend("bottom", legend = levels(factor(Ajuste$Cluster)),
#        col = colores, pch = 16, inset = -0.25, xpd = T, horiz = T)

Ajuste <- Ajuste %>% inner_join(mapeo)


Ajuste %>%
    ggplot(aes(x = CoordA, y = CoordB, colour = factor(Cluster))) +
    geom_point(size = 5) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = "bottom",
          legend.title = element_blank()) +
    geom_label_repel(aes(label = Nombre), show.legend = F)

rm(list = setdiff(ls(), "mapeo"))




# FKM N3 SinCol
load("/Ruta/Usuario/AjustesN3_SinCol_ScaledPaperTRUE_ScaledFuncFALSE_Total_RKM_0.75_Datos-low.RData")

Ajuste <- list_ajustesN3_SinCol[[length(list_ajustesN3_SinCol)]][[1]] %>% tbl_df()

# Graficamos en 3D
# colores <- randomColor(count = length(unique(Ajuste$Cluster)),
#                        luminosity = "bright")
# colores_full <- data.frame(Cluster = sort(unique(Ajuste$Cluster)), Color = colores)
# Ajuste <- Ajuste %>% inner_join(colores_full)
#
# sp3d_FKM_N1 <- scatterplot3d(Ajuste[, 2:4], pch = 16, color = Ajuste$Color,
#                              axis = T, x.ticklabs = NA, y.ticklabs = NA,
#                              z.ticklabs = NA, xlab = NA, ylab = NA, zlab = NA)
# legend("bottom", legend = levels(factor(Ajuste$Cluster)),
#        col = colores, pch = 16, inset = -0.25, xpd = T, horiz = T)

Ajuste <- Ajuste %>% inner_join(mapeo)

Ajuste %>%
    ggplot(aes(x = CoordA, y = CoordB, colour = factor(Cluster))) +
    geom_point(size = 5) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = "bottom",
          legend.title = element_blank()) +
    geom_label_repel(aes(label = Nombre), show.legend = F)


rm(list = setdiff(ls(), "mapeo"))
