#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# Codigo para obtener graficas de las cuentas de balance por grupo --------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------



# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(ggridges)
library(gganimate)
library(readxl)
library(gghighlight)

# Directorios -------------------------------------------------------------

rutaPrin <- "/Ruta/Usuario/"
setwd(rutaPrin)


rutaDatos_AjustesCluster <- "/Ruta/Usuario/CriteriosTodosAjustes_RKM.xlsx"




#   -----------------------------------------------------------------------
# Analisis y visualizaciones de resultados --------------------------------
#   -----------------------------------------------------------------------



# Criterios de ajustes de clusters ----------------------------------------

# TRUE - TRUE
datos_TT <- rutaDatos_AjustesCluster %>%
    read_excel(sheet = "SP_T_SF_T",
               col_names = T, col_types = c("date", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric"))
datos_TT$Fecha <- datos_TT$Fecha %>% as.Date()


# TRUE - FALSE
datos_TF <- rutaDatos_AjustesCluster %>%
    read_excel(sheet = "SP_T_SF_F",
               col_names = T, col_types = c("date", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric"))
datos_TF$Fecha <- datos_TF$Fecha %>% as.Date()


# FALSE - TRUE
datos_FT <- rutaDatos_AjustesCluster %>%
    read_excel(sheet = "SP_F_SF_T",
               col_names = T, col_types = c("date", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric"))
datos_FT$Fecha <- datos_FT$Fecha %>% as.Date()


# FALSE - FALSE
datos_FF <- rutaDatos_AjustesCluster %>%
    read_excel(sheet = "SP_F_SF_F",
               col_names = T, col_types = c("date", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric"))
datos_FF$Fecha <- datos_FF$Fecha %>% as.Date()


# Pegamos todos en un solo data frame
datos_CritAju <- list(datos_FF, datos_FT, datos_TF, datos_TT) %>% bind_cols()
datos_CritAju <- datos_CritAju %>% select(-Fecha1, -Fecha2, -Fecha3)


# Convertimos para graficar
datos_CritAju <- datos_CritAju %>%
    gather(key = "Ajuste", value = "Valor", -Fecha)

# Es conveniente separar por niveles de granularidad en las cuentas

# N1
critN1 <- datos_CritAju %>%
    filter(grepl(pattern = "N1", x = Ajuste, ignore.case = F)) %>%
    ggplot(aes(x = Fecha, y = Valor, colour = Ajuste)) +
    geom_line(size = 0.75) +
    facet_wrap(~Ajuste, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          strip.text = element_text(face = "bold", size = 10),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(size = 12)) +
    ggtitle(label = "Criterio de ajuste", subtitle = "Primer nivel de granularidad")
critN1 %>% ggsave(filename = paste0(rutaPrin, "CriterioAjuste_N1.eps"),
                  device = "eps", dpi = 300)


# N2
critN2 <- datos_CritAju %>%
    filter(grepl(pattern = "N2", x = Ajuste, ignore.case = F)) %>%
    ggplot(aes(x = Fecha, y = Valor, colour = Ajuste)) +
    geom_line(size = 0.75) +
    facet_wrap(~Ajuste, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          strip.text = element_text(face = "bold", size = 10),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(size = 12)) +
    ggtitle(label = "Criterio de ajuste", subtitle = "Segundo nivel de granularidad")
critN2 %>% ggsave(filename = paste0(rutaPrin, "CriterioAjuste_N2.eps"),
                  device = "eps", dpi = 300)



# N3
critN3 <- datos_CritAju %>%
    filter(grepl(pattern = "N3", x = Ajuste, ignore.case = F)) %>%
    ggplot(aes(x = Fecha, y = Valor, colour = Ajuste)) +
    geom_line(size = 0.75) +
    facet_wrap(~Ajuste, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          strip.text = element_text(face = "bold", size = 10),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(size = 12)) +
    ggtitle(label = "Criterio de ajuste", subtitle = "Tercer nivel de granularidad")
critN3 %>% ggsave(filename = paste0(rutaPrin, "CriterioAjuste_N3.eps"),
                  device = "eps", dpi = 300)
