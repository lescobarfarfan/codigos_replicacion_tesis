#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# Codigos para graficas de criterio de ajuste -----------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------



# Biblioteca --------------------------------------------------------------

library(tidyverse)
library(tidyquant)
library(directlabels)
library(gghighlight)
library(scales)

options(scipen = 999)

# Directorios -------------------------------------------------------------

# Ruta a los criterios de ajustes con FKM
ruta_datos_fkm <- "/Ruta/Del/Usuario/FKM_CriteriosAjuste.csv"

# Ruta a los criterios de ajustes con RKM
ruta_datos_rkm <- "/Ruta/Del/Usuario/RKM_CriteriosAjuste.csv"





# Procesamiento de datos --------------------------------------------------

datos_fkm <- ruta_datos_fkm %>% 
  read_csv(col_names = T)


datos_rkm <- ruta_datos_rkm %>%
  read_csv(col_names = T)


# pegamos en un solo data frame
datos <- datos_fkm %>% left_join(datos_rkm, by = "Fecha")

# Convertimos a un formato adecuado para graficar los datos
datos_gather <- datos %>%
  gather(key = Ajuste, value = Criterio, -Fecha)

datos_gather <- datos_gather %>%
  mutate(Metodo = substr(Ajuste, 1, 3)) %>%
  mutate(Nivel = substr(Ajuste, 5, length(Ajuste)))


# Ajustes con FKM
datos_gather %>%
  filter(Metodo == "FKM") %>%
  ggplot(aes(x = Fecha, y = Criterio, colour = Ajuste)) +
  geom_line() +
  facet_wrap(~Ajuste, scales = "free_y", ncol = 2) +
  theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size = 15),
        axis.title = element_blank(),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = scales::scientific_format(digits = 1)) +
  expand_limits(y = 0) +
  ggtitle(label = "Criterios de ajuste para el método de FKM") +
  NULL




datos_gather %>%
  filter(Metodo == "RKM") %>%
  ggplot(aes(x = Fecha, y = Criterio, colour = Ajuste)) +
  geom_line() +
  facet_wrap(~Ajuste, scales = "free_y", ncol = 2) +
  theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size = 15),
        axis.title = element_blank(),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(angle = 90)) +
  expand_limits(y = 0) +
  ggtitle(label = "Criterios de ajuste para el método de RKM") +
  NULL




