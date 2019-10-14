# ==============================================================================
# ==============================================================================
# ==============================================================================
# ========== Codigo para crear diagramas alluviales de los clusters ============
# ========== obtenidos con los modelos de bloques estocasticos      ============
# ==============================================================================
# ==============================================================================
# ==============================================================================


# Bibliotecas
library(dplyr)
library(ggplot2)
library(alluvial)
library(tidyr)
library(data.table)
library(magrittr)
library(stringi)
library(pbapply)
library(lubridate)

# Directorio
rutaPrin <- "/home/luis/Desktop/StochasticBlockModels/"

# Directorio donde están los datos
rutaDatos <- paste0(rutaPrin, "DatosSalida/RData/")
# Nos posicionamos en la carpeta de datos para tener acceso a los nombres sin
# tener que completar la ruta.
setwd(rutaDatos)

# Obtenemos una lista de los archivos
archivos <- list.files(rutaDatos, pattern = "*.RData")

# Necesitamos obtener los datos de los grupos a los que pertenece cada intitucion
# en cada fecha, metemo en un ciclo.

# Creamos un data frame que contendrá todas las agrupaciones.
agrupaciones <- data.frame()
for (i in 1:length(archivos)) {
  load(archivos[i])
  
  # Obtenemos la fecha
  fecha <- stri_sub(archivos[i], 11, 20)
  
  temp <- listaFinal$Agrupacion
  
  # Agregamos la fecha
  temp$Fecha <- fecha
  
  # Re acomodamos
  temp <- temp %>%
    select(Fecha, Institucion, Nombre_Corto, NoGroup)
  
  # Pegamos al data frame con toda la informacion
  agrupaciones <- rbindlist(list(agrupaciones, temp))
  
  # eliminamos lo que ya no necesitamos
  rm(fecha, temp, listaFinal)
}

# Damos una forma diferente para poder graficar el alluvial.
agrupaciones1 <- agrupaciones %>%
  select(Fecha, Institucion, NoGroup)

agrupaciones1 <- agrupaciones1 %>%
  spread(key = Fecha, value = NoGroup)

# Alluvial
agrupaciones1$Freq <- 1
alluvial(agrupaciones1[, 2:(dim(agrupaciones1)[2] - 1)],
         freq = agrupaciones1$Freq, alpha = 1)



# Es necesario re-etiquetar los grupos.
agrupaciones_group <- agrupaciones %>%
  group_by(Institucion) %>%
  summarize(PromGrupo = mean(NoGroup, na.rm = T), VarGroup = sd(NoGroup, na.rm = T)) %>%
  mutate(PmasV = PromGrupo + VarGroup, PmenosV = PromGrupo- VarGroup) %>%
  mutate(PtoMedio = (PmasV + PmenosV / 2))

agrupaciones_group1 <- agrupaciones_group %>%
  spread(Fecha, PromGrupo)


# ==================== Intento 2 ===========================
agrupaciones2 <- agrupaciones %>%
  group_by(Institucion) %>%
  summarize(PromGrupo = mean(NoGroup, na.rm = T),
            SDGroup = sd(NoGroup, na.rm = T))

agrupaciones2 <- left_join(agrupaciones, agrupaciones2, by = "Institucion")

agrupaciones2 <- agrupaciones2 %>%
  mutate(PromMenosSD = PromGrupo - SDGroup, PromMasSD = PromGrupo + SDGroup)

agrupaciones2 <- agrupaciones2 %>% 
  mutate(Grupo = if_else(condition = NoGroup >= PromMenosSD & NoGroup <= PromMasSD,
                         true = PromGrupo, false = NoGroup))

agrupaciones2$Grupo <- agrupaciones2$Grupo %>% round(digits = 0)

agrupaciones2 <- agrupaciones2 %>%
  select(Fecha, Nombre_Corto, Grupo)

agrupaciones2 <- agrupaciones2 %>%
  spread(Fecha, Grupo)

# Hacemos de nuevo el diagrama alluvial
agrupaciones2$Freq <- 1
alluvial(agrupaciones2[, 2:(dim(agrupaciones2)[2] - 1)],
         freq = agrupaciones2$Freq, alpha = 1)




# ============================ Análisis Estructural ============================
