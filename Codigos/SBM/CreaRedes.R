# ==============================================================================
# ==============================================================================
# ==============================================================================
# =============== Análisis estructural de las redes de exposición ==============
# ==============================================================================
# ==============================================================================
# ==============================================================================


# Bibliotecas
library(igraph)
library(tidyr)
library(dplyr)
library(lubridate)
# library(reticulate)
library(magrittr)
library(readr)
library(stringi)
library(data.table)
library(ggplot2)

# Directorio
rutaPrin <- "/home/luis/Desktop/StochasticBlockModels/"
setwd(rutaPrin)

# Directorio donde están las matrices
rutaMat <- "/home/luis/Desktop/StochasticBlockModels/MatricesSBM/"

# Directorio para guardar los objetos de red graphml
rutaObj_graphml <- "/home/luis/Desktop/StochasticBlockModels/ObjetosRed_graphml/"

# Obtenemos la lista de los archivos con las matrices
files <- list.files(rutaMat, pattern = "*.csv")

# Extraemos las fechas y las convertimos al formato adecuado
fechas <- mapply(FUN = substr, files, MoreArgs = list(1, 6)) %>%
  unname() %>%
  as.factor() %>% 
  ymd()

# Necesitamos crear objetos de red y obtener las principales medidas estructurales
# por fecha.

# Leemos todas las matrices
matrices <- lapply(X = paste0(rutaMat, files), FUN = read_csv, col_names = T) %>%
  lapply(as.matrix)
names(matrices) <- fechas
colnames(matrices[[1]])

# ================== Medidas principales de red ==================

# Creamos las redes
redes <- list()
for (i in 1:length(matrices)) {
  temp <- graph.adjacency(matrices[[i]], mode = "directed",
                          weighted = T)
  redes[[i]] <- temp
  rm(temp)
}
names(redes) <- fechas

# Guardamos los objetos
for (i in 1:length(redes)) {
  red <- redes[[i]]
  
  nombre_obj <- paste0(rutaObj_graphml, names(redes)[i], ".graphml")
  
  # Guardamos
  igraph::write_graph(graph = red, file = nombre_obj, format = "graphml")
  
  rm(red, nombre_obj)
  
}



# =================== Un poco de análisis en R =====================

# De la salida de los SBM, leemos la perteneciad de cada institucion
rutaDatos <- paste0(rutaPrin, "DatosSalida/RData/")
# Nos posicionamos en la carpeta de datos para tener acceso a los nombres sin
# tener que completar la ruta.
setwd(rutaDatos)
# Obtenemos una lista de los archivos
archivos <- list.files(rutaDatos, pattern = "*.RData")

# Extraemos los grupos para cada fecha.
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

# Calculamos el grado para cada red y extraemos la información.
grado <- data.frame() %>% tbl_df()
for (i in 1:length(redes)) {
  temp <- igraph::degree(graph = redes[[i]], normalized = F)
  temp <- temp %>% as.data.frame()
  temp$Institucion <- rownames(temp)
  temp$Fecha <- fechas[i]
  names(temp)[which(names(temp) == ".")] <- "Grado"
  rownames(temp) <- NULL
  temp <- temp %>% select(Fecha, Institucion, Grado)
  grado <- bind_rows(grado, temp)
  rm(temp)
}

# Agregamos el grado a las agrupaciones
agrupaciones$Fecha <- agrupaciones$Fecha %>% ymd()
grado$Institucion <- grado$Institucion %>% as.integer()
agrupaciones <- left_join(agrupaciones, grado, by = c("Fecha", "Institucion"))

agrupaciones <- agrupaciones %>% tbl_df()


# Podemos hacer una gráfica de caja y brazos por fecha para cada grupo
agrupaciones %>%
  ggplot(aes(x = Fecha, y = Grado, group = factor(Fecha))) +
  geom_boxplot(aes(fill = factor(NoGroup))) +
  facet_grid(~NoGroup, switch = "x") +
  theme_bw() +
  scale_fill_discrete(name = "Grupo", breaks = c("1", "2", "3", "4"),
                      labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo4")) +
  theme(strip.placement = "outside") +
  geom_smooth(aes(x = Fecha, y = Grado, group = factor(Fecha), colour = factor(NoGroup)))


# Un scatter para ver cómo se movieron las instituciones de grupo con el tiempo
agrupaciones %>%
  ggplot(aes(x = Fecha, y = NoGroup, group = factor(Institucion))) +
  geom_jitter(aes(colour = factor(Institucion))) +
  geom_smooth()

agrupaciones %>%
  ggplot(aes(x = Fecha, y = NoGroup, group = factor(Institucion))) +
  geom_line(aes(colour = factor(Institucion))) +
  theme_bw() +
  facet_wrap(~Institucion, ncol = 6) +
  geom_smooth(aes(colour = factor(Institucion))) +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  ylab("Grupo")


redPrueba <- redes[[43]]
l = igraph::layout.kamada.kawai(redPrueba, )
plot(redPrueba, layout = l)


gradoPruebaDF <- filter(agrupaciones, Fecha == fechas[43])
gradoPruebaDF <- select(gradoPruebaDF, Institucion, NoGroup)
gradoPrueba <- gradoPruebaDF$NoGroup
names(gradoPrueba) <- gradoPruebaDF$Institucion

redPrueba
V(redPrueba)$Grupo <- gradoPrueba
V(redPrueba)$Grupo

# Guardamos
igraph::write.graph(redPrueba, "RedPrueba.graphml", format = "graphml")
