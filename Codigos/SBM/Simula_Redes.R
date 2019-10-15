#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# Script para simular redes -----------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------



library(igraph)


ruta_escritura <- "Escriba la ruta donde quiere guardar las redes:\n" %>% 
  readline()

numero_instituciones <- c(48, 49, 49, 49, 49, 49)


set.seed(123)


for (i in 1:length(numero_instituciones)) {
  red_aux <- random.graph.game(n = numero_instituciones[i], p.or.m = 0.6,
                               type = "gnp", directed = T, loops = F, )
  write_graph(graph = red_aux, file = paste0(ruta_escritura, "Red_", i,
                                             ".graphml"),
              format = "graphml")
  rm(red_aux)
}
