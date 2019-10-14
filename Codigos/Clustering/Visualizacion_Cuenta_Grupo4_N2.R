library(tidyverse)
library(gghighlight)
library(ggrepel)

parte_cadena <- function(cadena, largo = 12) {
    return(paste(strwrap(cadena, width = largo), collapse = "\n"))
}

parte_cadena_vec <- Vectorize(parte_cadena)



ruta_datos <- "C:/Users/K15523/Desktop/Tesis/Datos_Definitivos/Clusters/Datos_Diferenciar_Grupo4_RKM_N2.csv"


datos <- ruta_datos %>%
    read_csv(col_names = T) %>%
    filter(Clave %in% c(410100000000, 420100000000,
                        211100000000, 221400000000,
                        230200000000, 241300000000,
                        130100000000, 120100000000,
                        240900000000, 140900000000))

datos$Clave <- factor(datos$Clave, levels = c(410100000000, 420100000000,
                                              211100000000, 221400000000,
                                              230200000000, 241300000000,
                                              130100000000, 120100000000,
                                              240900000000, 140900000000),
                      ordered = T)

datos <- datos %>% arrange(Clave)
datos$Rank <- seq(1:10)


datos$Concepto <- c("CAP CONTRIBUIDO ??? CAP SOCIAL (4)",
                    "CAP GANADO ??? RESERVAS DE CAP (4)",
                    "CAPTACION TRADICIONAL ??? DEPOSITOS A PLAZO (2)",
                    "OPE CON VAL ??? DERIVADOS (2)",
                    "PRES INTERBAN ??? DE CORTO PLAZO (2)",
                    "OTR CUEN POR PAG ??? ACREEDORES POR COLATERALES RECIBIDOS EN EFECTIVO (2)",
                    "CARTERA VIG ??? CREDITOS COMERCIALES (1)",
                    "INV EN VALORES ??? TITULOS PARA NEGOCIAR (1)",
                    "OTR CUEN POR PAG ??? ACREEDORES POR LIQUIDACION DE OPERACIONES (2)",
                    "OTR CUEN POR COB ??? DEUDORES POR LIQUIDACION DE OPERACIONES (1)")

# datos$Concepto <- datos$Concepto %>% factor(levels = c("CAP CONTRIBUIDO ??? CAP SOCIAL (4)",
#                                                        "CAP GANADO ??? RESERVAS DE CAP (4)",
#                                                        "CAPTACION TRADICIONAL ??? DEPOSITOS A PLAZO (2)",
#                                                        "OPE CON VAL ??? DERIVADOS (2)",
#                                                        "PRES INTERBAN ??? DE CORTO PLAZO (2)",
#                                                        "OTR CUEN POR PAG ??? ACREEDORES POR COLATERALES RECIBIDOS EN EFECTIVO (2)",
#                                                        "CARTERA VIG ??? CREDITOS COMERCIALES (1)",
#                                                        "INV EN VALORES ??? TITULOS PARA NEGOCIAR (1)",
#                                                        "OTR CUEN POR PAG ??? ACREEDORES POR LIQUIDACION DE OPERACIONES (2)",
#                                                        "OTR CUEN POR COB ??? DEUDORES POR LIQUIDACION DE OPERACIONES (1)"),
#                                             ordered = T)

datos$Concepto <- datos$Concepto %>% parte_cadena_vec(largo = 15)


datos_tidy <- datos %>%
    gather(key = Institucion, value = Valor, -Concepto, -Clave, -Rank)

datos_tidy <- datos_tidy %>%
    mutate(Nombre2 = case_when(Institucion == "40139" ~ "B 8",
                               Institucion == "40147" ~ "B 39",
                               TRUE ~ Institucion))

datos_tidy <- datos_tidy %>%
    select(Concepto, Valor, Nombre2, Rank)


datos_tidy %>%
    ggplot(aes(x = Valor, y = reorder(Concepto, -Rank), colour = Nombre2)) +
    geom_point(size = 2, alpha = 0.75) +
    gghighlight(Nombre2 %in% c("B 8", "B 39"), label_key = Nombre2, use_group_by = F) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_text(size = 10, face = "bold"),
          plot.title = element_text(size = 16, face = "bold")) +
    ggtitle(label = "Valor escalado de las cuentas mas importantes, N2")

