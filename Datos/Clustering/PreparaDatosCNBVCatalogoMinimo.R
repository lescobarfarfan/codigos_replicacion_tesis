#   -----------------------------------------------------------------------
# Script para preparar datos publicos CNBV catalogo minimo ----------------
#   -----------------------------------------------------------------------

library(tidyverse)
options(scipen = 999)



# 2016 --------------------------------------------------------------------



datos16 <- read_csv("/Ruta/Usuario/CatalogoMinimoR01_40_2016.csv",
                    locale = locale(encoding = "ISO-8859-2"))


datos16 <- datos16 %>%
    group_by(Periodo, Serie, Reporte, Institución, Nombre, `Clave Concepto`,
             `Descripción Concepto`) %>%
    summarize(Saldo = sum(Saldo, na.rm = T)) %>%
    ungroup()

datos16 <- datos16 %>%
    filter(!(Institución %in% c("040997", "040999")))


ada <- datos16 %>% filter(Periodo == 201603) %>%
    filter(`Clave Concepto` == 100000000000)


datos162 <- datos16 %>%
    filter(Periodo %in% c(201603, 201606, 201609, 201612)) %>%
    select(Periodo, `Descripción Concepto`, `Clave Concepto`, Institución, Saldo) %>%
    spread(key = Institución, value = Saldo, fill = 0)

datos162 <- datos162 %>%
    arrange(Periodo, `Clave Concepto`)

datos162 <- datos162 %>%
    filter(str_sub(`Clave Concepto`, 1, 1) %in% c("1", "2", "4"))


datos162 <- datos162 %>%
    mutate(Categoria = str_sub(`Clave Concepto`, 1, 1),
           Nivel1 = str_sub(`Clave Concepto`, 2, 3),
           Nivel2 = str_sub(`Clave Concepto`, 4, 5),
           Nivel3 = str_sub(`Clave Concepto`, 6, 7),
           Nivel4 = str_sub(`Clave Concepto`, 8, 9)) %>%
    select(Periodo, `Descripción Concepto`, `Clave Concepto`, Categoria,
           Nivel1, Nivel2, Nivel3, Nivel4, starts_with(match = "040"))


datos162 <- datos162 %>%
    rename(Fecha = Periodo,
           Concepto = `Descripción Concepto`,
           Clave = `Clave Concepto`)




# 2017 --------------------------------------------------------------------



datos17 <- read_csv("/Ruta/Usuario/CatalogoMinimoR01_40_2017.csv",
                    locale = locale(encoding = "ISO-8859-2"))


datos17 <- datos17 %>%
    group_by(Periodo, Serie, Reporte, Institución, Nombre, `Clave Concepto`,
             `Descripción Concepto`) %>%
    summarize(Saldo = sum(Saldo, na.rm = T)) %>%
    ungroup()

datos17 <- datos17 %>%
    filter(!(Institución %in% c("040997", "040999")))


ada <- datos17 %>% filter(Periodo == 201703) %>%
    filter(`Clave Concepto` == 100000000000)


datos172 <- datos17 %>%
    filter(Periodo %in% c(201703, 201706, 201709, 201712)) %>%
    select(Periodo, `Descripción Concepto`, `Clave Concepto`, Institución, Saldo) %>%
    spread(key = Institución, value = Saldo, fill = 0)

datos172 <- datos172 %>%
    arrange(Periodo, `Clave Concepto`)

datos172 <- datos172 %>%
    filter(str_sub(`Clave Concepto`, 1, 1) %in% c("1", "2", "4"))


datos172 <- datos172 %>%
    mutate(Categoria = str_sub(`Clave Concepto`, 1, 1),
           Nivel1 = str_sub(`Clave Concepto`, 2, 3),
           Nivel2 = str_sub(`Clave Concepto`, 4, 5),
           Nivel3 = str_sub(`Clave Concepto`, 6, 7),
           Nivel4 = str_sub(`Clave Concepto`, 8, 9)) %>%
    select(Periodo, `Descripción Concepto`, `Clave Concepto`, Categoria,
           Nivel1, Nivel2, Nivel3, Nivel4, starts_with(match = "040"))


datos172 <- datos172 %>%
    rename(Fecha = Periodo,
           Concepto = `Descripción Concepto`,
           Clave = `Clave Concepto`)



datos_totales <- data.table::rbindlist(l = list(datos162, datos172), fill = T)


write_csv(x = datos_totales, path = "/Ruta/Usuario/Totales.csv", col_names = T)






# Datos Escalados Segun Farne et. al. -------------------------------------


# 2016 --------------------------------------------------------------------



datos16 <- read_csv("/Ruta/Usuario/CatalogoMinimoR01_40_2016.csv",
                    locale = locale(encoding = "ISO-8859-2"))


datos16 <- datos16 %>%
    group_by(Periodo, Serie, Reporte, Institución, Nombre, `Clave Concepto`,
             `Descripción Concepto`) %>%
    summarize(Saldo = sum(Saldo, na.rm = T)) %>%
    ungroup()

datos16 <- datos16 %>%
    filter(!(Institución %in% c("040997", "040999")))


datos162 <- datos16 %>%
    filter(Periodo %in% c(201603, 201606, 201609, 201612)) %>%
    select(Periodo, `Descripción Concepto`, `Clave Concepto`, Institución, Saldo) %>%
    spread(key = Institución, value = Saldo, fill = 0)

datos162 <- datos162 %>%
    arrange(Periodo, `Clave Concepto`)

datos162 <- datos162 %>%
    filter(str_sub(`Clave Concepto`, 1, 1) %in% c("1", "2", "4"))


datos162 <- datos162 %>%
    mutate(Categoria = str_sub(`Clave Concepto`, 1, 1),
           Nivel1 = str_sub(`Clave Concepto`, 2, 3),
           Nivel2 = str_sub(`Clave Concepto`, 4, 5),
           Nivel3 = str_sub(`Clave Concepto`, 6, 7),
           Nivel4 = str_sub(`Clave Concepto`, 8, 9)) %>%
    select(Periodo, `Descripción Concepto`, `Clave Concepto`, Categoria,
           Nivel1, Nivel2, Nivel3, Nivel4, starts_with(match = "040"))


datos162 <- datos162 %>%
    rename(Fecha = Periodo,
           Concepto = `Descripción Concepto`,
           Clave = `Clave Concepto`)


aux16_max <- datos162 %>%
    select(Fecha, Clave, starts_with(match = "040"))

aux16_max <- aux16_max %>%
    gather(key = Institucion, value = Saldo, -Fecha, -Clave)

aux16_max <- aux16_max %>%
    filter(Clave %in% c(100000000000, 200000000000, 400000000000)) %>%
    group_by(Fecha, Clave) %>%
    summarize(MaxMonto = max(Saldo, na.rm = T))

aux16_max <- aux16_max %>%
    mutate(Indicador = str_sub(Clave, 1, 1))

aux16_max <- aux16_max %>%
    select(Fecha, Indicador, MaxMonto)


aux16 <- datos162 %>%
    filter(Clave %in% c(100000000000, 200000000000, 400000000000))

aux16 <- aux16 %>%
    mutate(Indicador = str_sub(Clave, 1, 1)) %>%
    gather(key = Institucion, value = Monto_Inst, -Fecha, -Concepto, -Clave,
           -Categoria, -Nivel1, -Nivel2, -Nivel3, -Nivel4, -Indicador)

aux16 <- aux16 %>%
    select(Fecha, Indicador, Institucion, Monto_Inst)


datos162 <- datos162 %>%
    mutate(Indicador = str_sub(Clave, 1, 1)) %>%
    gather(key = Institucion, value = Monto, -Fecha, -Concepto, -Clave, -Categoria, -Nivel1, -Nivel2, -Nivel3, -Nivel4, -Indicador)


datos162 <- datos162 %>%
    left_join(aux16_max, by = c("Fecha", "Indicador"))

datos162 <- datos162 %>%
    left_join(aux16, by = c("Fecha", "Institucion", "Indicador"))



datos162 <- datos162 %>%
    mutate(Monto_Escalado = case_when(Clave %in% c(100000000000,
                                                   200000000000,
                                                   400000000000) ~ Monto/MaxMonto,
                                      TRUE ~ Monto/Monto_Inst))

datos162 <- datos162 %>%
    select(-Indicador, -Monto, -MaxMonto, -Monto_Inst) %>%
    spread(key = Institucion, value = Monto_Escalado)




# 2017 --------------------------------------------------------------------



datos17 <- read_csv("/Ruta/Usuario/CatalogoMinimoR01_40_2017.csv",
                    locale = locale(encoding = "ISO-8859-2"))


datos17 <- datos17 %>%
    group_by(Periodo, Serie, Reporte, Institución, Nombre, `Clave Concepto`,
             `Descripción Concepto`) %>%
    summarize(Saldo = sum(Saldo, na.rm = T)) %>%
    ungroup()

datos17 <- datos17 %>%
    filter(!(Institución %in% c("040997", "040999")))


ada <- datos17 %>% filter(Periodo == 201703) %>%
    filter(`Clave Concepto` == 100000000000)


datos172 <- datos17 %>%
    filter(Periodo %in% c(201703, 201706, 201709, 201712)) %>%
    select(Periodo, `Descripción Concepto`, `Clave Concepto`, Institución, Saldo) %>%
    spread(key = Institución, value = Saldo, fill = 0)

datos172 <- datos172 %>%
    arrange(Periodo, `Clave Concepto`)

datos172 <- datos172 %>%
    filter(str_sub(`Clave Concepto`, 1, 1) %in% c("1", "2", "4"))


datos172 <- datos172 %>%
    mutate(Categoria = str_sub(`Clave Concepto`, 1, 1),
           Nivel1 = str_sub(`Clave Concepto`, 2, 3),
           Nivel2 = str_sub(`Clave Concepto`, 4, 5),
           Nivel3 = str_sub(`Clave Concepto`, 6, 7),
           Nivel4 = str_sub(`Clave Concepto`, 8, 9)) %>%
    select(Periodo, `Descripción Concepto`, `Clave Concepto`, Categoria,
           Nivel1, Nivel2, Nivel3, Nivel4, starts_with(match = "040"))


datos172 <- datos172 %>%
    rename(Fecha = Periodo,
           Concepto = `Descripción Concepto`,
           Clave = `Clave Concepto`)


aux17_max <- datos172 %>%
    select(Fecha, Clave, starts_with(match = "040"))

aux17_max <- aux17_max %>%
    gather(key = Institucion, value = Saldo, -Fecha, -Clave)

aux17_max <- aux17_max %>%
    filter(Clave %in% c(100000000000, 200000000000, 400000000000)) %>%
    group_by(Fecha, Clave) %>%
    summarize(MaxMonto = max(Saldo, na.rm = T))

aux17_max <- aux17_max %>%
    mutate(Indicador = str_sub(Clave, 1, 1))

aux17_max <- aux17_max %>%
    select(Fecha, Indicador, MaxMonto)


aux17 <- datos172 %>%
    filter(Clave %in% c(100000000000, 200000000000, 400000000000))

aux17 <- aux17 %>%
    mutate(Indicador = str_sub(Clave, 1, 1)) %>%
    gather(key = Institucion, value = Monto_Inst, -Fecha, -Concepto, -Clave,
           -Categoria, -Nivel1, -Nivel2, -Nivel3, -Nivel4, -Indicador)

aux17 <- aux17 %>%
    select(Fecha, Indicador, Institucion, Monto_Inst)


datos172 <- datos172 %>%
    mutate(Indicador = str_sub(Clave, 1, 1)) %>%
    gather(key = Institucion, value = Monto, -Fecha, -Concepto, -Clave, -Categoria, -Nivel1, -Nivel2, -Nivel3, -Nivel4, -Indicador)


datos172 <- datos172 %>%
    left_join(aux17_max, by = c("Fecha", "Indicador"))

datos172 <- datos172 %>%
    left_join(aux17, by = c("Fecha", "Institucion", "Indicador"))



datos172 <- datos172 %>%
    mutate(Monto_Escalado = case_when(Clave %in% c(100000000000,
                                                   200000000000,
                                                   400000000000) ~ Monto/MaxMonto,
                                      TRUE ~ Monto/Monto_Inst))

datos172 <- datos172 %>%
    select(-Indicador, -Monto, -MaxMonto, -Monto_Inst) %>%
    spread(key = Institucion, value = Monto_Escalado)






# Consolidamos fechas -----------------------------------------------------


datos_totales <- data.table::rbindlist(l = list(datos162, datos172), fill = T)


write_csv(x = datos_totales, path = "/Ruta/Usuario/Totales_Scaled.csv", col_names = T)
