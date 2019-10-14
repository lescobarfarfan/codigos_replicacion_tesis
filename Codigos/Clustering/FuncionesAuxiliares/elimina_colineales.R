# ==============================================================================
# ==============================================================================
# ==============================================================================
# =========== Función para eliminar colineales de acuerdo a Farne et al. =======
# ==============================================================================
# ==============================================================================
# ==============================================================================

elimina_colineales <- function(df, nivelCor = 0.95) {
    # First, we have to obtain correlation matrices of variables.
    
    datos <- df
    
    corre <- cor(datos)
    
    corre <- tbl_df(as.data.frame(corre))
    
    # Obtenemos la suma por renglones de la matriz
    corre <- mutate(corre, Peso = rowSums(abs(corre)) - 1)
    
    variables_para_eliminar <- c()
    for (i in 1:dim(corre)[1]) {
        for (j in 2:dim(corre)[2] - 1) {
            if (abs(corre[i, j]) >= nivelCor & abs(corre[i, j]) < 1) {
                if (corre$Peso[i] > corre$Peso[j]) {
                    variables_para_eliminar <- c(variables_para_eliminar,
                                                 names(corre)[j])
                    # corre <- corre[-j, -j]
                } else {
                    variables_para_eliminar <- c(variables_para_eliminar,
                                                 names(corre)[i])
                    next(i)
                    # corre <- corre[-i, -i]
                }
            } else {
                next(j)
            } 
        }
    }
    variables_para_eliminar <- sort(unique(variables_para_eliminar))
    aux <- 1:dim(datos)[2]
    var_to_delete <- which(names(datos) %in% variables_para_eliminar)
    vars_to_keep <- setdiff(aux, var_to_delete)
    datos_fin <- select(datos, vars_to_keep)
    return(datos_fin)
}