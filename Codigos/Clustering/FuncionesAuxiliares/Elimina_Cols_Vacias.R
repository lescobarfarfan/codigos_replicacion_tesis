
# Funcion auxiliar para eliminar variables con ceros ----------------------

elimina_cols_vacias <- function(df) {
  
  # Convertimos el data frame en matriz
  matriz_t <- t(as.matrix(df))
  
  # Obtenemos la suma por variable
  sumaCols <- rowSums(matriz_t)
  
  # Pegamos a la matriz
  matriz_t <- cbind(matriz_t, sumaCols)
  
  # Transformamos en data frame
  df_new <- tbl_df(as.data.frame(matriz_t))
  
  # Obtenemos los rengloes en los que la suma es cero
  ceros <- which(df_new$sumaCols == 0)
  
  # Quitamos de los nombres
  nom <- rownames(df_new)[-ceros]
  
  # Quitamos ceros
  df_new <- df_new[-ceros, ]
  
  # Ponemos nombres de nuevo
  rownames(df_new) <- nom
  
  # Quitamos la columna de suma
  df_new <- df_new %>% select(-sumaCols)
  
  df_new <- as.data.frame(t(as.matrix(df_new)))

  # Regresamos
  return(df_new)
}
