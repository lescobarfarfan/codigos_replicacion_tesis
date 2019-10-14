# ==============================================================================
# ==============================================================================
# ==============================================================================
# ============== Funci?n para ajuste clusters con biblioteca clustrd ===========
# ==============================================================================
# ==============================================================================
# ==============================================================================

obten_cluster <- function(datos, no_clus = 3:10, dime = 2:3, metodo = 'FKM',
                          centrar = F, escalar = F, no_inicial = 1000,
                          alpha = NULL, datos_FULL = "full") {
    # Library comes with a function that facilitates the number of cluster and
    # dimensions to choose for our specific data.

    # Para hacer m?s robusta la funci?n, agregamos un manejo de errores en el
    # ajuste.
    tryCatch({
        clust_chossing <- tuneclus(datos, nclusrange = no_clus,
                                   ndimrange = dime, alpha = alpha, scale = escalar,
                                   center = centrar, method = metodo, nstart = no_inicial,
                                   dst = datos_FULL)

        # Observamos los resultados ?ptimos
        clust_chossing$clusobjbest
        nclust_optimo <- clust_chossing$nclusbest
        ndim_optimo <- clust_chossing$ndimbest
        clust_chossing$critbest
        clust_chossing$critgrid





        # De acuerdo a la funci?n inicial, el mejor modelo para nuestros datos es de
        # 5 clusters para agrupar a los bancos, y 3 dimensi?nes en el espacio reducido.
        # La funci?n incluso devuelve un objeto con el ajuste ?ptimo.
        ajuste_optimo <- clust_chossing$clusobjbest


        rkm_clus_optimo <- as.data.frame(ajuste_optimo$cluster)

        # vemos los primeros registros del data frame que acabamos de crear.
        # head(rkm_clus_optimo)

        # Vemos que el nombre del banco no forma parte del data frame, si no m?s bien
        # est? como nombre de los renglones, lo extraemos y guardamos en el data frame
        # para tenerlo como una variable m?s. En lo siguiente, usamos el operador $ no
        # para acceder a un elemento de un objeto, si no para crear una nueva columna en
        # el data frame, en este caso, la variable Pais.
        rkm_clus_optimo$Institucion <- row.names(rkm_clus_optimo)

        # vemos de nuevo el inicio
        # head(rkm_clus_optimo)

        # Eliminamos los nombres de los paises para que dejen de ser el nombre de cada
        # rengl?n.
        row.names(rkm_clus_optimo) <- NULL

        # Checamos una vez m?s el inicio para notar los cambios
        # head(rkm_clus_optimo)

        # NO me gusta el orden de las colmnas, ni los nombres, reordenamos y renombramos
        # las columnas.
        names(rkm_clus_optimo) <- c('Cluster', 'Institucion') # renombramos
        rkm_clus_optimo <- select(rkm_clus_optimo, Institucion, Cluster) # Reordenamos.
        # head(rkm_clus_optimo)

        # Ahora, nos interesa ver los clusters en los que quedaron divididos los pa?ses.
        # Primero, obtenemos las corrdenadas del espacio transformado (no hagas caso de
        # esto, es parte del ajuste, s?lo checa c?mo extraer cosas de un objeto) para
        # poder gr?ficar. Lo covertimos de igual forma en data frame
        rkm_coords_optimo <- as.data.frame(ajuste_optimo$obscoord)
        # head(rkm_coords_optimo)
        Institucion <- row.names(rkm_coords_optimo)
        rkm_coords_optimo <- cbind(Institucion, rkm_coords_optimo)
        # Una vez m?s, los paises son nombres de renglones, y queremos que sean una variable
        # m?s, adem?s, los nombres de las columnas est?n feos, repetimos lo anterior.
        row.names(rkm_coords_optimo) <- NULL

        # Renombramos las columnas de las coordenadas, dado que el n?mero de columnas
        # depende de la dimensi?n, necesitamos renombrar de acuerdo a eso.
        # if (ndim_optimo == 2) {
            ejes <- LETTERS[1:ndim_optimo]
            nomb_dimension <- paste0('Coord', ejes)
        # } else {
        #     ejes <- c('X', 'Y', 'Z')
        #     nomb_dimension <- paste0('Coord', ejes)
        # }

        names(rkm_coords_optimo)[2:length(names(rkm_coords_optimo))] <- nomb_dimension


        # Ahora, para gr?ficar los clusters, necesitamos agregar a qu? cluster pertenece
        # cada pa?s a nuestro data frame rkm_coords, hacemos un join con el data frame
        # rkm_clus
        rkm_full_optimo <- left_join(rkm_coords_optimo, rkm_clus_optimo, by = 'Institucion')

        salida <- list(Ajuste = rkm_full_optimo,
                       Salida_obj = clust_chossing$clusobjbest)


        # Retornamos el data frame obtenido.
        return(salida)

        # Mostramos mensaje de que la funci?n ha terminado.
        print('Ajuste terminado')
    }, error = function(c) return(NULL))

}