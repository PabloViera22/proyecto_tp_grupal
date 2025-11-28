#==============================================================================#
# ANALISIS GENERAL
#==============================================================================#

resumen_estadistico <- function(data, 
                                vector_col_a_resumir, 
                                grupo, 
                                name_col_country = "country") {
  
  # Validación del argumento 'grupo'
  if (!grupo %in% c("year", "income", "region")) {
    stop("El 'grupo' debe ser 'year', 'income' o 'region'")
  }
  
  # Validación de las columnas a resumir
  if (!all(vector_col_a_resumir %in% names(data))) {
    stop("Alguna columna indicada en vector_col_a_resumir no existe en los datos")
  }
  
  # Para almacenar resultados
  lista_resultados <- list()
  
  for (variable in vector_col_a_resumir) {
    
    resultado <- data %>%
      dplyr::group_by(.data[[grupo]]) %>%
      summarise(
        variable = variable,
        cantidad_paises = dplyr::n_distinct(.data[[name_col_country]]),
        mediana = median(.data[[variable]], na.rm = TRUE),
        media   = mean(.data[[variable]], na.rm = TRUE),
        desvio  = sd(.data[[variable]], na.rm = TRUE),
        iqr     = IQR(.data[[variable]], na.rm = TRUE),
        minimo  = min(.data[[variable]], na.rm = TRUE),
        maximo  = max(.data[[variable]], na.rm = TRUE)
      )
    
    lista_resultados[[variable]] <- resultado
  }
  
  # Combinar todos los resúmenes en un solo data frame
  return(dplyr::bind_rows(lista_resultados))
}
















