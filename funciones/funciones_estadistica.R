#==============================================================================#
# ANALISIS GENERAL
#==============================================================================#

resumen_estadistico_grupo <- function(data, 
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
        cantidad_datos = dplyr::n_distinct(.data[[name_col_country]]),
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

resumen_estadistico<- function(data, name_col_country= "country", vector_col_a_resumir) {
  
  # Validación básica
  if (!name_col_country %in% names(data)) {
    stop("La columna de país no existe en el data.")
  }
  
  vector_col_a_resumir <- rlang::syms(vector_col_a_resumir)
  country_col <- rlang::sym(name_col_country)
  
  map_dfr(
    vector_col_a_resumir,
    \(var) {
      data %>%
        summarise(
          variable = as.character(rlang::as_name(var)),
          cantidad_paises = n_distinct(.data[[name_col_country]]),
          mediana = median(.data[[rlang::as_name(var)]], na.rm = TRUE),
          media   = mean(.data[[rlang::as_name(var)]], na.rm = TRUE),
          desvio  = sd(.data[[rlang::as_name(var)]], na.rm = TRUE),
          iqr     = IQR(.data[[rlang::as_name(var)]], na.rm = TRUE),
          minimo  = min(.data[[rlang::as_name(var)]], na.rm = TRUE),
          maximo  = max(.data[[rlang::as_name(var)]], na.rm = TRUE)
        )
    }
  )
}

#==============================================================================#
# CURVA DENSIDAD

graficar_densidad <- function(tabla, variable) {
  # Validación
  if (!is.data.frame(tabla)) {stop("El objeto `tabla` debe ser un data frame.")}
    var <- enquo(variable)
  
  ggplot(tabla, aes(x = !!var)) +
    geom_density(fill = "skyblue", alpha = 0.5) +
    labs(
      title = paste("Densidad de", rlang::as_name(var)),
      x = "Valor",
      y = "Densidad"
    ) +
    theme_minimal()
}


#==============================================================================#
# COMPARAR DOS TABLAS IGUALES

restar_tablas_relativas <- function(tablanueva, tablavieja) {
  
  # Validaciones
  if (!is.data.frame(tablanueva) || !is.data.frame(tablavieja)) {
    stop("Ambos objetos deben ser data frames.")
  }
  
  if (!all(names(tablanueva) == names(tablavieja))) {
    stop("Las dos tablas deben tener exactamente los mismos nombres de columnas.")
  }
  
  if (nrow(tablanueva) != nrow(tablavieja)) {
    stop("Las dos tablas deben tener el mismo número de filas.")
  }
  
  # Identificar columnas numéricas
  cols_num <- names(tablanueva)[sapply(tablanueva, is.numeric)]
  
  # Cálculo relativo para columnas numéricas
  tabla_resultado <- tablanueva
  tabla_resultado[cols_num] <- (tablanueva[cols_num] - tablavieja[cols_num]) / tablavieja[cols_num]
  
  return(tabla_resultado)
}




