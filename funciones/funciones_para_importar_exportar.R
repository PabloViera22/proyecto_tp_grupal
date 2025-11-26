#==============================================================================#
# FUNCION PARA EXPORTAR
#==============================================================================#
exportar_data <- function(data, 
                        nombre, 
                        carpeta = c("raw", "processed", "clean"),
                        format = "csv"
                        ) {
    # Validar argumentos
  folder <- match.arg(carpeta)
  format <- tolower(format)
  
  if (!format %in% c("csv", "excel")) {
    stop("El formato debe ser 'csv' o 'excel'")
  }
  
  if (!is.data.frame(data)) {
    stop("El objeto 'data' debe ser un data frame")
  }
  
  # Determinar extensión y ruta completa
  extension <- ifelse(format == "csv", ".csv", ".xlsx")
  filepath <- file.path("D:/Proyecto_Git_TP_Grupal/proyecto_tp_grupal/data/",carpeta, paste0(nombre, extension))
  
  # Exportar según el formato
  if (format == "csv") {
    write.csv(data, file = filepath, row.names = FALSE, fileEncoding = "UTF-8")
    message(paste("Archivo CSV exportado:", filepath))
  } else {
    # Verificar si writexl está instalado
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("El paquete 'writexl' es necesario para exportar a Excel. Instálalo con: install.packages('writexl')")
    }
    writexl::write_xlsx(data, path = filepath)
    message(paste("Archivo Excel exportado:", filepath))
  }
  
  # Retornar ruta del archivo
  invisible(filepath)
}

#==============================================================================#
# FUNCION QUE DEVUELVE NOMBRE DE ARCHIVOS
#==============================================================================#
archivos_en_data <- function() {
  ruta_base <- "D:/Proyecto_Git_TP_Grupal/proyecto_tp_grupal/data"
  rutas <- list(
    raw = file.path(ruta_base, "raw"),
    processed = file.path(ruta_base, "processed"),
    clean = file.path(ruta_base, "clean")
  )
  # Recolectar archivos por carpeta
  archivos <- lapply(rutas, list.files)
  # Imprimir carpeta + archivos
  nombres<-for (carpeta in names(archivos)) {
    cat("\nCarpeta:", carpeta, "\n")
    if (length(archivos[[carpeta]]) == 0) {
      cat("  (sin archivos)\n")
    } else {
      cat("  -", paste(archivos[[carpeta]], collapse = "\n  - "), "\n")
    }
  }
  invisible(archivos)
  return("D:/Proyecto_Git_TP_Grupal/proyecto_tp_grupal/data")
  return(nombres)
}


