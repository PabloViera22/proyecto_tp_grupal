source(here::here("config", "parametros.R"))

datos_crudos <- WDI(country = "all", indicator = indicadores,
              start = 1990, 
              end = 2024)
write.csv(
  x = datos_crudos,          # Tu data.frame a exportar
  file = file.path(dir_data_raw, "csv_datos3"),      # La ruta completa del archivo
  row.names = FALSE,         # Evita incluir los números de fila como una columna
  fileEncoding = "UTF-8"     # Codificación para manejar acentos y caracteres especiales
)

exportar_datos(datos_crudos, datos_crudos,carpeta_destino = "raw")





