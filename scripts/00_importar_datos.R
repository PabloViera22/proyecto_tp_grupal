source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))

# Importar los datos del banco mundial
datos_crudos <- WDI(country = "all", indicator = indicadores,
              start = 1990, 
              end = 2024) # Tarda mucho en descargar, mejor no probar
# Exportar los datos a la carpeta raw
exportar_data(data = datos_crudos,nombre = "datos_importados_wdi", carpeta = "raw", format = "csv")

# Importar metadatos del banco mundial (continente y nivel de ingreso)
meta <- WDI_data$country
meta_continente<-meta%>%dplyr::select("iso3c","region", "income")
# Exportar los datos
exportar_data(data = meta_continente,nombre = "datos_meta_wdi", carpeta = "raw", format = "csv")
#==============================================================================#
# Juntar a través de un join ambas columnas
#==============================================================================#
datos_join <- datos_crudos %>%
  left_join(meta_continente, by = "iso3c")
# Exportar a carpeta de procesado
exportar_data(data = datos_join,nombre = "datos_wdi_con_meta", carpeta = "processed", format = "csv")


#==============================================================================#
# IMPORTAR DATOS  
#==============================================================================#

# Datos Banco Mundial
datos_wdi <- cargar_datos(nombre_archivo = "datos_wdi_con_meta.csv", carpeta = "processed")
# Datos de deuda y deficit (Datos.macro)
deuda_deficit <- read.csv2(file.path(proyecto_tp_grupal, "data", "processed", "deuda_deficit_iso3.csv"))

# Join entre las dos tablas
datos_wdi_mas_macro <- datos_wdi %>% 
  left_join(
    deuda_deficit,
    by = c("iso3c", "year" = "fecha", "country" = "paises")
  ) %>%
  rename(anio = year) %>%
  rename(pais = country) %>%
  filter(anio %in% c(2017, 2020, 2023)) %>%
  dplyr::select(-c(iso2c, deuda_gob))
  
# Exporta tabla del join
exportar_data(data = datos_wdi_mas_macro, nombre = tabla_completa, carpeta = "processed")

#==============================================================================#





