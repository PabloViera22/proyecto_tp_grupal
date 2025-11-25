source(here::here("config", "parametros.R"))

# Importar los datos del banco mundial
datos_crudos <- WDI(country = "all", indicator = indicadores,
              start = 1990, 
              end = 2024)
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
datos_unidos <- datos_crudos %>%
  left_join(meta_continente, by = "iso3c")
# Exportar a carpeta de procesado
exportar_data(data = datos_unidos,nombre = "datos_wdi_con_meta", carpeta = "processed", format = "csv")


#========================#
# IMPORTACION DE DATOS  
#========================#

# Datos Banco Mundial

datos_wdi <- read.csv("https://raw.githubusercontent.com/PabloViera22/proyecto_tp_grupal/refs/heads/main/data/processed/datos_wdi_con_meta.csv?token=GHSAT0AAAAAADPYWERTJ6POIHZZUA2IKVEY2JGEJCA")

# Datos de deuda y deficit (Datos.macro)

deuda_deficit <- read.csv2("https://raw.githubusercontent.com/PabloViera22/proyecto_tp_grupal/refs/heads/main/data/processed/deuda_deficit_iso3.csv?token=GHSAT0AAAAAADPYWERT6VROQVRF3LIQKBNA2JGEH2Q")

# Join entre las dos tablas

datos <- datos_wdi %>% 
  left_join(
    deuda_deficit,
    by = c("iso3c", "year" = "fecha", "country" = "paises")
  ) %>% 
  rename(anio = year) %>% 
  rename(pais = country) %>% 
  filter(anio %in% c(2017,2020,2023)) %>% 
  select(-c(
    iso2c, 
    deuda_gob))
  










