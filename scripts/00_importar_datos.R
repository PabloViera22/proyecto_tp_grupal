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


# Datos de deuda y deficit

deuda_deficit <- read.csv2("https://raw.githubusercontent.com/PabloViera22/proyecto_tp_grupal/refs/heads/main/data/raw/deuda_deficit.csv?token=GHSAT0AAAAAADPYWERSDZWS573ZUAV7COX22JGDD5A")

deuda_deficit <- deuda_deficit %>%
  mutate(iso3c = countrycode(sourcevar = paises,
                             origin = "country.name",
                             destination = "iso3c")) %>%
  relocate(iso3c)













