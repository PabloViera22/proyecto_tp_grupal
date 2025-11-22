source(here::here("config", "parametros.R"))

indicadores <- c("deuda_gob" = "GC.DOD.TOTL.GD.ZS", 
                 "crecimiento_pbi" = "NY.GDP.MKTP.KD.ZG", 
                 "recaudacion" = "GC.TAX.TOTL.GD.ZS", 
                 "ipc" = "FP.CPI.TOTL.ZG",
                 "pbi_constante" = "NY.GDP.MKTP.KD",
                 "region",
                 "poblacion_total"="SP.POP.TOTL")

# Lista de Países por continente
africa <- c("NER", "ETH", "COD", "KEN", "GHA", "ZAF", "MUS")
america <- c("HTI", "BOL", "HND", "PER", "MEX", "CHL", "URY", "USA", "CAN")
asia <- c("AFG", "NPL", "BGD", "IND", "VNM", "PHL", "CHN", "KOR", "JPN", "SGP")
europa <- c("UKR", "MDA", "ALB", "SRB", "ROU", "DEU", "FRA", "ESP", "ITA", "SWE", "DNK")
oceania <- c("FJI", "AUS", "NZL")

# lista total de paises

paises <- c(africa, america, asia, europa, oceania)

datos <- WDI(country = "all", indicator = indicadores,
            start = 2015, 
            end = 2024)
datos

# Exportar los datos a RAW y hacerlos csv

ruta_completa<- file.path(dir_data_raw, "csv_paises")


# meta datos
meta <- WDI_data$country
meta_continente<-meta%>%select("iso3c","region", "income")

# data + meta(continente)
datos_con_continente <- datos %>%
  left_join(meta_continente, by = "iso3c")

#exportar csv
write.csv(
  x = datos_con_continente,          # Tu data.frame a exportar
  file = ruta_completa,      # La ruta completa del archivo
  row.names = FALSE,         # Evita incluir los números de fila como una columna
  fileEncoding = "UTF-8"     # Codificación para manejar acentos y caracteres especiales
)

# Redondeo de todas las variables

datos_dos_digitos <- datos %>%
  mutate(across(c(deuda_gob, crecimiento_pbi, recaudacion, ipc, pbi_constante, pbi_corriente), 
                round, digits = 2))





