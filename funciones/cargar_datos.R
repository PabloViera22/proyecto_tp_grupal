source(here::here("config", "parametros.R"))

indicadores <- c("deuda_gob" = "GC.DOD.TOTL.GD.ZS", 
                 "crecimiento_pbi" = "NY.GDP.MKTP.KD.ZG", 
                 "recaudacion" = "GC.TAX.TOTL.GD.ZS", 
                 "ipc" = "FP.CPI.TOTL.ZG",
                 "pbi_constante" = "NY.GDP.MKTP.KD")

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

write.csv(
  x = datos,          # Tu data.frame a exportar
  file = ruta_completa,      # La ruta completa del archivo
  row.names = FALSE,         # Evita incluir los números de fila como una columna
  fileEncoding = "UTF-8"     # Codificación para manejar acentos y caracteres especiales
)

# PBI expresado a miles de millones

datos$pbi_constante <- datos$pbi_constante / 1e9
datos$pbi_corriente <- datos$pbi_corriente / 1e9

# Redondeo de todas las variables

datos_dos_digitos <- datos %>%
  mutate(across(c(deuda_gob, crecimiento_pbi, recaudacion, ipc, pbi_constante, pbi_corriente), 
                round, digits = 2))
















