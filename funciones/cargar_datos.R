source(here::here("config", "parametros.R"))

indicadores <- c("deuda_gob" = "GC.DOD.TOTL.GD.ZS", 
                 "crecimiento_pbi" = "NY.GDP.MKTP.KD.ZG", 
                 "recaudacion" = "GC.TAX.TOTL.GD.ZS", 
                 "ipc" = "FP.CPI.TOTL.ZG",
                 "pbi_constante" = "NY.GDP.MKTP.KN",
                 "pbi_corriente" = "NY.GDP.MKTP.CD")

# Lista de Países por continente y nivel economico
africa_bajos  <- c("NER","ETH","COD")
africa_medios <- c("KEN","GHA","ZAF")
africa_altos  <- c("MUS")
latam_bajos  <- c("HTI")
latam_medios <- c("BOL","HND","PER","MEX")
latam_altos  <- c("CHL","URY","USA","CAN")
asia_bajos  <- c("AFG","NPL","BGD")
asia_medios <- c("IND","VNM","PHL","CHN")
asia_altos <- c("KOR","JPN","SGP")
europa_bajos  <- c("UKR")
europa_medios <- c("MDA","ALB","SRB","ROU")
europa_altos  <- c("DEU","FRA","ESP","ITA","SWE","DNK")
oceania_medios <- c("FJI")
oceania_altos  <- c("AUS","NZL")
# lista de paises por continente
africa <- c(africa_bajos, africa_medios, africa_altos)
latam <- c(latam_bajos, latam_medios, latam_altos)
asia <- c(asia_bajos, asia_medios, asia_altos)
europa <- c(europa_bajos, europa_medios, europa_altos)
oceania <- c(oceania_medios, oceania_altos)
# lista total de paises
paises <- c(africa, latam, asia, europa, oceania)

datos <- WDI(country = paises, indicator = indicadores,
            start = 2018, 
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
















