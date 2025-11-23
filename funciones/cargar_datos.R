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

#==============================================================================#
#        HACER OTRO CSV CON DATOS DE PAISES DE EUROPA PARA NUEVO ANALISIS      #
#==============================================================================#
indicadores2 <- c(
  "GC.DOD.TOTL.GD.ZS",   # DEBT (Central government debt)
  "NY.GDP.MKTP.KD.ZG",   # GDP_GROWTH (GDP growth (annual %))
  "NY.GDP.PCAP.KD",      # GDP_P_C (GDP per capita - Constant US$)
  "NE.TRD.GNFS.ZS",      # OPENNESS (Trade (% of GDP))
  "NE.GDI.FTOT.ZS",      # LTOTAL (Inversión - Gross capital formation)
  "NE.CON.GOVT.ZS",      # GOV_EXPEND (Government consumption)
  "FR.INR.RINR",         # Interest rate (Real interest rate)
  "NY.GDP.DEFL.KD.ZG",   # Inflation (GDP deflator (annual %))
  "SP.POP.GROW"          # Population growth
)
paises2 <- c("ALB", "DEU", "AND", "AUT", "BEL", "BLR",
              "BIH", "BGR", "CZE", "CYP", "HRV",
              "DNK", "SVK", "SVN", "ESP", "EST", "FIN",
              "FRA", "GBR", "GRC", "NLD", "HUN", "ITA",
              "IRL", "ISL", "LVA", "LIE", "LTU", "LUX",
              "MKD", "MDA", "MLT", "MCO", "NOR", "POL",
              "PRT", "ROU", "RUS", "SMR", "SRB", "MNE",
              "SWE", "CHE", "UKR", "UZB")

datos2 <- WDI(country = paises2, indicator = indicadores2,
             start = 2000, 
             end = 2024)

# exportar a RAW
ruta_completa2<- file.path(dir_data_raw, "csv_paises2")

datos2_con_ingreso <- datos2 %>%
  left_join(meta_continente, by = "iso3c")

write.csv(
  x = datos2_con_ingreso,          # Tu data.frame a exportar
  file = ruta_completa2,      # La ruta completa del archivo
  row.names = FALSE,         # Evita incluir los números de fila como una columna
  fileEncoding = "UTF-8"     # Codificación para manejar acentos y caracteres especiales
)











