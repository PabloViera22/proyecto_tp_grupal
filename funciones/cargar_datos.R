source(here::here("config", "parametros.R"))

indicadores<-c("Deuda_del_Gob"="GC.DOD.TOTL.GD.ZS", 
               "Tasa_Crecimiento_PBI"="NY.GDP.MKTP.KD.ZG", 
               "Recaudacion"="GC.TAX.TOTL.GD.ZS", 
               "IPC"="FP.CPI.TOTL.ZG")
# Lista de Países por continente y nivel economico
africa_bajos  <- c("NER","ETH","COD")
africa_medios <- c("KEN","GHA","ZAF")
africa_altos  <- c("MUS")
latam_bajos  <- c("HTI")
latam_medios <- c("BOL","HND","PER","MEX")
latam_altos  <- c("CHL","URY","USA","CAN")
asia_bajos  <- c("AFG","NPL","BGD")
asia_medios <- c("IND","VNM","PHL","CHN")
asia_altos  <- c("KOR","JPN","SGP")
europa_bajos  <- c("UKR")
europa_medios <- c("MDA","ALB","SRB","ROU")
europa_altos  <- c("DEU","FRA","ESP","ITA","SWE","DNK")
oceania_medios <- c("FJI")
oceania_altos  <- c("AUS","NZL")

africa<-c(africa_bajos, africa_medios, africa_altos)
latam<-c(latam_bajos, latam_medios, latam_altos)
asia<-c(asia_bajos, asia_medios, asia_altos)
europa<-c(europa_bajos, europa_medios, europa_altos)
oceania<-c( oceania_medios, oceania_altos)

paises <- c(africa, latam, asia, europa, oceania)


datos<- WDI(country = paises,indicator = indicadores,
            start = 2018, 
            end = 2024)
datos
# El banco mundial carece de informacion sobre la deuda publica la recaudacion y
#sobre la inflación.

countries <- wb_countries()
codigos_paises <- countries[, c("country", "iso2c", "iso3c", "region")]
head(codigos_paises)
paises<-c(chile="CHL", colombia="COL", argentina="ARG", brasil="BRA")
