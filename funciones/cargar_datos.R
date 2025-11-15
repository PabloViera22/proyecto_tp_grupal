source(here::here("config", "parametros.R"))

indicadores<-c("Deuda_del_Gob"="GC.DOD.TOTL.GD.ZS", 
               "Tasa_Crecimiento_PBI"="NY.GDP.MKTP.KD.ZG", 
               "Recaudacion"="GC.TAX.TOTL.GD.ZS", 
               "IPC"="FP.CPI.TOTL.ZG")
paises<-c(chile="CHL", colombia="COL", argentina="ARG", brasil="BRA")
paises<-c("CHL","COL","ARG","BRA")

datos<- WDI(country = paises,indicator = indicadores,
            start = 2018, 
            end = 2023)
datos
# El banco mundial carece de informacion sobre la deuda publica la recaudacion y
#sobre la inflación.

countries <- wb_countries()
codigos_paises <- countries[, c("country", "iso2c", "iso3c", "region")]
head(codigos_paises)
paises<-c(chile="CHL", colombia="COL", argentina="ARG", brasil="BRA")
