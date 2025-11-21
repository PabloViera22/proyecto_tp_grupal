# Coneccion github
install.packages("usethis")
library(usethis)
use_git_config(user.name = "geroosilva", user.email = "silvageronimofelipe2002@gmail.com")
create_github_token()

library(gitcreds)
gitcreds_set()

git_sitrep()

# Librerias 

library(WDI)
library(tidyverse)

# Los indicadores están correctos
indicadores <- c("deuda_gob" = "GC.DOD.TOTL.GD.ZS",
                 "crecimiento_pbi" = "NY.GDP.MKTP.KD.ZG",
                 "recaudacion" = "GC.TAX.TOTL.GD.ZS",
                 "ipc" = "FP.CPI.TOTL.ZG",
                 "pbi_constante" = "NY.GDP.MKTP.KN",
                 "pbi_corriente" = "NY.GDP.MKTP.CD")

paises_ocde <- c(
  "AR",  # Argentina
  "DE", # Alemania
  "AU", # Australia
  "AT", # Austria
  "BE", # Bélgica
  "CA", # Canadá
  "CL", # Chile
  "CO", # Colombia
  "KR", # Corea (República de Corea)
  "CR", # Costa Rica
  "DK", # Dinamarca
  "SK", # Eslovaquia
  "SI", # Eslovenia
  "ES", # España
  "US", # Estados Unidos
  "EE", # Estonia
  "FI", # Finlandia
  "FR", # Francia
  "GR", # Grecia
  "HU", # Hungría
  "IE", # Irlanda
  "IS", # Islandia
  "IL", # Israel
  "IT", # Italia
  "JP", # Japón
  "LV", # Letonia
  "LT", # Lituania
  "LU", # Luxemburgo
  "MX", # México
  "NO", # Noruega
  "NZ", # Nueva Zelanda
  "NL", # Países Bajos
  "PL", # Polonia
  "PT", # Portugal
  "GB", # Reino Unido
  "CZ", # República Checa
  "SE", # Suecia
  "CH", # Suiza
  "TR"  # Turquía
)

options(scipen = 999)

datos <- WDI(country = paises_ocde, 
             indicator = indicadores,
             start = 2019, 
             end = 2023)

# Manipulacion de datos

datos$pbi_constante <- datos$pbi_constante / 1e9
datos$pbi_corriente <- datos$pbi_corriente / 1e9

# Redondeo de datos

datos <- datos %>%
  mutate(across(c(deuda_gob, crecimiento_pbi, recaudacion, ipc, pbi_constante, pbi_corriente), 
                round, digits = 2))


ggplot(data = datos, aes(x = pbi_corriente, y = deuda_gob)) +
  geom_point() 
  

# tabla <- as.tibble(WDIsearch('gdp.*current'))
