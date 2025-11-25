source(here::here("config", "parametros.R"))

deuda_deficit <- read.csv2("https://raw.githubusercontent.com/PabloViera22/proyecto_tp_grupal/refs/heads/main/data/raw/deuda_deficit.csv?token=GHSAT0AAAAAADPYWERSTLWQCMCGWQO263GW2JGDX6A")

deuda_deficit <- deuda_deficit %>%
  mutate(iso3c = countrycode(sourcevar = paises,
                             origin = "country.name",
                             destination = "iso3c")) %>%
  relocate(iso3c)

write_csv2(deuda_deficit, "data/processed/deuda_deficit_iso3.csv")
