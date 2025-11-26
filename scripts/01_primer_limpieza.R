archivos_en_data()
tabla_wdi<-read_csv("D:/Proyecto_Git_TP_Grupal/proyecto_tp_grupal/data/processed/datos_wdi_con_meta.csv")
tabla_deficit_deuda<-read_csv2("D:/Proyecto_Git_TP_Grupal/proyecto_tp_grupal/data/raw/deuda_deficit.csv")
datos_meta_wdi<-read_csv("D:/Proyecto_Git_TP_Grupal/proyecto_tp_grupal/data/raw/datos_meta_wdi.csv")

tabla_deficit_deuda_iso3 <- tabla_deficit_deuda %>%dplyr::select(paises, fecha,deuda_pbi, deficit_pbi)%>%
  mutate(iso3c = countrycode(
    sourcevar = paises,            # tu columna con nombres
    origin = "country.name",  # nombres en español
    destination = "iso3c",       # código ISO3
    warn = TRUE                  # avisa si alguno no se mapea
  ))
# 1ro agregar codigo a tabla deficit y deuda
tabla_completa <- tabla_wdi %>% 
  left_join(tabla_deficit_deuda_iso3, by = c("iso3c", "year" = "fecha", "country" = "paises")
  ) %>% 
  filter(year %in% c(2017,2020,2023), ) %>% 
  dplyr::select(-c(iso2c, deuda_gob))

tabla_completa_filtrada<-tabla_completa%>%inner_join(tabla_deficit_deuda_iso3,by = c("iso3c", "year" = "fecha", "country" = "paises"))



nrow(tabla_completa_filtrada)

























