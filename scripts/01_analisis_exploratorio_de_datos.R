source(here::here("config", "parametros.R"))
# IMPORTAR DEL SCRIP ANTERIOR
archivos_en_data()

analisis_exploratorio<-read_csv(file.path(proyecto_tp_grupal, "data", 
                                          "processed", "tabla_completa.csv")) 
#==============================================================================#
medidas_estaditicas<-resumen_estadistico(data =analisis_exploratorio, 
                                         vector_col_a_resumir = c("deuda_pbi", "deficit_pbi"),
                                         grupo = "income")
medidas_estaditicas%>% kable(format = "html", caption=" Tabla Resumen")%>%
  kable_styling(full_width = FALSE)
#==============================================================================#
# CANTIDAD DE DATOS POR INGRESO:
datos_conteo_01 <- analisis_exploratorio %>%
  mutate(income = factor(income, levels = c("High income",  "Upper middle income", 
                                            "Lower middle income", "Low income"))) %>%
  group_by(income) %>% 
  summarise(cantidad_paises = n_distinct(country)) %>% 
  ungroup()

# MEDIA Y MEDIANA DE DEUDA POR AÑO
analisis_exploratorio%>%group_by(year)%>%
  summarise(media=mean(deuda_pbi),mediana= median(deuda_pbi))%>%
  kable(format = "html", caption=" Tabla Resumen")%>%
  kable_styling(full_width = FALSE)
# MEDIA Y MEDIANA DE DEUDA POR INGRESO
analisis_exploratorio%>%group_by(income)%>%
  summarise(media=mean(deuda_pbi),mediana= median(deuda_pbi))%>%
  kable(format = "html", caption=" Tabla Resumen")%>%
  kable_styling(full_width = FALSE)

#==============================================================================#
# GRAFICOS







