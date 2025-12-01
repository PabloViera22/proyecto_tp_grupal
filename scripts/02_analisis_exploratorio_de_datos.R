source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_estadistica.R"))
source(here::here("funciones", "funciones_analisis_na.R"))
# IMPORTAR DEL SCRIP ANTERIOR
archivos_en_data()

analisis_exploratorio<-cargar_datos(nombre_archivo = "tabla_primer_limpieza.csv", carpeta = "processed")
analisis_datos_wdi<-cargar_datos(nombre_archivo = "datos_importados_wdi.csv", carpeta = "raw")
analizar_na_general(tabla = analisis_exploratorio) 
#==============================================================================#
# ANALISIS NA DE DATOS DE WDI
na_de_wdi<-analizar_na_general(analisis_datos_wdi) 
na_de_wdi%>%kable(format = "html", caption="NA en tabla de WDI")%>%
  kable_styling(full_width = FALSE) 

#==============================================================================#
#ANALISIS POR GRUPO
medidas_estaditicas_grupo<-resumen_estadistico_grupo(data =analisis_exploratorio, 
                                         vector_col_a_resumir = c("deuda_pbi", "deficit_pbi"),
                                         grupo = "income")
medidas_estaditicas_grupo%>% kable(format = "html", caption="Resumen Estadístico Por Grupo")%>%
  kable_styling(full_width = FALSE)

#ANALISIS GENERAL
col_resumir<-c("deuda_pbi", "deficit_pbi", "formacion_bruta_capital", "inflacion", "consumo_gobierno")
medidas_estaditicas<-resumen_estadistico(data =analisis_exploratorio, 
                                               vector_col_a_resumir = col_resumir)
medidas_estaditicas%>% kable(format = "html", caption=" Resumen Estadístico")%>%
  kable_styling(full_width = FALSE)
#GRAFICOS DE DENSIDAD
graficar_densidad(analisis_exploratorio, variable = deuda_pbi)
graficar_densidad(analisis_exploratorio, variable = deficit_pbi)
graficar_densidad(analisis_exploratorio, variable = inflacion)
graficar_densidad(analisis_exploratorio, variable = formacion_bruta_capital)

#==============================================================================#
# CANTIDAD DE DATOS POR INGRESO:
analisis_exploratorio$income%>%table()%>%as.data.frame() %>% dplyr::arrange(desc(Freq))%>%
  kable(format = "html", caption="Conteo de Datos",col.names = c("ingreso", "frecuencia"))%>%
  kable_styling(full_width = FALSE)
nrow(analisis_exploratorio)

# MEDIA Y MEDIANA DE DEUDA POR AÑO
analisis_exploratorio%>%group_by(year)%>%
  summarise(media=mean(deuda_pbi),mediana= median(deuda_pbi))%>%
  kable(format = "html", caption=" MEDIA Y MEDIANA DE DEUDA POR AÑO")%>%
  kable_styling(full_width = FALSE)
# MEDIA Y MEDIANA DE DEUDA POR INGRESO
analisis_exploratorio%>%group_by(income)%>%
  summarise(media=mean(deuda_pbi),mediana= median(deuda_pbi))%>%
  kable(format = "html", caption=" MEDIA Y MEDIANA DE DEUDA POR INGRESO")%>%
  kable_styling(full_width = FALSE)

# VARIANZA Y DESVIACION ESTANDAR POR AÑO
deuda_dispersion_por_año <- analisis_exploratorio %>%group_by(year)%>%
  summarise(desv_estandar_año = sd(deuda_pbi, na.rm = TRUE), varianza_año = var(deuda_pbi, na.rm = TRUE))%>%
  kable(format = "html", caption=" VARIANZA Y DESVIACION ESTANDAR POR AÑO")%>%
  kable_styling(full_width = FALSE)
print(deuda_dispersion_por_año)
# VARIANZA Y DESVIACION ESTANDAR POR INGRESO
deuda_dispersion_por_ingreso <- analisis_exploratorio %>%group_by(income)%>%
  summarise(desv_estandar_ingreso = sd(deuda_pbi, na.rm = TRUE), varianza_ingreso = var(deuda_pbi, na.rm = TRUE))%>%
  kable(format = "html", caption=" Tabla Resumen")%>%
  kable_styling(full_width = FALSE)
print(deuda_dispersion_por_ingreso)
#==============================================================================#
# BLOXPOT DE COLUMNAS NUMÉRICAS.
df_long <- analisis_exploratorio %>%
  pivot_longer(cols = where(is.numeric),      # todas las columnas numéricas
               names_to = "variable",
               values_to = "valor")%>% dplyr::filter(!variable %in% c("year", "poblacion"))

inspeccion_bloxpot<-ggplot(df_long, aes(x = valor)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),        # sacar título eje Y
    axis.text.y  = element_blank(),        # sacar etiquetas eje Y
    axis.ticks.y = element_blank(),        # sacar ticks eje Y
    strip.text = element_text(size = 12, face = "bold")
  )
print(inspeccion_bloxpot)






