source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_visualizacion.R"))
source(here::here("funciones", "funciones_estadistica.R"))

# Importacion de scipt anterior
archivos_en_data()
tabla_limpia <- cargar_datos(nombre_archivo = "tabla_limpia.csv", carpeta = "clean")

datos_viejos<-cargar_datos(nombre_archivo = "tabla_primer_limpieza.csv", carpeta = "processed")#para comparacion
#====================================#
#      TABLA DE ANALISIS            #
#====================================#
# ANALISIS GENERAL
col_resumir<-c("deuda_pbi", "deficit_pbi", "formacion_bruta_capital", "inflacion", "consumo_gobierno")
medidas_estaditicas<-resumen_estadistico(data =tabla_limpia, 
                                         vector_col_a_resumir = col_resumir)#datos nuevos
medidas_estaditicas_anterior<-resumen_estadistico(data =datos_viejos, 
                                                  vector_col_a_resumir = col_resumir)#para comparar

medidas_estaditicas%>% kable(format = "html", caption=" Resumen Estadístico Con Tabla Limpia")%>%
  kable_styling(full_width = FALSE)#cuadro de los nuevos datos

restar_tablas_relativas(medidas_estaditicas,medidas_estaditicas_anterior)%>% kable(format = "html", caption="Cambio en variables")%>%
  kable_styling(full_width = FALSE)
#GRAFICOS DE DENSIDAD
graficar_densidad(tabla_limpia, variable = deuda_pbi) #SE PUEDEN COMPRAR CON EL ANTERIOR
graficar_densidad(tabla_limpia, variable = deficit_pbi)#SE PUEDEN COMPRAR CON EL ANTERIOR
graficar_densidad(tabla_limpia, variable = inflacion)#SE PUEDEN COMPRAR CON EL ANTERIOR
graficar_densidad(tabla_limpia, variable = formacion_bruta_capital)#SE PUEDEN COMPRAR CON EL ANTERIOR

# ANALISIS GRUPO
analisis_estadistico <- tabla_limpia %>% 
  group_by(income, year) %>% 
  summarise(
    cantida_paises = n_distinct(country),
    mediana = median(crecimiento_pbi),
    media = mean(crecimiento_pbi),
    desvio = sd(crecimiento_pbi),
    iqr = IQR(crecimiento_pbi),
    minimo = min(crecimiento_pbi),
    maximo = max(crecimiento_pbi),
    percentil_5 = quantile(crecimiento_pbi, probs = 0.05),
    cuartil_1 = quantile(crecimiento_pbi, probs = 0.25),
    cuartil_3 = quantile(crecimiento_pbi, probs = 0.75),
    percentil_95 = quantile(crecimiento_pbi, probs = 0.95),
  ) %>% 
  ungroup() %>% 
  mutate(income = factor(income, levels = c("High income", 
                                            "Upper middle income", 
                                            "Lower middle income", 
                                            "Low income")))
# ANALISIS CONTEO DE PAISES

datos_conteo <- tabla_limpia %>%
  mutate(income = factor(income, levels = c("High income", 
                                            "Upper middle income", 
                                            "Lower middle income", 
                                            "Low income"))) %>%
  group_by(income, year) %>% 
  summarise(
    cantidad_paises = n_distinct(country)
  ) %>% 
  ungroup()

totales_por_anio <- datos_conteo %>%
  group_by(year) %>%
  summarise(total = sum(cantidad_paises))

# ANALISIS DE MEDIA y MEDIANA

metricas_por_anio <- tabla_limpia %>%
  group_by(year) %>%
  summarise(
    media   = mean(crecimiento_pbi, na.rm = TRUE),
    mediana = median(crecimiento_pbi, na.rm = TRUE)
  )

#====================================
#             GRAFICOS              #
#====================================

# GRAFICO CONTEO de PAISES

graf_conteo <- ggplot(datos_conteo, aes(x = factor(year), y = cantidad_paises, fill = income)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(aes(label = cantidad_paises), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 5) +
  geom_label(data = totales_por_anio,
             aes(x = factor(year), y = total, label = total),
             inherit.aes = FALSE,
             fill = "white",     # Color de fondo del cuadrito
             color = "black",    # Color del número
             size = 5, 
             fontface = "bold",
             vjust = -0.25,          # 0 = Apoyado justo sobre la línea final de la barra
             label.size = 0.5    # Grosor del borde del cuadrito (0 = sin borde)
  ) +
  paleta_ingresos() +
  labs(title = "Composición de la Muestra por Nivel de Ingreso",
       subtitle = "Cantidad de países en cada categoría por año",
       caption = "Elaboración propia segun Banco Mundial y Datos Macro",
       x = "",
       y = "Cantidad de Países",
       fill = "Ingreso") +
  tema_proyecto()

guardar_grafico(graf_conteo, "conteo_crecimiento")
  
# GRAFICO de MEDIANA y MEDIA

# MEDIANA

graf_mediana <- ggplot(analisis_estadistico, aes(x = income, y = mediana, fill = income)) +
  geom_col(alpha = 0.8, color = "black") +
  facet_wrap(~year) +
  geom_text(aes(label = round(mediana, 1)), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  paleta_ingresos() +
  labs(title = "Evolución de la Mediana del Crecimiento",
       subtitle = "Comparación por nivel de ingreso",
       caption = "Elaboración propia segun Banco Mundial y Datos Macro",
       y = "Crecimiento",
       x = "",        
       fill = "Nivel de Ingreso") + 
  tema_proyecto() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

guardar_grafico(graf_mediana, "mediana_crecimiento")

# MEDIA

graf_media <- ggplot(analisis_estadistico, aes(x = income, y = media, fill = income)) +
  geom_col(alpha = 0.8, color = "black") +
  facet_wrap(~year) +
  geom_text(aes(label = round(media, 1)), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  paleta_ingresos() +
  labs(title = "Evolución del Crecimiento Promedio (Media)",
       subtitle = "Comparación por nivel de ingreso",
       caption = "Elaboración propia segun Banco Mundial y Datos Macro",
       y = "Crecimiento",
       x = "",        
       fill = "Nivel de Ingreso") + 
 tema_proyecto() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

guardar_grafico(graf_media, "media_crecimiento")

# GRAFICO de HISTOGRAMA

graf_histograma <- ggplot(tabla_limpia, aes(x = crecimiento_pbi)) +
  # Histograma
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 25, 
                 fill = "#5D6D7E", 
                 color = "white", 
                 alpha = 0.6) +
  
  # Distribución
  geom_density(color = "#1A5276",  
               fill = "#AED6F1",   
               alpha = 0.3,        
               size = 1) +
  
  # Lineas de media y mediana
  geom_vline(data = metricas_por_anio, 
             aes(xintercept = media), 
             color = "#2E86C1",  
             linetype = "dashed", 
             size = 1) +
  
  geom_vline(data = metricas_por_anio, 
             aes(xintercept = mediana), 
             color = "#E74C3C",  
             linetype = "solid", 
             size = 1) +
  
  facet_wrap(~year , ncol = 1) +
  
  labs(
    title = "Distribucion del Crecimiento",
    subtitle = "Rojo (Sólida) = Mediana | Azul (Punteada) = Media",
    caption = "Elaboración propia segun Banco Mundial y Datos Macro",
    x = "Crecimiento",
    y = "Densidad"
  ) +
  tema_proyecto()

print(graf_histograma)

guardar_grafico(graf_histograma, "histrograma_crecimiento")



