# Agregar funciones o parametros, no se bien como se hace. Eliminar cuando ya este

library(ggrepel) # Agregar esta libreria a los nuevos parametros

# Importacion de scipt anterior

# IMPORTANTE!!
# Agregar tabla con el nuevo link cuando el repositorio sea publico (borrar comentario cuando se haga)
tabla_imputar <- read.csv("https://raw.githubusercontent.com/PabloViera22/proyecto_tp_grupal/refs/heads/main/data/processed/tabla_para_imputacion.csv?token=GHSAT0AAAAAADPYWERTFRGRCBLFFQ5CM7HK2JJE2BQ") 

#====================================
#      TABLA DE ANALISIS            #
#====================================

# ANALISIS GENERAL

analisis_estadistico <- tabla_imputar %>% 
  group_by(income, year) %>% 
  summarise(
    cantida_paises = n_distinct(country),
    mediana = median(deuda_pbi),
    media = mean(deuda_pbi),
    desvio = sd(deuda_pbi),
    iqr = IQR(deuda_pbi),
    minimo = min(deuda_pbi),
    maximo = max(deuda_pbi)
  ) %>% 
  ungroup() %>% 
  mutate(income = factor(income, levels = c("High income", 
                                            "Upper middle income", 
                                            "Lower middle income", 
                                            "Low income")))
# ANALISIS CONTEO DE PAISES

datos_conteo <- tabla_imputar %>%
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

metricas_por_anio <- tabla_imputar %>%
  group_by(year) %>%
  summarise(
    media   = mean(deuda_pbi, na.rm = TRUE),
    mediana = median(deuda_pbi, na.rm = TRUE)
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
  scale_fill_manual(values = c(
    "High income"         = "#2E8B57",
    "Upper middle income" = "#9ACD32",
    "Lower middle income" = "#FFA500",
    "Low income"          = "#CD5C5C"
  )) +
  labs(title = "Composición de la Muestra por Nivel de Ingreso",
       subtitle = "Cantidad de países en cada categoría por año",
       caption = "Elaboración propia segun Banco Mundial y Datos Macro",
       x = "",
       y = "Cantidad de Países",
       fill = "Ingreso") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0))
  
# GRAFICO de MEDIANA y MEDIA

graf_mediana <- ggplot(analisis_estadistico, aes(x = income, y = mediana, fill = income)) +
  geom_col(alpha = 0.8, color = "black") +
  facet_wrap(~year) +
  geom_text(aes(label = round(mediana, 1)), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c(
    "High income"         = "#2E8B57",
    "Upper middle income" = "#9ACD32",
    "Lower middle income" = "#FFA500",
    "Low income"          = "#CD5C5C"
  )) +
  labs(title = "Evolución de la Deuda Mediana",
       subtitle = "Comparación por nivel de ingreso",
       caption = "Elaboración propia segun Banco Mundial y Datos Macro",
       y = "Deuda Promedio (% PBI)",
       x = "",        
       fill = "Nivel de Ingreso") + 
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"), 
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

graf_media <- ggplot(analisis_estadistico, aes(x = income, y = media, fill = income)) +
  geom_col(alpha = 0.8, color = "black") +
  facet_wrap(~year) +
  geom_text(aes(label = round(media, 1)), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c(
    "High income"         = "#2E8B57",
    "Upper middle income" = "#9ACD32",
    "Lower middle income" = "#FFA500",
    "Low income"          = "#CD5C5C"
  )) +
  labs(title = "Evolución de la Deuda Promedio (Media)",
       subtitle = "Comparación por nivel de ingreso",
       caption = "Elaboración propia segun Banco Mundial y Datos Macro",
       y = "Deuda Promedio (% PBI)",
       x = "",        
       fill = "Nivel de Ingreso") + 
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"), 
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

# GRAFICO de HISTOGRAMA

graf_histograma <- ggplot(tabla_imputar, aes(x = deuda_pbi)) +
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
    title = "Distribucion de la Deuda Pública",
    subtitle = "Rojo (Sólida) = Mediana | Azul (Punteada) = Media",
    caption = "Elaboración propia segun Banco Mundial y Datos Macro",
    x = "Deuda Pública (% PBI)",
    y = "Cantidad de Países"
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0))

print(graf_histograma)


