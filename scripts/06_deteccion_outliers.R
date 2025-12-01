source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_visualizacion.R"))

# Importacion de scipt anterior

archivos_en_data()

tabla_limpia <- cargar_datos(nombre_archivo = "tabla_limpia.csv", carpeta = "clean")

#======================================================================
#                   AÑO 2017: El mundo pre-pandemia                    #                   
#======================================================================

anio_2017 <- tabla_limpia %>% 
  filter(year == 2017) %>% 
  mutate(income = factor(income, levels = c("High income", 
                                          "Upper middle income", 
                                          "Lower middle income", 
                                          "Low income")))

#==========================================
# MEDIA, MEDIANA y ANALISIS DE DISPERSION # 
#==========================================

# Ordeno segun ingreso

# Analizaremos segun ingreso de cada pais

anio_2017 %>%
  group_by(income) %>%
  summarise(
    cantida_paises = n_distinct(country),
    mediana = median(deuda_pbi),
    media = mean(deuda_pbi),
    desvio = sd(deuda_pbi),
    iqr = IQR(deuda_pbi),
    minimo = min(deuda_pbi),
    maximo = max(deuda_pbi)
  ) %>% 
  arrange(income)

#==================================
# ANALISIS DEL BOXPLOT y OUTLIERS #
#==================================

# Outliers dentro de cada grupo

paises_outliers_2017 <- anio_2017 %>% 
  group_by(income) %>% 
  mutate(
    Q1 = quantile(deuda_pbi, 0.25),
    Q3 = quantile(deuda_pbi, 0.75),
    IQR = IQR(deuda_pbi),
    techo = Q3 + 1.5*IQR,
    piso = Q1 - 1.5*IQR
  ) %>% 
  filter(deuda_pbi > techo | deuda_pbi < piso) %>% 
  select(country, income, pbi_p_c, deuda_pbi) %>% 
  arrange(desc(deuda_pbi))
  
print("Para el año 2017, estos paises fueron outliers dentro de sus grupos: ")
print(paises_outliers_2017)

# Grafico del boxplot 

boxplot_2017 <- ggplot(anio_2017, aes(x = income, y = deuda_pbi, fill = income)) +
  geom_boxplot(alpha = 0.8) + 
  # Titulos
  labs(
    title = "Distribución de la Deuda Pública para el año 2017",
    subtitle = "Ordenado segun ingresos de los paises",
    caption = "Elaboracion propia segun Banco Mundial y Datos Macro",
    x = "",
    y = "Deuda como % del PBI"
  ) +
  # Etiqueta outliers
  
  geom_text_repel(data = paises_outliers_2017,   
                  aes(label = country),     
                  size = 4,                 
                  box.padding = 0.4
                  ) +
  paleta_ingresos() +
  tema_boxplot()

print(boxplot_2017)

#======================================================================
#                   AÑO 2020: El mundo en pandemia                    #                   
#======================================================================
  
anio_2020 <- tabla_limpia %>% 
  filter(year == 2020) %>% 
  mutate(income = factor(income, levels = c("High income", 
                                            "Upper middle income", 
                                            "Lower middle income", 
                                            "Low income")))

#==========================================
# MEDIA, MEDIANA y ANALISIS DE DISPERSION # 
#==========================================

# Ordeno segun ingreso

# Analizaremos segun ingreso de cada pais

anio_2020 %>%
  group_by(income) %>%
  summarise(
    cantida_paises = n_distinct(country),
    mediana = median(deuda_pbi),
    media = mean(deuda_pbi),
    desvio = sd(deuda_pbi),
    iqr = IQR(deuda_pbi),
    minimo = min(deuda_pbi),
    maximo = max(deuda_pbi)
  ) %>% 
  arrange(income)

#==================================
# ANALISIS DEL BOXPLOT y OUTLIERS #
#==================================

# Outliers dentro de cada grupo

paises_outliers_2020 <- anio_2020 %>% 
  group_by(income) %>% 
  mutate(
    Q1 = quantile(deuda_pbi, 0.25),
    Q3 = quantile(deuda_pbi, 0.75),
    IQR = IQR(deuda_pbi),
    techo = Q3 + 1.5*IQR,
    piso = Q1 - 1.5*IQR
  ) %>% 
  filter(deuda_pbi > techo | deuda_pbi < piso) %>% 
  select(country, income, pbi_p_c, deuda_pbi) %>% 
  arrange(desc(deuda_pbi))

print("Para el año 2020, estos paises fueron outliers dentro de sus grupos: ")
print(paises_outliers_2020)

# Grafico del boxplot 

boxplot_2020 <- ggplot(anio_2020, aes(x = income, y = deuda_pbi, fill = income)) +
  geom_boxplot(alpha = 0.8) + 
  # Titulos y etiquetas
  labs(
    title = "Distribución de la Deuda Pública para el año 2020",
    subtitle = "Ordenado segun ingresos de los paises",
    caption = "Elaboracion propia segun Banco Mundial y Datos Macro",
    x = "",
    y = "Deuda como % del PBI"
  ) +
  # Etiqueta outliers
  
  geom_text_repel(data = paises_outliers_2020,   
                  aes(label = country),     
                  size = 4,                 
                  box.padding = 0.4
  ) +
  paleta_ingresos() +
  tema_boxplot()

print(boxplot_2020)

#======================================================================
#                   AÑO 2023: El mundo post-pandemia                  #                   
#======================================================================

anio_2023 <- tabla_limpia %>% 
  filter(year == 2023) %>% 
  mutate(income = factor(income, levels = c("High income", 
                                            "Upper middle income", 
                                            "Lower middle income", 
                                            "Low income")))

#==========================================
# MEDIA, MEDIANA y ANALISIS DE DISPERSION # 
#==========================================

# Ordeno segun ingreso

# Analizaremos segun ingreso de cada pais

anio_2023 %>%
  group_by(income) %>%
  summarise(
    cantida_paises = n_distinct(country),
    mediana = median(deuda_pbi),
    media = mean(deuda_pbi),
    desvio = sd(deuda_pbi),
    iqr = IQR(deuda_pbi),
    minimo = min(deuda_pbi),
    maximo = max(deuda_pbi)
  ) %>% 
  arrange(income)

#==================================
# ANALISIS DEL BOXPLOT y OUTLIERS #
#==================================

# Outliers dentro de cada grupo

paises_outliers_2023 <- anio_2023 %>% 
  group_by(income) %>% 
  mutate(
    Q1 = quantile(deuda_pbi, 0.25),
    Q3 = quantile(deuda_pbi, 0.75),
    IQR = IQR(deuda_pbi),
    techo = Q3 + 1.5*IQR,
    piso = Q1 - 1.5*IQR
  ) %>% 
  filter(deuda_pbi > techo | deuda_pbi < piso) %>% 
  select(country, income, pbi_p_c, deuda_pbi) %>% 
  arrange(desc(deuda_pbi))

print("Para el año 2023, estos paises fueron outliers dentro de sus grupos: ")
print(paises_outliers_2023)

# Grafico del boxplot 

boxplot_2023 <- ggplot(anio_2023, aes(x = income, y = deuda_pbi, fill = income)) +
  geom_boxplot(alpha = 0.8) + 
  # Titulos y etiquetas
  labs(
    title = "Distribución de la Deuda Pública para el año 2023",
    subtitle = "Ordenado segun ingresos de los paises",
    caption = "Elaboracion propia segun Banco Mundial y Datos Macro",
    x = "",
    y = "Deuda como % del PBI"
  ) +
  # Etiqueta outliers
  
  geom_text_repel(data = paises_outliers_2023,   
                  aes(label = country),     
                  size = 4,                 
                  box.padding = 0.4
  ) +
  paleta_ingresos() +
  tema_boxplot()

print(boxplot_2023)

#======================================================================
#                   BOXPLOT CONJUNTO                                  #                   
#======================================================================

anio_todos <- tabla_limpia %>% 
  mutate(income = factor(income, levels = c("High income", 
                                            "Upper middle income", 
                                            "Lower middle income", 
                                            "Low income")))

paises_outliers_todos <- anio_todos %>%
  group_by(year, income) %>% 
  mutate(
    Q1 = quantile(deuda_pbi, 0.25),
    Q3 = quantile(deuda_pbi, 0.75),
    IQR = IQR(deuda_pbi),
    techo = Q3 + 1.5 * IQR,
    piso  = Q1 - 1.5 * IQR
  ) %>%
  ungroup() %>%
  filter(deuda_pbi > techo | deuda_pbi < piso) %>% 
  select(country, year, income, pbi_p_c, deuda_pbi) %>% 
  arrange(desc(deuda_pbi))

print("Todos los outliers")
print(paises_outliers_todos)

boxplot_evolucion <- ggplot(tabla_limpia, aes(x = income, y = deuda_pbi, fill = income)) +
  geom_boxplot(alpha = 0.8) + 
  facet_wrap(~ year) + # Dividido en tres paneles
  labs(
    title = "Evolución de la Deuda Pública (2017 - 2023)",
    subtitle = "Comparación de distribución y outliers por año y nivel de ingreso",
    caption = "Elaboracion propia segun Banco Mundial y Datos Macro",
    x = "",
    y = "Deuda como % del PBI",
    fill = "Ingreso"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(), 
    plot.caption = element_text(hjust = 0),
    strip.text = element_text(size = 12, face = "bold") 
  ) +
  geom_text_repel(data = paises_outliers_todos,   
                  aes(label = country),      
                  size = 3,                  
                  box.padding = 0.4,
                  max.overlaps = 15 
  ) +
  paleta_ingresos()

print(boxplot_evolucion)
