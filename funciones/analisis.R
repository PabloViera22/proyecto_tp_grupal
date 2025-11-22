# Analisis de NA
analisar_na<-read_csv(ruta_completa)

# 1. Definir las columnas de datos que quieres revisar (excluyendo 'country', 'iso2c', 'iso3c', 'year')
# Ajusta los nombres de las columnas si son diferentes en tu data frame real.
columnas_a_revisar <- c("deuda_gob", "crecimiento_pbi", "recaudacion", "ipc", "pbi_constante")

# 2. Agrupar por el año y sumar los NAs en las columnas seleccionadas
resumen_na_por_año <- datos %>%
  # Agrupar las filas por el año
  group_by(year) %>%
  
  # Para cada año, calcular la suma de NAs en cada columna
  summarise(
    # La expresión sum(is.na(nombre_columna)) cuenta cuántos TRUE (NA) hay
    NA_deuda_gob = sum(is.na(deuda_gob)),
    NA_crecimiento_pbi = sum(is.na(crecimiento_pbi)),
    NA_recaudacion = sum(is.na(recaudacion)),
    NA_ipc = sum(is.na(ipc)),
    NA_pbi_constante = sum(is.na(pbi_constante))
    # Puedes añadir más columnas según sea necesario
  )
# 3. Mostrar el resultado
print(resumen_na_por_año)


#==============================================================================#
# Resultados: 2015 es el año con menos faltantes, uso ese año
#==============================================================================#
filtrado_2015<-analisar_na%>%filter(year==2015)
filtrado_na<-filtrado_2015%>%filter(!is.na(deuda_gob))%>%mutate(pbi_constante_en_milmillones=pbi_constante/ 1e9)%>%mutate(log_pbi=log(pbi_constante_en_milmillones))


#==============================================================================#
# Voy a hacer un gráfico de dispersión de deuda publica y crecimietno del pbi 
#por contintente
#==============================================================================#

grafico_dispersion_por_region <- ggplot(
  data = filtrado_na, 
  # CLAVE: Añadir 'color = region' dentro de aes()
  aes(x = crecimiento_pbi, y = deuda_gob, color = region)
) +
  # Añade la capa de puntos. Ya no necesitas especificar color="darkblue" aquí
  geom_point(alpha = 0.6) + 
  
  # Añade una línea de tendencia (regresión lineal) general.
  # Si quieres una línea por cada región, pon color = region también en geom_smooth
  geom_smooth(method = "lm", se = FALSE) + 
  
  # Etiquetado y títulos
  labs(
    title = "Relación entre Deuda del Gobierno y Crecimiento del PBI, por Región",
    x = "Crecimiento del PBI (%)",
    y = "Deuda del Gobierno (% del PBI)",
    color = "Región WDI", # Título para la leyenda de color
    caption = "Fuente: Datos del Banco Mundial (WDI)"
  ) +
  # Temas para darle un aspecto más limpio
  theme_classic()

# 3. Muestra el gráfico
print(grafico_dispersion_por_region)


#==============================================================================#
# Voy a hacer un gráfico de dispersión de deuda publica y pbi
#==============================================================================#

grafico_dispersion_2 <- ggplot(
  data = filtrado_na, 
  aes(x = log_pbi, y = deuda_gob, color= region) # Define qué columna va en X y qué va en Y
) +
  # Añade la capa de puntos (la dispersión)
  geom_point(alpha = 1.6) + 
  
  # Añade una línea de tendencia (regresión lineal) opcional para ver la relación
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  
  # Etiquetado y títulos
  labs(
    title = "Relación entre Deuda del Gobierno y PBI constante en USD",
    x = "PBI a precios constantes",
    y = "Deuda del Gobierno (% del PBI)",
    caption = "Fuente: Datos del Banco Mundial (WDI)"
  ) +
  # Temas para darle un aspecto más limpio
  theme_minimal()

# 3. Muestra el gráfico
print(grafico_dispersion_2)

#==============================================================================#
# Voy a hacer un gráfico de dispersión de deuda publica y pbi por ingreso
#==============================================================================#
grafico_dispersion_por_region <- ggplot(
  data = filtrado_na, 
  # CLAVE: Añadir 'color = region' dentro de aes()
  aes(x = crecimiento_pbi, y = deuda_gob, color = income)
) +
  # Añade la capa de puntos. Ya no necesitas especificar color="darkblue" aquí
  geom_point(alpha = 0.6) + 
  
  # Añade una línea de tendencia (regresión lineal) general.
  # Si quieres una línea por cada región, pon color = region también en geom_smooth
  geom_smooth(method = "lm", se = FALSE) + 
  
  # Etiquetado y títulos
  labs(
    title = "Relación entre Deuda del Gobierno y Crecimiento del PBI, por Región",
    x = "Crecimiento del PBI (%)",
    y = "Deuda del Gobierno (% del PBI)",
    color = "Región WDI", # Título para la leyenda de color
    caption = "Fuente: Datos del Banco Mundial (WDI)"
  ) +
  # Temas para darle un aspecto más limpio
  theme_classic()

# 3. Muestra el gráfico
print(grafico_dispersion_por_region) #los que dicen agregates no son paises, sino agregados de países



















