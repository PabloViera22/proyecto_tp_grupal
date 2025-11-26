# 1ero cargar tabla

# Ver estructura general
str()

# Ver nombres de columnas
names()

# Resumen estadistico
summary()

# Medias por columnas
#colMeans(df[sapply(df, is.numeric)], na.rm = TRUE)

# Medianas
#apply(df, 2, median, na.rm = TRUE)

# Desvíos estándar / varianza
#sapply(df, sd, na.rm = TRUE)

# Hacer boxplot
# 2. Crear el Boxplot
grafico_boxplot <- ggplot(
  data = ,          # DATOS A USAR
  aes(x = ,        # Variable en el eje X
    y = ,  ))+ # Variable en el eje Y
   geom_boxplot() +
  labs( title = "TITULO: COLUMNA",
    x = "X",
    y = "Y") +
  theme_bw()

# PARA GRAFICO DE VIOLIN
grafico_violin <- ggplot(
  data = ,
  aes(x = Species,
    y = Sepal.Length,
    fill = Species)) + 
  geom_violin() +
  geom_jitter(
    color = "black", 
    size = 0.5, 
    alpha = 0.7, 
    width = 0.2 # Evita la superposición extrema
  ) +
  labs(
    title = "TITULO",
    x = "X",
    y = "Y",
    fill = "?"
  ) +
  theme_bw()

# Pdriamos hacer una matriz de correlacion o mapa de calor de correlacion (buscar como HEATMAP)
#pero capaz haya que limpiarlo primero











