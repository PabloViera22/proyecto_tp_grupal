# 1ero cargar tabla
tabla_inspeccion<-read_csv("D:/Proyecto_Git_TP_Grupal/proyecto_tp_grupal/data//processed/tabla_completa.csv")
# Ver estructura general
str(tabla_inspeccion)
nrow(tabla_inspeccion)
ncol(tabla_inspeccion)
# Ver nombres de columnas
names(tabla_inspeccion)

# Resumen estadistico
summary(tabla_inspeccion)

# Desvíos estándar / varianza
#sapply(df, sd, na.rm = TRUE)

# Hacer boxplot
# 2. Crear el Boxplot
df_long <- tabla_inspeccion %>%
  pivot_longer(cols = where(is.numeric),      # todas las columnas numéricas
    names_to = "variable",
    values_to = "valor")%>% dplyr::filter(!variable %in% c("year", "poblacion"))

ggplot(df_long, aes(x = valor)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),        # sacar título eje Y
    axis.text.y  = element_blank(),        # sacar etiquetas eje Y
    axis.ticks.y = element_blank(),        # sacar ticks eje Y
    strip.text = element_text(size = 12, face = "bold")
  )

# Pdriamos hacer una matriz de correlacion o mapa de calor de correlacion (buscar como HEATMAP)
#pero capaz haya que limpiarlo primero











