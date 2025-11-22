#Datos
analizar_na<-read_csv(ruta_completa_filtrado)

# Realizamos la regresión con el creimiento economico
modelo_regresion <- lm(crecimiento_pbi ~ deuda_gob, data = analizar_na)
print(summary(modelo_regresion))
#R^2= el modelo no explica nada

# Realizamos la regresión con el pbi
modelo_regresion_2 <- lm(pbi_constante_en_milmillones ~ deuda_gob, data = analizar_na)
print(summary(modelo_regresion_2))


modelo_regresion_3 <- lm(recaudacion ~ deuda_gob, data = analizar_na)
print(summary(modelo_regresion_3))








