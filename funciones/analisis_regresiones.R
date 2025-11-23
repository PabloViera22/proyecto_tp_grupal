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

modelo_regresion_4 <- lm(recaudacion ~ pbi_percapita, data = analizar_na)
print(summary(modelo_regresion_4))




vector_x<-c("High income", "Low income", "Lower middle income", "Upper middle income")


# Veo segun ingreso
for (x in vector_x) {
  nuevo<-analizar_na%>% filter(income==x)
  #para crecimiento y deuda
  modelo_regresion <- lm(crecimiento_pbi ~ deuda_gob, data = analizar_na)
  #para recaudacion y deuda
  modelo_regresion_3 <- lm(recaudacion ~ deuda_gob, data = analizar_na)

  #para pobi y deuda
  modelo_regresion_2 <- lm(pbi_percapita ~ deuda_gob, data = analizar_na)
  # los print
  print("#================#crecimietno y deuda#================#")
  print(summary(modelo_regresion))
  print("#================#pbi y deuda#================#")
  print(summary(modelo_regresion_3))
  print("#================#recaudacion y deuda#================#")
  print(summary(modelo_regresion_2))
}

modelo_regresion_multiple<-lm(pbi_percapita ~ deuda_gob + recaudacion+ipc , data = analizar_na)
print(summary(modelo_regresion_multiple))

