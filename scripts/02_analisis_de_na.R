archivos_en_data()
vector_income<- c("Upper middle income", "High income","Lower middle income", "Low income")
# Cargamos los datos del script anterior
datos_analisis_na<-read.csv("D:/Proyecto_Git_TP_Grupal/proyecto_tp_grupal/data//processed/datos_wdi_con_meta.csv")

analisis<-analizar_na(tabla = datos_analisis_na, grupo = "income")
analisis
print(analisis, n=90)
# justificacion para eliminar los datos:

# condicion grupo = Upper middle income, High income,Lower middle income o Low income
grafico_na_medios_altos<-analizar_na_grafico(tabla = datos_analisis_na, grupo="Upper middle income")
grafico_na_altos<-analizar_na_grafico(tabla = datos_analisis_na, grupo="High income")
grafico_na_medios_bajos<-analizar_na_grafico(tabla = datos_analisis_na, grupo="Lower middle income")
grafico_na_bajos<-analizar_na_grafico(tabla = datos_analisis_na, grupo="Low income")

# Test de Little para ver si los datos MCAR
test_de_little(datos = datos_analisis_na) #capaz haya que haceerlo para cada nivel de ingreso

#==============================================================================#
# VEMOS SI SON MAR
#==============================================================================#
datos_indicadores <- datos_filtrados_final %>%
  mutate(
    missing_consumo = as.numeric(is.na(consumo_gobierno)),
    missing_capital = as.numeric(is.na(formacion_bruta_capital)),
    missing_interes = as.numeric(is.na(interes_real))
  )
# Modelo logístico para predecir ingreso faltante
modelo_consumo <- glm(missing_consumo ~ deuda_gob + crecimiento_pbi + 
                        pbi_p_c + apertura + formacion_bruta_capital+
                        interes_real+ inflacion,
                      data = datos_indicadores,
                      family = binomial())   

# Modelo logístico para predecir ingreso faltante
modelo_capital <- glm(missing_capital ~ deuda_gob + crecimiento_pbi + 
                        pbi_p_c + apertura + 
                        interes_real+ inflacion,
                      data = datos_indicadores,
                      family = binomial())
# Modelo para gasto_salud faltante
modelo_interes <- glm(missing_interes ~ deuda_gob + crecimiento_pbi + 
                        pbi_p_c + apertura + formacion_bruta_capital+
                        inflacion,
                      data = datos_indicadores,
                      family = binomial())

# Crear tabla de resumen
resumen_modelos <- bind_rows(
  broom::tidy(modelo_consumo) %>% mutate(modelo = "consumo faltante"),
  broom::tidy(modelo_capital) %>% mutate(modelo = "capital faltante"),
  broom::tidy(modelo_interes) %>% mutate(modelo = "interes faltante")) %>%
  filter(p.value < 0.1) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

resumen_modelos %>%
  dplyr::select(modelo, term, estimate, std.error, p.value) %>%
  kable(caption = "Variables significativas para predecir faltantes (p < 0.1)") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  row_spec(which(resumen_modelos$p.value < 0.05), bold = TRUE)












