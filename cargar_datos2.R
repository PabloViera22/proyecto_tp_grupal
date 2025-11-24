source(here::here("config", "parametros.R"))


indicadores3 <- c(
  "deuda_gob" ="GC.DOD.TOTL.GD.ZS",   # DEBT (Central government debt)
  "crecimiento_pbi" ="NY.GDP.MKTP.KD.ZG",   # GDP_GROWTH (GDP growth (annual %))
  "pbi_p_c" ="NY.GDP.PCAP.KD",      # GDP_P_C (GDP per capita - Constant US$)
  "apertura" ="NE.TRD.GNFS.ZS",      # OPENNESS (Trade (% of GDP))
  "formacion_bruta_capital" ="NE.GDI.FTOT.ZS",      # LTOTAL (Inversión - Gross capital formation)
  "consumo_gobierno" ="NE.CON.GOVT.ZS",      # GOV_EXPEND (Government consumption)
  "interes_real" = "FR.INR.RINR",         # Interest rate (Real interest rate)
  "inflacion" = "NY.GDP.DEFL.KD.ZG",   # Inflation (GDP deflator (annual %))
  "poblacion" = "SP.POP.GROW"          # Population growth
)

datos3 <- WDI(country = "all", indicator = indicadores3,
             start = 1990, 
             end = 2024)
datos3
write.csv(
  x = datos3,          # Tu data.frame a exportar
  file = file.path(dir_data_raw, "csv_datos3"),      # La ruta completa del archivo
  row.names = FALSE,         # Evita incluir los números de fila como una columna
  fileEncoding = "UTF-8"     # Codificación para manejar acentos y caracteres especiales
)

#==============================================================================#
#            ANALISIS DE DATOS PERDIDOS PARA PAISES                            #
#==============================================================================#
# 1. Aplicar miss_var_summary() a tu data frame
conteo_na_por_columna2 <- datos3 %>% 
  miss_var_summary() 

# 2. Mostrar solo el nombre de la variable y el conteo de NA
conteo_na_por_columna2 %>%
  select(variable, n_miss) %>%
  print(n = Inf) # 'print(n = Inf)' para ver todas las filas si hay muchas columnas
nrow(datos3)

#VARIABLE DE OCURRENCIA
# Matriz de patrones usando VIM
variables_key <- c("deuda_gob", "interes_real", "formacion_bruta_capital", "consumo_gobierno","apertura")

aggr(datos3[, variables_key],
     col = c('steelblue', 'red'), 
     numbers = TRUE,
     sortVars = TRUE,
     labels = variables_key,
     cex.axis = 0.8,
     gap = 3,
     ylab = c("Proporción de faltantes", "Patrón de combinación"))

# VEMOS QUE ONDA LA CANTIDAD DE NA DE DEUDA POR AÑO

filtrado_de_na<-datos3%>%filter(!is.na(deuda_gob))
filtrado_de_na


#==============================================================================#
#                     MISMO ANALISIS CON FILTRADO_DE_NA                        #
#==============================================================================#
# 1. Aplicar miss_var_summary() a tu data frame
conteo_na_por_columna2 <- filtrado_de_na %>% 
  miss_var_summary() 

# 2. Mostrar solo el nombre de la variable y el conteo de NA
conteo_na_por_columna2 %>%
  select(variable, n_miss) %>%
  print(n = Inf) # 'print(n = Inf)' para ver todas las filas si hay muchas columnas
nrow(filtrado_de_na)

#VARIABLE DE OCURRENCIA
# Matriz de patrones usando VIM
variables_key <- c("deuda_gob", "interes_real", "formacion_bruta_capital", "consumo_gobierno","apertura")

aggr(filtrado_de_na[, variables_key],
     col = c('steelblue', 'red'), 
     numbers = TRUE,
     sortVars = TRUE,
     labels = variables_key,
     cex.axis = 0.8,
     gap = 3,
     ylab = c("Proporción de faltantes", "Patrón de combinación"))
#==============================================================================#
#FILTRO AQULLAS QUE TENGAS EN UNA FILA MAS DE UN DATO FALTANTE
#==============================================================================#

datos_filtrados_final <- filtrado_de_na %>%
  filter(rowSums(is.na(.)) <= 1)
nrow(datos_filtrados_final)
nrow(filtrado_de_na)

aggr(datos_filtrados_final[, variables_key],
     col = c('steelblue', 'red'), 
     numbers = TRUE,
     sortVars = TRUE,
     labels = variables_key,
     cex.axis = 0.8,
     gap = 3,
     ylab = c("Proporción de faltantes", "Patrón de combinación"))

# Mostrar el resultado (opcional)
# print(datos_filtrados)

#==============================================================================#
# TEST DE LITTLE
#==============================================================================#
# Preparar datos para el test
datos_para_test <- datos_filtrados_final %>%
  select(where(is.numeric)) %>%
  select(where(~any(is.na(.))))
# Realizar test de Little
test_mcar <- naniar::mcar_test(datos_para_test)

# Crear dataframe con resultados
resultados_mcar <- data.frame(
  Estadistico = round(test_mcar$statistic, 2),
  `Grados de libertad` = test_mcar$df,
  `P-value` = format(test_mcar$p.value, scientific = TRUE),
  Conclusion = ifelse(test_mcar$p.value < 0.05, 
                      "Rechazamos H0: Datos NO son MCAR",
                      "No rechazamos H0: Datos podrían ser MCAR")
)

resultados_mcar %>%
  kable(caption = "Resultados del Test de Little para MCAR") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  column_spec(4, bold = TRUE, 
              color = ifelse(test_mcar$p.value < 0.05, "red", "green"))
# RESULTADO ES QUE NO SON DATOS PERDIDOS COMPLETAMNTE AL AZAR


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
  select(modelo, term, estimate, std.error, p.value) %>%
  kable(caption = "Variables significativas para predecir faltantes (p < 0.1)") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  row_spec(which(resumen_modelos$p.value < 0.05), bold = TRUE)

# LOS DATOS SON MAR


#==============================================================================#
# IMPUTACION MULTIPLE
#==============================================================================#
# Seleccionar variables para imputación
vars_mice <- datos_filtrados_final %>%
  select(consumo_gobierno, apertura, formacion_bruta_capital,
         interes_real)

# Configurar y ejecutar MICE
mice_imp <- mice(vars_mice,
                 m = 5,            # 5 imputaciones
                 method = 'pmm',   # Predictive mean matching
                 seed = 2025,
                 printFlag = FALSE)

# Ver métodos utilizados
print(mice_imp$method)

# aca estan los datos imputados
datos_imp_mice <- complete(mice_imp, 1)

#==============================================================================#
# DATOS COMPLETOS
#==============================================================================#
# 1. Definir las columnas a actualizar
vars_a_actualizar <- c("consumo_gobierno", "apertura", "formacion_bruta_capital", "interes_real")

# 2. Reemplazar los NA en la tabla original con los valores imputados de 'datos_imp_mice'
datos_completos <- datos_filtrados_final %>%
  mutate(
    # Usamos 'across' para aplicar la misma lógica a todas las variables en la lista
    across(
      .cols = all_of(vars_a_actualizar),
      
      # Lógica: Si el valor original es NA, usa el valor de la columna imputada
      ~ coalesce(
        .x, # El valor original (puede ser NA)
        datos_imp_mice[[cur_column()]] # El valor correspondiente de la tabla imputada
      )))

#==============================================================================#
# DETECCION DE OUTLIERS
#==============================================================================#
cuartiles <- quantile(datos_completos$crecimiento_pbi, probs = c(0.25, 0.75))
Q1 <- cuartiles[1]
Q3 <- cuartiles[2]

# 2. Calcular el IQR
IQR_valor <- IQR(datos_completos$crecimiento_pbi)

# 3. Definir los límites
limite_inferior <- Q1 - 1.5 * IQR_valor
limite_superior <- Q3 + 1.5 * IQR_valor

outliers <- datos_completos$crecimiento_pbi[
  datos_completos$crecimiento_pbi < limite_inferior | 
    datos_completos$crecimiento_pbi > limite_superior]
print(outliers)

# eliminar outliers
datos_completos_sin_outliers <- datos_completos[
  datos_completos$crecimiento_pbi >= limite_inferior & 
    datos_completos$crecimiento_pbi <= limite_superior, 
]
nrow(datos_completos_sin_outliers)
nrow(datos_completos)



#==============================================================================#
# ANALISIS DE LINEALIDAD Y TODO ESO
#==============================================================================#

modelo_lineal <- lm( crecimiento_pbi~ deuda_gob + apertura + formacion_bruta_capital + 
                      consumo_gobierno + interes_real + inflacion, data = datos_completos_sin_outliers)
summary(modelo_lineal)


resettest(modelo_lineal)
# NO HAY LINEALIDAD, EL MODELO LINEAL NO ES SUFICIENTE


library(mgcv)
modelo_gam<-gam(crecimiento_pbi~ s(deuda_gob) + s(apertura) + s(formacion_bruta_capital) + 
                 s(consumo_gobierno) + s(interes_real) + s(inflacion), data = datos_completos_sin_outliers)
summary(modelo_gam)

AIC(modelo_lineal, modelo_gam)
BIC(modelo_lineal, modelo_gam)


#==============================================================================#
# POR ULTIMO, VEO EL MODELO DE PAPER A VER QUE ONDA CAPAZ TENGO SUERTE
#==============================================================================#
library(plm)

pstr_modelo
# NO SE PUDO, QUELE VAMO A HACER

datos_completos_sin_outliers$deuda2 <- datos_completos_sin_outliers$deuda_gob^2


modelo_nl <- lm(crecimiento_pbi~ deuda_gob + deuda2 + apertura +
                  formacion_bruta_capital + 
                  consumo_gobierno + interes_real + inflacion, data = datos_completos_sin_outliers)
summary(modelo_nl)


datos_completos_sin_outliers$prediccion<-predict(modelo_lineal, newdata = NULL, type = "response", se.fit = FALSE)


ggplot(data = datos_completos_sin_outliers, aes(x = crecimiento_pbi, y = prediccion)) +
  geom_point (color = "blue") +
  # Usar la variable original (crecimiento_pbi) para los puntos
  geom_point(aes(y = crecimiento_pbi), alpha = 0.4) +
  labs(
    title = "Predicción del PBI vs. Deuda de Gobierno",
    y = "Crecimiento PBI (Predicción y Real)",
    x = "Deuda de Gobierno"
  )


modelo_lineal_basico <- lm( crecimiento_pbi~ deuda_gob + deuda2, data = datos_completos_sin_outliers)
summary(modelo_lineal_basico)

#==============================================================================#
# VOY A PROBAR EL MODELO CON DATOS SEGUN CONTINENTE E INGRESO, SUERTE POR FAVOR
#==============================================================================#
datos_completos_sin_outliers

# meta datos
meta <- WDI_data$country
meta_continente<-meta%>%select("iso3c","region", "income")
# agregamos
datos_completos_income_continente <- datos_completos_sin_outliers %>%
  left_join(meta_continente, by = "iso3c")

vector_x<-c("High income", "Low income", "Lower middle income", "Upper middle income")
for (x in vector_x) {
  nuevo<-datos_completos_income_continente%>% filter(income==x)
  #para crecimiento y deuda
  modelo_regresion <- lm( crecimiento_pbi~ deuda_gob + apertura + formacion_bruta_capital + 
                                             consumo_gobierno + interes_real + inflacion, data = datos_completos_sin_outliers)
  
  #para recaudacion y deuda
  modelo_regresion_2 <- lm(crecimiento_pbi~ deuda_gob + deuda2 + apertura + formacion_bruta_capital + 
                             consumo_gobierno + interes_real + inflacion, data = datos_completos_sin_outliers)
  # los print
  print(x)
  print("#================#crecimietno y deuda #================#")
  print(summary(modelo_regresion))
  print("#================#agrego deuda^2#================#")
  print(summary(modelo_regresion_2))

}

datos_completos_sin_agregados<-datos_completos_income_continente%>%filter(region!="Aggregates")
vector_y<- c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean",
            "Middle East & North Africa", "North America", "South Asia", "Sub-Saharan Africa"
            )
for (x in vector_y) {
  nuevo<-datos_completos_income_continente%>% filter(region==x)
  #para crecimiento y deuda
  modelo_regresion <- lm( crecimiento_pbi~ deuda_gob + apertura + formacion_bruta_capital + 
                            consumo_gobierno + interes_real + inflacion, data = datos_completos_sin_outliers)
  
  #para recaudacion y deuda
  modelo_regresion_2 <- lm(crecimiento_pbi~ deuda_gob + deuda2 + apertura + formacion_bruta_capital + 
                             consumo_gobierno + interes_real + inflacion, data = datos_completos_sin_outliers)
  # los print
  print(x)
  print("#================#crecimietno y deuda #================#")
  print(summary(modelo_regresion))
  print("#================#agrego deuda^2#================#")
  print(summary(modelo_regresion_2))
  
}

for (x in vector_y) {
  nuevo<-datos_completos_income_continente%>% filter(region==x)
  #para crecimiento y deuda
  modelo_regresion <- lm( crecimiento_pbi~ deuda_gob, data = datos_completos_sin_outliers)
  
  #para recaudacion y deuda
  modelo_regresion_2 <- lm(crecimiento_pbi~ deuda_gob + deuda2, data = datos_completos_sin_outliers)
  # los print
  print(x)
  print("#================#crecimietno y deuda #================#")
  print(summary(modelo_regresion))
  print("#================#agrego deuda^2#================#")
  print(summary(modelo_regresion_2))
  
}





modelo_nl <- lm(deuda_gob~ interes_real, data = datos_completos_sin_outliers)
summary(modelo_nl)


#==============================================================================#
# MODELO CON LOS DATOS DE GERO
#==============================================================================#
gero <- read.csv2("D:/Proyecto_Git_TP_Grupal/proyecto_tp_grupal/data/raw/deuda_deficit.csv")
gero


ajuste_para_join<-datos_completos_sin_outliers %>%
  rename(paises = country)%>% filter(year==2023)

# data + meta(continente)
datos_join <- ajuste_para_join %>%
  left_join(gero, by = "paises")


modelo_nl <- lm(crecimiento_pbi~ deuda_gob+ deuda2+deficit_pbi, data = datos_join)
summary(modelo_nl)

modelo_regresion <- lm( crecimiento_pbi~ deuda_gob + apertura + deficit_pbi+ formacion_bruta_capital + 
                          consumo_gobierno + interes_real + inflacion, data = datos_join)

#para recaudacion y deuda
modelo_regresion_2 <- lm(crecimiento_pbi~ deuda_gob + deuda2 + apertura + deficit_pbi +formacion_bruta_capital + 
                           consumo_gobierno + interes_real + inflacion, data = datos_join)
summary(modelo_regresion)
summary(modelo_regresion_2)







