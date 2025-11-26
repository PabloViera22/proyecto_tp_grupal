analizar_na<-function(tabla, grupo){
  if (!grupo %in% c("income", "region")){stop("El formato debe ser 'income' o 'region'") }
  if (!is.data.frame(tabla)) {stop("El objeto 'tabla' debe ser un data frame")}
  conteo_na_por_columna<-tabla%>%group_by(.data[[grupo]])%>%miss_var_summary()
  print(conteo_na_por_columna)
}
  
analizar_na_grafico<-function(tabla,grupo){
  if (!grupo %in% c("Upper middle income", "High income","Lower middle income", "Low income"))
    {stop("El formato debe ser Upper middle income, High income,Lower middle income, Low income") }
  if (!is.data.frame(tabla)) {stop("El objeto 'tabla' debe ser un data frame")}
  
  var_grupo<-paste("Proporción de faltantes:",grupo)
  patron_grupo<-paste("Patrón de combinación:", grupo)
  
  variables_key <- c("deuda_gob", "interes_real", "formacion_bruta_capital", "consumo_gobierno","apertura")
  filtro<-tabla%>%filter(income==grupo)  
  aggr(filtro[, variables_key],
       col = c('steelblue', 'red'), 
       numbers = TRUE,
       sortVars = TRUE,
       labels = variables_key,
       cex.axis = 0.8,
       gap = 3,
       ylab = c(var_grupo,patron_grupo))
  }
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

#==============================================================================#
# TEST DE LITTLE
#==============================================================================#
# Preparar datos para el test
test_de_little<- function(datos){
datos_para_test <- datos %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(where(~any(is.na(.))))
# Realizar test de Little
test_mcar <- naniar::mcar_test(datos_para_test)
return(test_mcar)
# Crear dataframe con resultados
resultados_mcar <- data.frame(
  Estadistico = round(test_mcar$statistic, 2),
  `Grados de libertad` = test_mcar$df,
  `P-value` = format(test_mcar$p.value, scientific = TRUE),
  Conclusion = ifelse(test_mcar$p.value < 0.05, 
                      "Rechazamos H0: Datos NO son MCAR",
                      "No rechazamos H0: Datos podrían ser MCAR"))
resultados_mcar %>%
  kable(caption = "Resultados del Test de Little para MCAR") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  column_spec(4, bold = TRUE, 
              color = ifelse(test_mcar$p.value < 0.05, "red", "green"))
# RESULTADO ES QUE NO SON DATOS PERDIDOS COMPLETAMNTE AL AZAR
}

#==============================================================================#
# IMPUTACION MULTIPLE
#==============================================================================#
vector_columna<-c("consumo_gobierno", "apertura", "formacion_bruta_capital",
                "interes_real")
# Seleccionar variables para imputación
imputacion_multiple<-function(datos, vector_columna){
vars_mice <- datos %>%
  dplyr::select(vactor_columna)

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
return(datos_imp_mice)
}



