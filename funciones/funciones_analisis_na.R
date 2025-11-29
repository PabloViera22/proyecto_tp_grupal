source(here::here("config", "parametros.R"))
#==============================================================================#
# funcion para analizar de forma automatica los NA por grupo
analizar_na<-function(tabla, grupo){
  if (!grupo %in% c("income", "region")){stop("El formato debe ser 'income' o 'region'") }
  if (!is.data.frame(tabla)) {stop("El objeto 'tabla' debe ser un data frame")}
  conteo_na_por_columna<-tabla%>%group_by(.data[[grupo]])%>%miss_var_summary()
  conteo_na<-tabla%>%miss_var_summary()
  list(
    por_columna = conteo_na_por_columna,
    total = conteo_na
  )
}
#==============================================================================#
# funcion para analizar de forma automatica los NA de una tabla sin dividir em grupos
analizar_na_general <- function(tabla) {
  # Validación
  if (!is.data.frame(tabla)) {
    stop("El objeto `tabla` debe ser un data frame.")
  }
  # Cálculo
  conteo_na <- tabla %>%
    miss_var_summary()
  # Salida
  list(por_columna = conteo_na)
}
#==============================================================================#
# hace grafico de analisis de na parecido a heatmap de forma automatica pero no
#permite cambiar las variables a anlizar
analizar_na_grafico<-function(tabla,grupo){
  if (!grupo %in% c("Upper middle income", "High income","Lower middle income", "Low income"))
    {stop("El formato debe ser Upper middle income, High income,Lower middle income, Low income") }
  if (!is.data.frame(tabla)) {stop("El objeto 'tabla' debe ser un data frame")}
  
  var_grupo<-paste("Proporción de faltantes:",grupo)
  patron_grupo<-paste("Patrón de combinación:", grupo)
  
  variables_key <- c("crecimiento_pbi", "pbi_p_c","interes_real", "inflacion","formacion_bruta_capital", "consumo_gobierno","apertura")
  filtro<-tabla%>%filter(income==grupo)  
  aggr(filtro[, variables_key],
       col = c('steelblue', 'red'), 
       numbers = TRUE,
       sortVars = TRUE,
       labels = c("crecimiento", "pbi_p_c","interes_real", "inflacion","FMK", "consumo","apertura"),
       cex.axis = 0.8,
       gap = 3,
       ylab = c(var_grupo,patron_grupo))
  }

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

}














