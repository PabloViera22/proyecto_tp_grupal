#==============================================================================#
# IMPUTACION MULTIPLE
#==============================================================================#
vector_columna<-c("consumo_gobierno", "apertura", "formacion_bruta_capital",
                  "funciones_para_imputar.R")
# Seleccionar variables para imputación
imputacion_multiple<-function(datos, vector_columna){
  vars_mice <- datos %>%
    dplyr::select(vector_columna)
  
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


