source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_para_imputar.R"))
# Importamos los datos del anterior script
tabla_imputar<-cargar_datos(nombre_archivo = "tabla_para_imputacion.csv",carpeta = "processed")

columnas_todas<-names(tabla_imputar)
col_a_imputar<- c("formacion_bruta_capital", "apertura", "pbi_p_c", "crecimiento_pbi", "consumo_gobierno", "inflacion")
tabla_imputada<-imputacion_multiple(datos = tabla_imputar, vector_columna = columnas_todas)
nrow(tabla_imputada)
tabla_imputada

exportar_data(data = tabla_imputada, nombre = "tabla_limpia", carpeta = "clean")
