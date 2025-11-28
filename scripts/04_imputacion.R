# Importamos los datos del anterior script
tabla_imputar<-read_csv("D:/Proyecto_Git_TP_Grupal/proyecto_tp_grupal/data//processed/tabla_para_imputacion.csv")

col_a_imputar<- c("formacion_bruta_capital", "apertura", "pbi_p_c", "crecimiento_pbi", "consumo_gobierno", "inflacion")
tabla_imputada<-imputacion_multiple(datos = tabla_imputar, vector_columna = col_a_imputar)
nrow(tabla_imputada)


tabla_completa_limpia <- tabla_imputar %>% dplyr::select(-all_of(col_a_imputar))%>%
  left_join(tabla_imputada, by = c("id", "year"), suffix = c("_orig", "_imp"))








exportar_data(data = tabla_imputada, nombre = "tabla_limpia", carpeta = "clean")
