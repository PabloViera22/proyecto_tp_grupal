archivos_en_data()
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

