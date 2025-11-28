# IMPORTAMOS DATOS
tabla_regresion<-read.csv("D:/Proyecto_Git_TP_Grupal/proyecto_tp_grupal/data//clean/tabla_limpia.csv")
# HIPOTESIS: DEUDA TIENE REALCION EN FORMA DE U INVERTIDA. EL COEFICIENTE DE DEUDA>0 Y EL COEF. DEUDA^2<0.


#==============================================================================#
# MODELO REGRESION GENERAL CORROBARCION DE HIPÓTESIS
#==============================================================================#
modelo_regresion_general <- lm( crecimiento_pbi~ deuda_pbi + deuda_cuadrada+ deficit_pbi + apertura + formacion_bruta_capital + 
                          consumo_gobierno + inflacion, data = tabla_regresion)
summary(modelo_regresion_general)
# A PRIMERA VISTA VEMOS QUE NO PODEMOS DETERMINAR EL SIGNO DE DEUDA Y DEUDA CUADRADA

#==============================================================================#
# REGRESION PARA INGRESO ALTO
#==============================================================================#
ingreso_alto<-tabla_regresion%>%dplyr::filter(income=="High income")
modelo_regresion_alto <- lm( crecimiento_pbi~ deuda_pbi + deuda_cuadrada+ deficit_pbi + apertura + formacion_bruta_capital + 
                                  consumo_gobierno + inflacion, data = ingreso_alto)
summary(modelo_regresion_alto)
#==============================================================================#
# REGRESION PARA INGRESO MEDIO
#==============================================================================#
ingreso_medio<-tabla_regresion%>%dplyr::filter(income=="Upper middle income")
modelo_regresion_medio <- lm( crecimiento_pbi~ deuda_pbi + deuda_cuadrada+ deficit_pbi + apertura + formacion_bruta_capital + 
                               consumo_gobierno + inflacion, data = ingreso_medio)
summary(modelo_regresion_medio)

#==============================================================================#
# REGRESION PARA INGRESO MEDIO BAJO
#==============================================================================#
ingreso_medio_bajo<-tabla_regresion%>%dplyr::filter(income=="Lower middle income")
modelo_regresion_medio_bajo <- lm( crecimiento_pbi~ deuda_pbi + deuda_cuadrada+ deficit_pbi + apertura + formacion_bruta_capital + 
                                consumo_gobierno + inflacion, data = ingreso_medio_bajo)
summary(modelo_regresion_medio_bajo)

#==============================================================================#
# REGRESION PARA INGRESO BAJO
#==============================================================================#
ingreso_bajo<-tabla_regresion%>%dplyr::filter(income=="Low income")
modelo_regresion_bajo <- lm( crecimiento_pbi~ deuda_pbi + deuda_cuadrada+ deficit_pbi + apertura + formacion_bruta_capital + 
                                     consumo_gobierno + inflacion, data = ingreso_bajo)
summary(modelo_regresion_bajo)

# CONCLUSIÓN 
# los datos no son concluyentes pero no podemos rechazar la hipotesis de que un 
#aumento de la deuda publica aumente el crecimiento en el corto plazo y lo reduzca
#en el largo. Esta hipótesis es significatva para el caso en los paises con ingresos
#medios








