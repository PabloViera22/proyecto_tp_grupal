source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_regresion.R"))
# IMPORTAMOS DATOS
tabla_regresion<-cargar_datos(nombre_archivo = "tabla_limpia.csv", carpeta = "clean")
# HIPOTESIS: DEUDA TIENE REALCION EN FORMA DE U INVERTIDA. EL COEFICIENTE DE DEUDA>0 Y EL COEF. DEUDA^2<0.
# TEST DE HIPOTESIS 
# HAY QUE HACER DOS TEST DE HIPOTESIS. 
# 1) H_0: Beta_1>=0 
#    H_1: Beta_1<0
# 2) H_0: Beta_2<=0
#    H_1: Beta_2>0

#==============================================================================#
# MODELO REGRESION GENERAL CORROBARCION DE HIPÓTESIS
#==============================================================================#
modelo_regresion_general <- lm( crecimiento_pbi~ deuda_pbi + deuda_cuadrada+ deficit_pbi + apertura + formacion_bruta_capital + 
                          consumo_gobierno + inflacion+ income, data = tabla_regresion)
summary(modelo_regresion_general)
# A PRIMERA VISTA VEMOS QUE NO PODEMOS DETERMINAR EL SIGNO DE DEUDA Y DEUDA CUADRADA

# PRUEBA HIPTESIS
prueba_hipotesis(modelo_regresion_general)
#==============================================================================#
# REGRESION PARA INGRESO ALTO
#==============================================================================#
ingreso_alto<-tabla_regresion%>%dplyr::filter(income=="High income")
modelo_regresion_alto <- lm( crecimiento_pbi~ deuda_pbi + deuda_cuadrada+ deficit_pbi + apertura + formacion_bruta_capital + 
                                  consumo_gobierno + inflacion, data = ingreso_alto)
summary(modelo_regresion_alto)

# PRUEBA HIPTESIS
prueba_hipotesis(modelo_regresion_alto)
#==============================================================================#
# REGRESION PARA INGRESO MEDIO
#==============================================================================#
ingreso_medio<-tabla_regresion%>%dplyr::filter(income=="Upper middle income")
modelo_regresion_medio <- lm( crecimiento_pbi~ deuda_pbi + deuda_cuadrada+ deficit_pbi + apertura + formacion_bruta_capital + 
                               consumo_gobierno + inflacion, data = ingreso_medio)
summary(modelo_regresion_medio)

# PRUEBA HIPTESIS
prueba_hipotesis(modelo_regresion_medio)


#==============================================================================#
# REGRESION PARA INGRESO MEDIO BAJO
#==============================================================================#
ingreso_medio_bajo<-tabla_regresion%>%dplyr::filter(income=="Lower middle income")
modelo_regresion_medio_bajo <- lm( crecimiento_pbi~ deuda_pbi + deuda_cuadrada+ deficit_pbi + apertura + formacion_bruta_capital + 
                                consumo_gobierno + inflacion, data = ingreso_medio_bajo)
summary(modelo_regresion_medio_bajo)

# PRUEBA HIPTESIS
prueba_hipotesis(modelo_regresion_medio_bajo)


#==============================================================================#
# REGRESION PARA INGRESO BAJO
#==============================================================================#
ingreso_bajo<-tabla_regresion%>%dplyr::filter(income=="Low income")
modelo_regresion_bajo <- lm( crecimiento_pbi~ deuda_pbi + deuda_cuadrada+ deficit_pbi + apertura + formacion_bruta_capital + 
                                     consumo_gobierno + inflacion, data = ingreso_bajo)
summary(modelo_regresion_bajo)

# PRUEBA HIPTESIS
prueba_hipotesis(modelo_regresion_bajo)

# CONCLUSIÓN 
# los datos no son concluyentes pero no podemos rechazar la hipotesis de que un 
#aumento de la deuda publica aumente el crecimiento en el corto plazo y lo reduzca
#en el largo. Esta hipótesis es significatva para el caso en los paises con ingresos
#medios








