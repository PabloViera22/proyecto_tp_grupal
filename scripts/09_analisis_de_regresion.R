
# IMPORTAMOS DATOS
tabla_regresion<-read.csv("D:/Proyecto_Git_TP_Grupal/proyecto_tp_grupal/data//clean/tabla_limpia.csv")
# HIPOTESIS: DEUDA TIENE REALCION EN FORMA DE U INVERTIDA. EL COEFICIENTE DE DEUDA>0 Y EL COEF. DEUDA^2<0.









vector_x<-c("High income", "Low income", "Lower middle income", "Upper middle income")
for (x in vector_x) {
  nuevo<-tabla_regresion%>% filter(income==x)
  #para crecimiento y deuda
  modelo_regresion <- lm( crecimiento_pbi~ deuda_gob + apertura + formacion_bruta_capital + 
                            consumo_gobierno + inflacion, data = tabla_regresion)
  
  #para recaudacion y deuda
  modelo_regresion_2 <- lm(crecimiento_pbi~ deuda_gob + deuda2 + apertura + formacion_bruta_capital + 
                             consumo_gobierno + inflacion, data = tabla_regresion)
  # los print
  print(x)
  print("#================#crecimietno y deuda #================#")
  print(summary(modelo_regresion))
  print("#================#agrego deuda^2#================#")
  print(summary(modelo_regresion_2))
  
}
