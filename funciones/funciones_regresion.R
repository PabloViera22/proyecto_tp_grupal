prueba_hipotesis<-function(modelo_de_regresion){
  g_de_l<-modelo_de_regresion$df.residual
  
  t_critico_comun<-qt(0.05/2, df = g_de_l) #df son los grados de libertad
  t_critico_estricto<-qt(0.01/2, df = g_de_l)
  t_critico_laxo<-qt(0.10/2, df = g_de_l)
  
  deuda_t_obs<-summary(modelo_de_regresion)$coefficients[2, "t value"]
  
  if (-t_critico_comun-deuda_t_obs<0) {print("TEST NORMAL: RECHAZO H_0")}else {print("TEST NORMAL: NO RECHAZO H_0")}
  if (-t_critico_estricto-deuda_t_obs<0) {print("TEST ESTRICTO: RECHAZO H_0")}else{print("TEST ESTRICTO: NO RECHAZO H_0")}
  if (-t_critico_laxo-deuda_t_obs<0) {print("TEST LAXO: RECHAZO H_0")}else{print("TEST LAXO: NO RECHAZO H_0")}
  
}








