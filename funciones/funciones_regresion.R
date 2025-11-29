prueba_hipotesis<-function(modelo_de_regresion){
  g_de_l<-modelo_de_regresion$df.residual
  
  t_critico_comun<-qt(0.05/2, df = g_de_l) #df son los grados de libertad
  t_critico_estricto<-qt(0.01/2, df = g_de_l)
  t_critico_laxo<-qt(0.10/2, df = g_de_l)
  
  deuda_t_obs<-summary(modelo_de_regresion)$coefficients[2, "t value"]
  deuda_cuadrada_t_obs<-summary(modelo_de_regresion)$coefficients[3, "t value"]
  
  if (-t_critico_comun-deuda_t_obs<0) {print("TEST 1 NORMAL: RECHAZO H_0, deuda tiene signo positivo")}else {print("TEST 1 NORMAL: NO RECHAZO H_0, deuda no tiene signo positivo")}
  if (-t_critico_estricto-deuda_t_obs<0) {print("TEST 1 ESTRICTO: RECHAZO H_0, deuda tiene signo positivo")}else{print("TEST 1 ESTRICTO: NO RECHAZO H_0, deuda no tiene signo positivo")}
  if (-t_critico_laxo-deuda_t_obs<0) {print("TEST 1 LAXO: RECHAZO H_0, deuda tiene signo positivo")}else{print("TEST 1 LAXO: NO RECHAZO H_0, deuda no tiene signo positivo")}

  if (t_critico_comun-deuda_cuadrada_t_obs>0) {print("TEST 2 NORMAL: RECHAZO H_0, deuda^2 tiene signo negativo")}else {print("TEST 2 NORMAL: NO RECHAZO H_0, deuda^2 no tiene signo negativo")}
  if (t_critico_estricto-deuda_cuadrada_t_obs>0) {print("TEST 2 ESTRICTO: RECHAZO H_0, deuda^2 tiene signo negativo")}else{print("TEST 2 ESTRICTO: NO RECHAZO H_0, deuda^2 no tiene signo negativo")}
  if (t_critico_laxo-deuda_cuadrada_t_obs>0) {print("TEST 2 LAXO: RECHAZO H_0, deuda^2 tiene signo negativo")}else{print("TEST 2 LAXO: NO RECHAZO H_0, deuda^2 no tiene signo negativo")}
}








