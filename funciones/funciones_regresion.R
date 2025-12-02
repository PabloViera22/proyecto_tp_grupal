# FUNCION PARA PROBAR LA HIPOTESIS SOBRE EL SIGNO DE LOS COEFICIENTES
prueba_hipotesis <- function(modelo_de_regresion,titulo) {
  
  g_de_l <- modelo_de_regresion$df.residual
  
  # Valores críticos
  t_critico <- c(
    laxo = qt(0.90, df = g_de_l),
    normal = qt(0.95, df = g_de_l),
    estricto = qt(0.99, df = g_de_l)
  )
  
  # Valores t observados
  deuda_t <- summary(modelo_de_regresion)$coefficients[2, "t value"]
  deuda2_t <- summary(modelo_de_regresion)$coefficients[3, "t value"]
  
  # Crear tabla resultado
  tabla <- tibble(
    test = c(
      "TEST 1: deuda > 0 (laxo)",
      "TEST 1: deuda > 0 (normal)",
      "TEST 1: deuda > 0 (estricto)",
      "TEST 2: deuda^2 < 0 (laxo)",
      "TEST 2: deuda^2 < 0 (normal)",
      "TEST 2: deuda^2 < 0 (estricto)"
    ),
    criterio = c(
      deuda_t > t_critico["laxo"],
      deuda_t > t_critico["normal"],
      deuda_t > t_critico["estricto"],
      deuda2_t < -t_critico["laxo"],
      deuda2_t < -t_critico["normal"],
      deuda2_t < -t_critico["estricto"]
    ),
    decision = ifelse(criterio, "Rechazo H0", "No rechazo H0")
  )
  
  tabla%>% kable(format = "html", caption=titulo)%>%
    kable_styling(full_width = FALSE)
}
