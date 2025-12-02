source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_regresion.R"))
source(here::here("funciones", "funciones_visualizacion.R"))

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
# VISUALIZACION DE LA HIPOTESIS
#==============================================================================#

tabla_regresion <- tabla_regresion %>% 
  mutate(income = factor(income, levels = c("High income", 
                                            "Upper middle income", 
                                              "Lower middle income", 
                                              "Low income")))


graf_hipotesis <- ggplot(tabla_regresion, aes(x = deuda_pbi, y = crecimiento_pbi)) +
  geom_point(aes(color = income), alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), 
              color = "black", size = 1, se = FALSE) +
  facet_wrap(~year, scales = "free_y") +
  labs(title = "Relación Deuda vs. Crecimiento",
       subtitle = "Test visual de la hipótesis de U Invertida (Línea negra = Ajuste cuadrático)",
       x = "Deuda Pública (% PBI)",
       y = "Crecimiento del PBI (%)",
       color = "Ingreso") +
  scale_color_manual(values = c(
    "High income"         = "#2E8B57",
    "Upper middle income" = "#9ACD32",
    "Lower middle income" = "#FFA500",
    "Low income"          = "#CD5C5C"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, face = "bold"),
    plot.caption = element_text(hjust = 0)
  ) 
  
guardar_grafico(graf_hipotesis, "graf_hipotesis")

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
# aumento de la deuda publica aumente el crecimiento en el corto plazo y lo reduzca
# en el largo. Esta hipótesis es significativa para el caso en los paises con ingresos
# medios








