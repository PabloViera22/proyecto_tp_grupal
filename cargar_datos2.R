source(here::here("config", "parametros.R"))


indicadores3 <- c(
  "deuda_gob" ="GC.DOD.TOTL.GD.ZS",   # DEBT (Central government debt)
  "crecimiento_pbi" ="NY.GDP.MKTP.KD.ZG",   # GDP_GROWTH (GDP growth (annual %))
  "pbi_p_c" ="NY.GDP.PCAP.KD",      # GDP_P_C (GDP per capita - Constant US$)
  "apertura" ="NE.TRD.GNFS.ZS",      # OPENNESS (Trade (% of GDP))
  "formacion_bruta_capital" ="NE.GDI.FTOT.ZS",      # LTOTAL (Inversión - Gross capital formation)
  "consumo_gobierno" ="NE.CON.GOVT.ZS",      # GOV_EXPEND (Government consumption)
  "interes_real" = "FR.INR.RINR",         # Interest rate (Real interest rate)
  "inflacion" = "NY.GDP.DEFL.KD.ZG",   # Inflation (GDP deflator (annual %))
  "poblacion" = "SP.POP.GROW"          # Population growth
)

datos3 <- WDI(country = "all", indicator = indicadores3,
             start = 1990, 
             end = 2024)
datos3


#==============================================================================#
#            ANALISIS DE DATOS PERDIDOS PARA PAISES                            #
#==============================================================================#
# 1. Aplicar miss_var_summary() a tu data frame
conteo_na_por_columna2 <- datos3 %>% 
  miss_var_summary() 

# 2. Mostrar solo el nombre de la variable y el conteo de NA
conteo_na_por_columna2 %>%
  select(variable, n_miss) %>%
  print(n = Inf) # 'print(n = Inf)' para ver todas las filas si hay muchas columnas
nrow(datos3)

#VARIABLE DE OCURRENCIA
# Matriz de patrones usando VIM
variables_key <- c("deuda_gob", "interes_real", "formacion_bruta_capital", "consumo_gobierno","apertura")

aggr(datos3[, variables_key],
     col = c('steelblue', 'red'), 
     numbers = TRUE,
     sortVars = TRUE,
     labels = variables_key,
     cex.axis = 0.8,
     gap = 3,
     ylab = c("Proporción de faltantes", "Patrón de combinación"))




