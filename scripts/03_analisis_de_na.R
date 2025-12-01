source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
source(here::here("funciones", "funciones_analisis_na.R"))
source(here::here("funciones", "funciones_estadistica.R"))
archivos_en_data()
vector_income<- c("Upper middle income", "High income","Lower middle income", "Low income")
# Cargamos los datos del script anterior

datos_analisis_na<-cargar_datos(nombre_archivo = "tabla_primer_limpieza.csv", carpeta = "processed")

analisis<-analizar_na(tabla = datos_analisis_na, grupo = "income")
print(analisis$por_columna, n=Inf)
print(analisis$total, n=Inf)
analisis$total%>%kable(format = "html", caption="Conteo de NA")%>%
  kable_styling(full_width = FALSE)
# justificacion para eliminar los datos:ELIMINAR COLUMNA INTERES QUE NO SIRVA
#PARA NADA Y TIENE UN MONTON DE FALTANTES

# condicion grupo = Upper middle income, High income,Lower middle income o Low income
analizar_na_grafico(tabla = datos_analisis_na, grupo="Low income")
analizar_na_grafico(tabla = datos_analisis_na, grupo="Lower middle income")
analizar_na_grafico(tabla = datos_analisis_na, grupo="Upper middle income")
analizar_na_grafico(tabla = datos_analisis_na, grupo="High income")

# GRÁFICO GENERAL
analizar_na_grafico_general(tabla =datos_analisis_na )



#==============================================================================#
# VEMOS SI LOS DATOS SON MCAR CON TEST DE LITTLE
#==============================================================================#
# Test de Little para ver si los datos MCAR
test_de_little(datos = datos_analisis_na) #capaz haya que haceerlo para cada nivel de ingreso
# LOS DATOS PRODRÍAN SER MCAR, BUENISIMO!!


#==============================================================================#
# VEMOS SI SON MAR SI RECHAZAMOS QUE SEAN MCAR
#==============================================================================#
colnames(datos_analisis_na)
variables_na<-c("formacion_bruta_capital", "apertura", "pbi_p_c", "crecimiento_pbi", "consumo_gobierno", "inflacion")
var_predictoras<-c("formacion_bruta_capital", "apertura", "pbi_p_c", "crecimiento_pbi",
                   "consumo_gobierno", "inflacion", "deuda_pbi", "deficit_pbi")

datos_indicadores <- datos_analisis_na %>%
  mutate(
    missing_apertura = as.numeric(is.na(apertura)),
    missing_fmk = as.numeric(is.na(formacion_bruta_capital)),
    missing_consumo = as.numeric(is.na(consumo_gobierno)),
    missing_pbipc = as.numeric(is.na(pbi_p_c)),
    missing_infla = as.numeric(is.na(inflacion)),
    missing_crecimiento = as.numeric(is.na(crecimiento_pbi)),
  )

# Modelo logístico para predecir ingreso faltante
modelo_apertura <- glm(missing_apertura ~ formacion_bruta_capital + 
                          pbi_p_c + crecimiento_pbi + consumo_gobierno+
                          inflacion+ deuda_pbi +deficit_pbi,
                        data = datos_indicadores,
                        family = binomial())   

# Modelo logístico para predecir ingreso faltante
modelo_fmk <- glm(missing_fmk ~ apertura + 
                     pbi_p_c + crecimiento_pbi + consumo_gobierno+
                     inflacion+ deuda_pbi +deficit_pbi,
                   data = datos_indicadores,
                   family = binomial())
# Modelo para gasto_salud faltante
modelo_consumo <- glm(missing_consumo ~ formacion_bruta_capital + apertura + 
                         pbi_p_c +  consumo_gobierno+
                         inflacion+ deuda_pbi +deficit_pbi,
                       data = datos_indicadores,
                       family = binomial())

modelo_pbipc <- glm(missing_pbipc ~ formacion_bruta_capital + apertura + 
                       crecimiento_pbi + consumo_gobierno+
                       inflacion+ deuda_pbi +deficit_pbi,
                     data = datos_indicadores,
                     family = binomial())   

# Modelo logístico para predecir ingreso faltante
modelo_crecimiento <- glm(missing_crecimiento ~ formacion_bruta_capital + apertura + 
                             pbi_p_c +  consumo_gobierno+
                             inflacion+ deuda_pbi +deficit_pbi,
                           data = datos_indicadores,
                           family = binomial())
# Modelo para gasto_salud faltante
modelo_infla <- glm(missing_infla ~ formacion_bruta_capital + apertura + 
                       pbi_p_c + crecimiento_pbi + consumo_gobierno+
                        deuda_pbi +deficit_pbi,
                     data = datos_indicadores,
                     family = binomial())
summary(modelo_infla)

# Crear tabla de resumen
resumen_modelos <- bind_rows(
  broom::tidy(modelo_apertura) %>% mutate(modelo = "apertura faltante"),
  broom::tidy(modelo_fmk) %>% mutate(modelo = "fmk faltante"),
  broom::tidy(modelo_consumo) %>% mutate(modelo = "consumo faltante"),
  broom::tidy(modelo_pbipc) %>% mutate(modelo = "pbipc faltante"),
  broom::tidy(modelo_crecimiento) %>% mutate(modelo = "crecimiento faltante"),
  broom::tidy(modelo_infla) %>% mutate(modelo = "inflacion faltante")) %>%
  filter(p.value < 0.1) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

resumen_modelos %>%
  dplyr::select(modelo, term, estimate, std.error, p.value) %>%
  kable(caption = "Variables significativas para predecir faltantes (p < 0.1)") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  row_spec(which(resumen_modelos$p.value < 0.05), bold = TRUE)
# no podemos predecir faltantes


#==============================================================================#
# CONSIDERAMOS A LOS DATOS FALTANTES COMO MCAR
#==============================================================================#
# Borramos aquellas columnas
variables_na<-c("formacion_bruta_capital", "apertura", "pbi_p_c", "crecimiento_pbi", "consumo_gobierno", "inflacion")
datos_analisis_na
datos_filtrados <- datos_analisis_na %>% dplyr::filter(!if_any(all_of(variables_na), is.na))
nrow(datos_analisis_na)
nrow(datos_filtrados)



#==============================================================================#
# EXPORTAMOS LOS DATOS SIN LA COLUMNA INTERES REAL
#==============================================================================#
datos_analisis_na
exportar_data(data = datos_filtrados, nombre = "tabla_para_imputacion", carpeta = "processed")









