# Agregar funciones o parametros, no se bien como se hace. Eliminar cuando ya este

# Importacion de scipt anterior

# IMPORTANTE!!
# Agregar tabla con el nuevo link cuando el repositorio sea publico (borrar comentario cuando se haga)
tabla_imputar <- read.csv("https://raw.githubusercontent.com/PabloViera22/proyecto_tp_grupal/refs/heads/main/data/processed/tabla_para_imputacion.csv?token=GHSAT0AAAAAADPYWERTDILAZ5KNJANN25G42JI5MCQ") 

#======================================================================
#                   AÑO 2017: El mundo pre-pandemia                    #                   
#======================================================================

anio_2017 <- tabla_imputar %>% 
  filter(year == 2017)

#==========================================
# MEDIA, MEDIANA y ANALISIS DE DISPERSION # 
#==========================================

# Ordeno segun ingreso

orden_ingresos <- c("High income", 
                    "Upper middle income", 
                    "Lower middle income", 
                    "Low income")

# Analizaremos segun ingreso de cada pais

anio_2017 %>%
  group_by(income) %>%
  summarise(
    cantida_paises = n_distinct(country),
    mediana = median(deuda_pbi),
    media = mean(deuda_pbi),
    desvio = sd(deuda_pbi),
    iqr = IQR(deuda_pbi),
    minimo = min(deuda_pbi),
    maximo = max(deuda_pbi)
  ) %>% 
  mutate(income = factor(income, levels = orden_ingresos)) %>%
  arrange(income)

#==================================
# ANALISIS DEL BOXPLOT y OUTLIERS #
#==================================

# Outliers dentro de cada grupo

paises_outliers_2017 <- anio_2017 %>% 
  group_by(income) %>% 
  mutate(
    Q1 = quantile(deuda_pbi, 0.25),
    Q3 = quantile(deuda_pbi, 0.75),
    IQR = IQR(deuda_pbi),
    techo = Q3 + 1.5*IQR,
    piso = Q1 - 1.5*IQR
  ) %>% 
  filter(deuda_pbi > techo | deuda_pbi < piso) %>% 
  select(country, income, pbi_p_c, deuda_pbi) %>% 
  arrange(desc(deuda_pbi))
  
print("Para el año 2017, estos paises fueron outliers dentro de sus grupos: ")
print(paises_outliers_2017)

