# Agregar funciones o parametros, no se bien como se hace. Eliminar cuando ya este

# Importacion de scipt anterior

# IMPORTANTE!!
# Agregar tabla con el nuevo link cuando el repositorio sea publico (borrar comentario cuando se haga)
tabla_imputar <- read.csv("https://raw.githubusercontent.com/PabloViera22/proyecto_tp_grupal/refs/heads/main/data/processed/tabla_para_imputacion.csv?token=GHSAT0AAAAAADPYWERTI4P2EUDVTYSAAM4E2JI3KJQ") 

#==================
# MEDIA Y MEDIANA #
#==================

# Media y mediana de la deuda segun region

tabla_imputar %>% 
  group_by(region) %>% 
  summarise(
    cantida_paises = n_distinct(country),
    mediana_deuda_pbi = median(deuda_pbi),
    media_deuda_pbi = mean(deuda_pbi)
  ) %>% 
  arrange(desc(mediana_deuda_pbi))
 
# Media y mediana de la deuda segun ingreso

# Ordeno segun ingreso

orden_ingresos <- c("High income", 
                    "Upper middle income", 
                    "Lower middle income", 
                    "Low income")

tabla_imputar %>% 
  group_by(income) %>% 
  summarise(
    cantida_paises = n_distinct(country),
    mediana_deuda_pbi = median(deuda_pbi),
    media_deuda_pbi = mean(deuda_pbi)
  ) %>% 
  mutate(income = factor(income, levels = orden_ingresos)) %>%
  arrange(income)


