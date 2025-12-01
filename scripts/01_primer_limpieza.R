# ESTO CREO QUE ESTA TODO HECHO EN LA 00_IMPORTAR
source(here::here("config", "parametros.R"))
source(here::here("funciones", "funciones_para_importar_exportar.R"))
archivos_en_data()
#==============================================================================#
# LE SACAMOS AQUELLAS FILAS QUE NO POSEAN VALOR Y COLUMNAS EN LO ABSOLUTO PARA ANALISIS.
primer_limpieza<-cargar_datos(nombre_archivo = "tabla_completa.csv", carpeta = "processed")
unique(primer_limpieza$income)

# VAMOS A SACAR AQUELLAS FILAS QUE SE REFIERAN EN "REGION" A AGREGADOS (COMO CONTINENTE, ZONA COMERCIAL), 
#A "NA" Y NO CLASIFICADOS, QUE ATROFIAN EL ANALISIS.
primer_limpieza_income<-primer_limpieza%>%dplyr::filter( !is.na(income),!income %in% c("Not classified", "Aggregates"))

# SACAMOS ALGUNAS COLUMNAS QUE NO SERÁN TENIDAS EN CUENTA Y AGREGAMOS DEUDA AL CUADRADO
colnames(primer_limpieza_income)
primer_limpieza_filtro<-primer_limpieza_income%>%
  dplyr::select(-c(interes_real,deuda_millones,deficit_millones, deuda_per_capita))%>%
  dplyr::mutate(deuda_cuadrada=deuda_pbi^2)
  
#==============================================================================#
exportar_data(data = primer_limpieza_filtro,nombre = "tabla_primer_limpieza", carpeta = "processed")























