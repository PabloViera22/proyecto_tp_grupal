# Para tener las mismas versiones de paquetes ***EN DUDA***
#install.packages("renv") #solo una vez
#renv::init() # Crea una carpeta para este proyecto
#renv::snapshot() #Para cuando se termine de actualizar los paquetes
#renv::restore() #Para cuando se abra el proyecto por primera vez
# =============================================================================#
# CONFIGURACIÓN GLOBAL DEL PROYECTO
# =============================================================================#
# LIMPIAR ENTORNO
rm(list = ls())

# CONFIGURAR OPCIONES GLOBALES
options(stringsAsFactors = FALSE)
options(scipen = 999)  # Evitar notación científica
options(digits = 2) # Decimales a mostrar 

# LIBRERÍAS DEL PROYECTO
library(here)
library(WDI)
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

# DEFINIR DIRECTORIO DE MANERA RERODUCIBLE
if (!exists("proyecto_tp_grupal")) {
  proyecto_tp_grupal <- here::here()  # Usa el paquete 'here'
  # Alternativa manual:
  # proyecto_tp_grupal <- dirname(rstudioapi::getSourceEditorContext()$path)
}

# RUTAS PRINCIPALES
dir_data_raw <- file.path(proyecto_tp_grupal, "data", "raw")
dir_data_clean<-file.path(proyecto_tp_grupal, "data", "clean")
dir_data_processed <- file.path(proyecto_tp_grupal, "data", "processed")
dir_outputs_figures <- file.path(proyecto_tp_grupal, "outputs", "figures")
dir_outputs_tables <- file.path(proyecto_tp_grupal, "outputs", "tables")

# CREAR DIRECTORIOS SI NO EXISTEN
dirs_crear <- c(dir_data_raw, dir_data_clean,dir_data_processed, 
                dir_outputs_figures, dir_outputs_tables)
for (dir in dirs_crear) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
}

# PARAMETROS DE ANÁLISIS
# Todavía Nada

# Funciones para mensajes consistentes
mensaje_exito <- function(texto) {
  cat("
 ✅
 ", texto, "\n")
}
mensaje_proceso <- function(texto) {
  cat("
 🔄
 ", texto, "...\n")
}
mensaje_exito("Configuración cargada correctamente")

##Carga de configuración en cada script
# Inicio de cada script



