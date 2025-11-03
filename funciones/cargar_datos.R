indicadores<-c("GC.DOD.TOTL.GD.ZS", "NY.GDP.MKTP.KD.ZG", "GC.TAX.TOTL.GD.ZS", "FP.CPI.TOTL.ZG")
datos<- WDI(country = "AR",indicator = indicadores,
            start = 1960, 
            end = 2023)
datos%>%rename(deuda_del_estado=GC.DOD.TOTL.GD.ZS,
               PBI_crecimiento_porc=NY.GDP.MKTP.KD.ZG,
               recaudacion_impuesto=GC.TAX.TOTL.GD.ZS,
               )
diccionario_wdi <- WDIsearch()

# 2. Definir el código críptico de la columna que quieres entender
codigo_a_traducir <- "FP.CPI.TOTL.ZG" 

# 3. Filtrar el diccionario para obtener la descripción
significado_columna <- diccionario_wdi %>%
  filter(indicator == codigo_a_traducir) %>%
  select(indicator, name)
significado_columna         
