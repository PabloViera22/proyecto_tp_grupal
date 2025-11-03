indicadores<-c("Deuda_del_Gob"="GC.DOD.TOTL.GD.ZS", 
               "Tasa_Crecimiento_PBI"="NY.GDP.MKTP.KD.ZG", 
               "Recaudacion"="GC.TAX.TOTL.GD.ZS", 
               "IPC"="FP.CPI.TOTL.ZG")
datos<- WDI(country = "AR",indicator = indicadores,
            start = 1960, 
            end = 2023)
datos

