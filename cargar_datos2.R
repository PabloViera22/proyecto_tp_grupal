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









