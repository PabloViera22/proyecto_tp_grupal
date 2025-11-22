datos_graficos<-read_csv(ruta_completa)
datos_graficos

# Los transformamos 
datos_milmillones<-datos_graficos %>% mutate(pbi_constante_en_millones=pbi_constante/ 1e9)
datos_2024<-datos_milmillones%>% filter(year==2024)



