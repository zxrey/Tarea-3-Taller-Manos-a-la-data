library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(dplyr)
library(lubridate)# para manejar fechas
library(stringr)
library(ggalluvial) #grafico del analisis exploratoria very interesting
library(parcats)
library(easyalluvial)
library(forcats)
library(tint)
install.packages("extrafont")
install.packages("ggthemes")
library(extrafont)
library(ggthemes)

setwd("E:/R clases/Taller manos a la data/datos/MDv01sesion3_parte-practica-master/1 Indecopi")
barreras <- readxl::read_xls("Data Logros al 31.12.2017.xls",skip = 1)
#fksabdkjasbkdjabs
barreras$`FECHA DE MODIF.`
table(barreras$`TIPO DE ACTUACIÓN`)
table(barreras$ENTIDAD)
unique(barreras$ENTIDAD)

barreras$TIPO_ENTIDAD <- "Otros"
barreras$TIPO_BARRERA <- "Otros"
barreras$TIPO_ADECUACION <- "Otros"

names(barreras)

View(barreras)

barreras <- barreras %>%
  mutate(TIPO_ENTIDAD = case_when(
                          grepl("MUNICIPALIDA", ENTIDAD) ~ "Gobierno Regional", #grepl cambiar nombres si: MUNICIPALIDA, 
                          grepl("UNIVERSIDAD", ENTIDAD) ~ "Universidad",       #pero este busca todo loque tenga esa parte del texto
                          grepl("MINISTERIO", ENTIDAD) ~ "Ejecutivo",
                          grepl("ASAMBLEA", ENTIDAD) ~ "Universidad",
                          grepl("SEGURO SOCIAL DE SALUD", ENTIDAD) ~ "Otras instituciones del Estado",
                          grepl("COLEGIO ODONTOLÓGICO", ENTIDAD) ~ "Colegios profesionales",
                          grepl("REGISTRO NACIONAL", ENTIDAD) ~ "Otras instituciones del Estado",
                          grepl("SUPERINTENDENCIA", ENTIDAD) ~ "Instituciones autónomas del Estado",
                          grepl("PRESIDENCIA DEL CONSEJO", ENTIDAD) ~ "Ejecutivo",
                          grepl("AUTORIDAD PORTUARIA", ENTIDAD) ~ "Otras instituciones del Estado",
                          grepl("DEFENSORIA DEL", ENTIDAD) ~ "Instituciones autónomas del Estado",
                          grepl("SERVICIO NACIONAL", ENTIDAD) ~ "Otras instituciones del Estado",
                          
                          )
  )

unique(barreras$`TIPO DE ACTUACIÓN`) #para observar los tipos de observaciones 
                              #y nos damos cuenta que se repite investigacion de oficio

barreras <- barreras %>%
  mutate(`TIPO DE ACTUACIÓN` = case_when(
    grepl("iNVESTIGACIÓN DE OFICIO", `TIPO DE ACTUACIÓN`) ~ "INVESTIGACIÓN DE OFICIO",
    TRUE~`TIPO DE ACTUACIÓN`
  )
  )

unique(barreras$TIPO_ENTIDAD) 

barreras <- barreras %>%
  mutate(TIPO_BARRERA = case_when(
    grepl("ORDEN", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD == "Gobierno Regional" ~ "Ordenanzas de gobiernos regionales"   
  )
  )

barreras <- barreras %>%
  mutate(TIPO_BARRERA = case_when(
    grepl("ORDEN", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Ordenanza de gobiernos regionales"  ,
    grepl("RESOLUC", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD %in% c("Universidad") ~ "Resolución universitaria",   
    grepl("DECRE", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Decreto de gobiernos regionales"  ,
    grepl("Ordenanza", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Ordenanza de gobiernos regionales"  ,
    grepl("ODENANZA", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Ordenanza de gobiernos regionales"  ,
    grepl("DECRETO SUPREMO", `NORMA QUE ESTABLECÍA LA BARRERA`) & TIPO_ENTIDAD %in% c("Ejecutivo") ~ "DECRETO SUPREMO"  ,
    grepl("PORTAL", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "Web"  ,
    grepl("RESOLUCIÓN MINISTERIAL", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "RESOLUCIÓN MINISTERIAL"  ,
    grepl("WEB INSTITUCIONA", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "Web"  ,
    grepl("RESOLUCIÓN DEFENSORIAL", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "RESOLUCIÓN DEFENSORIAL"  ,
    grepl("DECRETO SUPREMO", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "DECRETO SUPREMO"  ,
    grepl("RESOLUCIÓN JEFATURAL", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "RESOLUCIÓN JEFATURAL"  ,
    grepl("DIRECTIVA", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "Directiva"  ,
    grepl("REGLAMENTO", `NORMA QUE ESTABLECÍA LA BARRERA`) ~ "Reglamento"  ,
    
    
  )
  )






 
barreras2 <- barreras %>% filter(TIPO_BARRERA %in% c(NA))  #para fijarse que falta si no cambiaste algun dato
unique(barreras2$ENTIDAD)

#### Adecuación


barreras <- barreras %>%
  mutate(TIPO_ADECUACION = case_when(
    grepl("ORDEN", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Ordenanza de gobiernos regionales"  ,
    grepl("RESOLUC", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Universidad") ~ "Resolución universitaria",   
    grepl("DECRE", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Decreto de gobiernos regionales"  ,
    grepl("Ordenanza", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Ordenanza de gobiernos regionales"  ,
    grepl("ODENANZA", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Ordenanza de gobiernos regionales"  ,
    grepl("DECRETO SUPREMO", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Ejecutivo") ~ "DECRETO SUPREMO"  ,
    grepl("PORTAL", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "Web"  ,
    grepl("RESOLUCIÓN MINISTERIAL", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "RESOLUCIÓN MINISTERIAL"  ,
    grepl("WEB INSTITUCIONA", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "Web"  ,
    grepl("RESOLUCIÓN DEFENSORIAL", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "RESOLUCIÓN DEFENSORIAL"  ,
    grepl("DECRETO SUPREMO", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "DECRETO SUPREMO"  ,
    grepl("RESOLUCIÓN JEFATURAL", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "RESOLUCIÓN JEFATURAL"  ,
    grepl("DIRECTIVA", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "Directiva"  ,
    grepl("REGLAMENTO", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "Reglamento"  ,
    grepl("CONCLUSIÓN DEL PROCEDIMIENTO", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "CONCLUSIÓN DEL PROCEDIMIENTO"  ,
    grepl("Decreto de Alcaldía", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) & TIPO_ENTIDAD %in% c("Gobierno Regional") ~ "Decreto de gobiernos regionales"  ,
    grepl("RESOLUCIÓN DE GERENCIA GENERAL", `ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`) ~ "Reglamento"  ,
    
    
  )
  )

barreras2 <- barreras %>% filter(TIPO_ADECUACION %in% c(NA)) #para fijarse que falta
unique(barreras2$ENTIDAD)

barreras <- barreras %>%
  mutate(REVERSION = case_when(TIPO_ADECUACION==TIPO_BARRERA ~ "SI",
                               TRUE ~ "NO"))



barreras3 <- barreras[,c(2,10,11,12,13)]
names(barreras3)

barreras3 <-  barreras %>% select(REVERSION,`TIPO DE ACTUACIÓN`,TIPO_ENTIDAD,TIPO_BARRERA,TIPO_ADECUACION)
barreras3 <-  barreras %>% select(`TIPO DE ACTUACIÓN`,TIPO_ENTIDAD,TIPO_BARRERA,TIPO_ADECUACION,REVERSION)


barreras3 <- lapply(barreras3,function(x) as_factor(x))
barreras3 <- as.data.frame(barreras3)
p <-  alluvial_wide(barreras3, max_variables = 5)
parcats(p, marginal_histograms = TRUE, data_input = barreras3)


unique(barreras$ENTIDAD)



barreras %>%  filter(ENTIDAD == "UNIVERSIDAD NACIONAL DE SAN ANTONIO ABAD DE CUSCO")
g0 <- barreras %>%  filter(ENTIDAD == "UNIVERSIDAD NACIONAL DE SAN ANTONIO ABAD DE CUSCO", REVERSION =="SI") 


g1 <- barreras %>% count(`ADECUACIÓN (NORMA QUE ELIMINA BARRERA)`, sort = TRUE)
g2 <- barreras %>% count(TIPO_ENTIDAD, sort = TRUE)
g3 <- barreras %>% count(TIPO_BARRERA, sort = TRUE)
g4 <- barreras %>% count(TIPO_ADECUACION, sort = TRUE)
sapply(barreras, class)

g5 <- barreras %>% group_by(TIPO_ENTIDAD,TIPO_BARRERA)%>% count(TIPO_BARRERA, sort = TRUE) %>% arrange(TIPO_ENTIDAD,desc(n))

g6 <- barreras %>% group_by(ENTIDAD,TIPO_ENTIDAD) 
unique(barreras$TIPO_BARRERA)
unique(barreras$TIPO_ENTIDAD)
unique(barreras$`TIPO DE ACTUACIÓN`)
unique(barreras$MATERIA)


Conteo_tipo_actuacion <- barreras %>% group_by(ENTIDAD,`TIPO DE ACTUACIÓN`)  %>% count(ENTIDAD, sort = TRUE) %>% arrange(ENTIDAD,desc(n))

Conteo_tipo_actuacion_adecuacion <- barreras %>% group_by(TIPO_ENTIDAD,TIPO_BARRERA,TIPO_ADECUACION,)  %>% count(TIPO_ENTIDAD, sort = TRUE) %>% arrange(TIPO_ENTIDAD,desc(n))

unique(Conteo_tipo_actuacion$`TIPO DE ACTUACIÓN`)
unique(barreras$`TIPO DE ACTUACIÓN`)
windows()
  View() # mira la data

  
  
  #GRAFICOS
 
    
    
    theme_recession <- theme(
      rect = element_rect(fill = "grey92"),
      legend.key = element_rect(color = NA),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_text(color = "grey25"),
      plot.title = element_text(face = "italic", size = 16),
      
    )
  
    theme_tufte_recession <- theme_tufte() + theme_recession
  
    ggplot(g6, aes(TIPO_ENTIDAD, fill= TIPO_BARRERA))+
      geom_bar()+labs(x="Tipo de entidad",y="Numero de casos",title = "Numero de barreras por entidad") +
      theme_tufte_recession
  #grafico
 
  
  # Add a title and caption
  
    palette <- c(automatic = "#377EB8", manual = "#E41A1C")    
  z <-  theme_classic() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_text(color = "black"),
          axis.title = element_blank(),
          legend.position = "none")
  
  ggplot(g2, aes(x = n, y = TIPO_ENTIDAD)) +
    geom_point(size = 8) +
    geom_segment(aes(xend = 20, yend =  TIPO_ENTIDAD), size = 2) +
    geom_text(aes(label = round(n,1)), color = "white", size = 2.5) +
    scale_x_continuous("", expand = c(0,0), limits = c(-100,3000), position = "top") +
    scale_color_gradientn(colors = palette) +
    labs(title = "Numero de barreras segun entidad", caption = "Source: INDECOPI")+z
  
#EL RMD SE ENCUENTRA EN https://rpubs.com/zxrey/647356
