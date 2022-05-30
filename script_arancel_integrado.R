
#####Depuracion de la base de Arancel Integrado Comun desde la pagina de AFIP####

#Paquetes

library(tidyverse)
library(openxlsx)

#setwd("C:/Users/Ministerio/Documents/arancel_integrado_afip")
#Si no esta creada, se crea una carpeta para guardar las bases de AFIP

#Para chequear y configurar directorio de trabajo
#getwd()
#setwd("C:\Users\Ministerio\Desktop\Renzo\Proyectos RStudio\Arancel_integrado")

if(!file.exists("Bases_afip")) {
  dir.create("Bases_afip")
}

#Se descargan los archivos de la pagina de AFIP y se carga el arancel integrado

download.file(
  url = "https://www.afip.gob.ar/aduana/arancelintegrado/archivos/arancel.zip", 
  destfile = "Bases_afip/arancel.zip", mode='wb'
)

unzip(zipfile = 'Bases_afip/arancel.zip', exdir = "Bases_afip")
unlink('Bases_afip/arancel.zip') # Borra el archivo zip

fecha<-Sys.Date()
anio<-substr(fecha,start=1,stop=4)
mes<-substr(fecha,start=6,stop=7)
dia<-substr(fecha,start=9,stop=10)
ruta_base<-"Bases_afip/nomenclador_"

nomenclador <- readr::read_delim(paste0(ruta_base,dia,mes,anio,".txt"), 
                                        delim = "@", escape_double = FALSE, col_names = FALSE, 
                                        trim_ws = TRUE) #El archivo de AFIP lleva la fecha de hoy
                                   
View(nomenclador)
rm(fecha,anio,mes,dia,ruta_base)
                                 
####Data Wrangling - Modificamos el formato####

Columnas <- c("ID", "SIM", "Derecho_exportacion", "Reintegro_extrazona", "Derecho_impo_extrazona",
              "Reintegro_intrazona", "Derecho_impo_intrazona", "Derecho_impo_EM", 
              "Codigo_unidad_estadistica", "Codigo_unidad_derecho_especifico",
              "Descripcion")

colnames(nomenclador) <- Columnas
rm(Columnas)

nomenclador <- nomenclador %>% 
  mutate(Caracteres = nchar(SIM),
         Capitulo = substr(SIM, 1, 2),
         NCM_4 = substr(SIM, 1, 4),
         NCM = substr(SIM, 1, 10)) %>% 
  filter(Caracteres == 15) %>% 
  filter(Capitulo != "00")

nomenclador <- select(nomenclador, SIM, Capitulo, NCM_4, NCM, Descripcion, Derecho_exportacion, 
                      Reintegro_intrazona, Reintegro_extrazona,
                      Derecho_impo_intrazona, Derecho_impo_extrazona)

nomenclador <- nomenclador %>% 
  mutate(Derecho_exportacion = as.numeric(Derecho_exportacion),
         Derecho_impo_extrazona = as.numeric(Derecho_impo_extrazona),
         Derecho_impo_intrazona = as.numeric(Derecho_impo_intrazona),
         Reintegro_extrazona = as.numeric(Reintegro_extrazona),
         Reintegro_intrazona = as.numeric(Reintegro_intrazona),
         )

#Se modifica el encoding de la columna "Descripcion"

Encoding(nomenclador$Descripcion) <- "latin1" 
#Tablas resumidas

table(nomenclador$Derecho_exportacion)
table(nomenclador$Derecho_impo_extrazona)
table(nomenclador$Derecho_impo_intrazona)

#Agrupo a 8 dígitos

ncm <- nomenclador %>% 
  group_by(NCM) %>% 
  summarise(cantidad = n(),
            dex = mean(Derecho_exportacion))

sum(ncm$cantidad)

#Se modifican observaciones don datos faltantes

faltantes_nomenclador <- nomenclador %>% 
  filter(is.na(Derecho_exportacion))

head(faltantes_nomenclador)

faltantes_nomenclador <- faltantes_nomenclador%>%
  mutate(Derecho_exportacion=ifelse(SIM=="2008.19.00.110W",0,     Derecho_exportacion), 
         Reintegro_intrazona=ifelse(SIM=="2008.19.00.110W",3.25,  Reintegro_intrazona),
         Reintegro_extrazona=ifelse(SIM=="2008.19.00.110W",3.25,  Reintegro_extrazona),
         Derecho_impo_intrazona=ifelse(SIM=="2008.19.00.110W",0,  Derecho_impo_intrazona),
         Derecho_impo_extrazona=ifelse(SIM=="2008.19.00.110W",14, Derecho_impo_extrazona)
         )

nomenclador <- nomenclador %>%
  filter(!is.na(Derecho_exportacion))%>%
  rbind(faltantes_nomenclador)

rm(faltantes_nomenclador)

####Se exportan el archivo en otro formato####

if(!file.exists("Base_depurada")) {
  dir.create("Base_depurada")
}

#Formato Excel

write.xlsx(nomenclador, file = "Base_depurada/nomenclador.xlsx", dec = ",",
           overwrite = T) 

#Formato csv

write.csv(nomenclador, file = "Base_depurada/nomenclador.csv") 


