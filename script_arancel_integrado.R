
#####Depuracion de la base de Arancel Integrado Comun desde la pagina de AFIP####

####Paquetes####

library(tidyverse)
library(openxlsx)
library(readxl)

####Directorio de trabajo y archivos####

#Para chequear y configurar directorio de trabajo
#getwd()
#setwd("C:\Users\Ministerio\Desktop\Renzo\Proyectos RStudio\Arancel_integrado")

#Si no esta creada, se crea una carpeta para guardar las bases de AFIP
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
rm(fecha,ruta_base)
                                 
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

#Se modifican los derechos de hidrocarburos que tienen derechos móviles

hidrocarburos <- read_excel("Auxiliar/hidrocarburos.xlsx", 
                            sheet = "posiciones")

nomenclador <- left_join(nomenclador, hidrocarburos, by = "NCM")
nomenclador <- nomenclador %>% 
  filter(is.na(Hidrocarburos)) %>% 
  select(1:10)
rm(hidrocarburos)

hidrocarburos_info <- read_excel("Auxiliar/hidrocarburos.xlsx", 
                            sheet = "info")
nomenclador <- rbind(nomenclador, hidrocarburos_info)

#Se suman decretos de beneficios en dexs, automotriz y pymes. Hay que corregirles el formato

#Autopartes
Autopartes_incremental <- read_excel("Auxiliar/Autopartes_incremental.xlsx")

Autopartes_incremental <- Autopartes_incremental %>% 
  mutate(NCM_correcto=ifelse(substr(NCM,start=5,stop=5)==".",1,0)) #Los NCM con buen formato tienen siempre en el quinto dígito un punto

solo_correctos<-Autopartes_incremental%>%
  subset(NCM_correcto==1) %>% 
  select(NCM, Autopartes_incremental)

correccion_NCM<-Autopartes_incremental %>%
  subset(NCM_correcto==0)%>% #Los que tienen mal formato es porque excel pensó que eran fechas; por eso usamos as.Date()
  mutate(NCM=as.numeric(NCM), 
         NCM_corregido=as.Date(NCM,origin = "1899-12-30"), 
         NCM_corregido=gsub("-",".",NCM_corregido))%>%
  select(NCM_corregido, Autopartes_incremental) %>% 
  rename(NCM = NCM_corregido)

Autopartes_incremental_ok<-rbind(solo_correctos,correccion_NCM)%>%
  arrange(NCM)

rm(Autopartes_incremental, correccion_NCM, solo_correctos)

#Autos
Autos_incremental <- read_excel("Auxiliar/Autos_incremental.xlsx") #No se corrige porque está OK

#PYMES
PYMES <- read_excel("Auxiliar/Decreto_PyMEs.xlsx")

PYMES <- PYMES %>% 
  mutate(NCM_correcto=ifelse(substr(NCM,start=5,stop=5)==".",1,0)) #Los NCM con buen formato tienen siempre en el quinto dígito un punto

solo_correctos<-PYMES%>%
  subset(NCM_correcto==1) %>% 
  select(NCM, Beneficio_PYME)

correccion_NCM<-PYMES %>%
  subset(NCM_correcto==0)%>% #Los que tienen mal formato es porque excel pensó que eran fechas; por eso usamos as.Date()
  mutate(NCM=as.numeric(NCM), 
         NCM_corregido=as.Date(NCM,origin = "1899-12-30"), 
         NCM_corregido=gsub("-",".",NCM_corregido))%>%
  select(NCM_corregido, Beneficio_PYME) %>% 
  rename(NCM = NCM_corregido)

PYMES_ok<-rbind(solo_correctos,correccion_NCM)%>%
  arrange(NCM)

rm(PYMES, correccion_NCM, solo_correctos)

#Agregamos columnas al nomenclador

nomenclador <- left_join(nomenclador, Autopartes_incremental_ok, by = "NCM")
nomenclador <- left_join(nomenclador, Autos_incremental, by = "NCM")
nomenclador <- left_join(nomenclador, PYMES_ok, by = "NCM")

nomenclador$Autopartes_incremental[is.na(nomenclador$Autopartes_incremental)] <- 0
nomenclador$Autos_incremental[is.na(nomenclador$Autos_incremental)] <- 0
nomenclador$Beneficio_PYME[is.na(nomenclador$Beneficio_PYME)] <- 0

#Se corrigen decimales de columna "Derechos_Exportación"
nomenclador<- nomenclador%>%
  mutate(Derecho_exportacion=gsub("[.]",",",Derecho_exportacion)) #Cambia puntos por comas en columna Derecho_exportacion

table(nomenclador$Derecho_exportacion)

####Tablas resumidas####

#Agrupo a 8 dígitos -> No considera los dexs variables

ncm <- nomenclador %>% 
  group_by(NCM) %>% 
  summarise(cantidad = n(),
            dex = mean(Derecho_exportacion))

sum(ncm$cantidad)

#Tablas

table(nomenclador$Derecho_exportacion)
table(nomenclador$Derecho_impo_extrazona)
table(nomenclador$Derecho_impo_intrazona)

####Se exportan el archivo en otro formato####

if(!file.exists("Base_depurada")) {
  dir.create("Base_depurada")
}

ruta_depurada<-"Base_depurada/"

#Formato Excel - Separamos decimales con coma para que queden todas las col iguales

write.xlsx(nomenclador, file = paste0(ruta_depurada, "nomenclador_",dia,"_",
                                      mes,"_",anio,".xlsx"),dec = ".", overwrite = T) 

rm(list = ls())
