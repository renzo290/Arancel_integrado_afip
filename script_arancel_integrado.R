
#####Depuración de la base de Arancel Integrado Común desde la página de AFIP####

#Paquetes

library(tidyverse)
require(openxlsx)

#Si no está creada, se crea una carpeta para guardar las bases de AFIP

if(!file.exists("Bases_afip")) {
  dir.create("Bases_afip")
}

#Se descargan los archivos de la página de AFIP y se carga el arancel integrado

download.file(
  url = "https://www.afip.gob.ar/aduana/arancelintegrado/archivos/arancel.zip", 
  destfile = "Bases_afip/arancel.zip", mode='wb'
)

unzip(zipfile = 'Bases_afip/arancel.zip', exdir = "Bases_afip")
unlink('Bases_afip/arancel.zip')

nomenclador <- readr::read_delim("Bases_afip/nomenclador_27012022.txt", 
                                   delim = "@", escape_double = FALSE, col_names = FALSE, 
                                   trim_ws = TRUE)
View(nomenclador)

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
         Derecho_impo_EM = as.numeric(Derecho_impo_EM),
         Reintegro_extrazona = as.numeric(Reintegro_extrazona),
         Reintegro_intrazona = as.numeric(Reintegro_intrazona),
         Codigo_unidad_derecho_especifico = as.numeric(Codigo_unidad_derecho_especifico))

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

####Se exportan el archivo en otro formato####

if(!file.exists("Base_depurada")) {
  dir.create("Base_depurada")
}

#Formato Excel

write.xlsx(nomenclador, file = "Base_depurada/nomenclador.xlsx", dec = ",",
           overwrite = T) 

#Formato csv

write.csv(nomenclador, file = "Base_depurada/nomenclador.csv") 


