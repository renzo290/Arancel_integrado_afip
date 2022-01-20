
#Depuración de la base de Arancel Integrado Común desde la página de AFIP

#Paquetes

library(tidyverse)

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

nomenclador <- readr::read_delim("Bases_afip/nomenclador_20012022.txt", 
                                   delim = "@", escape_double = FALSE, col_names = FALSE, 
                                   trim_ws = TRUE)
View(nomenclador_20012022)

#Data Wrangling

Columnas <- c("ID", "SIM", "Alicuota_dex", )





