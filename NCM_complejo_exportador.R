#Entorno de trabajo-----
rm(list=ls())
gc()
#setwd("C:\Users\Ministerio\Desktop\Renzo\Proyectos RStudio\Arancel_integrado")

# Apertura de librerías
library(tidyverse)
library(readxl)
library(openxlsx)

#Descripción de NCM ---------
setwd("Auxiliar/")

variables_clasificaciones<-c("NCM",
                             "desc ncm",
                             "Rubros ICA" #Intercambio Comercial Argentino, INDEC
                             )

clasificaciones_NCM <- read_excel("Clasificaciones.xlsx", skip=1)[,variables_clasificaciones] 
names(clasificaciones_NCM)<- c("NCM","descripcion_NCM","rubro_ICA")
rm(variables_clasificaciones)
setwd("../")
#Complejos exportadores INDEC------
##El INDEC clasifica el 93% de las exportaciones según complejos exportadores. Puede ser útil para 
#el estudio ver los plazos de liquidación de los distintos complejos
#Se obtiene la clasificación en https://www.indec.gob.ar/ftp/cuadros/economia/nota_metodologica_complejos_exportadores_2018.pdf

###Sector cerealero

array_triguero<-c("1001","1101.00.10","1103.11.00","1108.11.00","1109.00.00","1901.20.00","1902","1904.30.00","1905","2302.30")
no_triguero<-c("1905.10.00")
array_list_triguero<-list(array_triguero,no_triguero)
names(array_list_triguero)<-c("Incluido","Excluido")
rm(array_triguero,no_triguero)

array_maicero<-c("0709.99.1","0710.40","1005","1102.20","1103.13","1104.23","1108.12","1515.21","1515.29","2005.80","2302.10")
array_arrocero<-c("1006")
array_cebada<-c("1003","1107","1901.90.10","2203.00.00")


###Sector hortícola
array_ajo<- c("0703.20","0712.90.10")
array_garbanzos<-c("0713.20")
array_porotos<-c("0708.20","0710.22","0713.3","2005.5")
array_papa<-c("0701","0710.10","1105.10","1105.20","1108.13","2004.10","2005.20","3504.00.30")
array_resto_horticola<-c("0702","0703.10","0703.90","0704","0705","0706","0707","0708.10","0708.90","0709.3","0709.4","0709.5","0709.6","0709.7",
                         "0709.91","0709.93","0709.99.90","0710.21","0710.29","0710.30","0710.80","0710.90","0711.40","0711.5","0711.90","0712.20",
                         "0712.3","0712.90.90","0713.10","0713.40","0713.50","0713.60","0713.90","0714","2001","2002","2003","2004","2005.10","2005.40","2005.60","2005.91",
                         "2005.99","2008.91","2009.50","2103.20")

###Sector frutícola
array_limon<-c("0805.50","2009.3","3301.13")

array_otros_citricos<-c("0805","2007.91","2008.30","2009.11","2009.12","2009.19","2009.2","3301.12.90","3301.19")
no_otros_citricos<-c("0805.50.00")
array_list_otros_citricos<-list(array_otros_citricos,no_otros_citricos)
names(array_list_otros_citricos)<-c("Incluido","Excluido")
rm(array_otros_citricos,no_otros_citricos)

array_uva<-c("0806.10","0806.20","2009.6","2204","2205","2208.20")
array_arandanos<-c("0809.2","0810.10","0810.20","0810.40","0811.10","0811.20","0812.10","2008.60","2008.80","2008.93","2009.81")
array_peras_manzanas<-c("0808.10","0808.30","0813.30","0813.40.10","2008.40.10","2008.40.90","2009.71","2009.79","2206.00.10")
array_resto_fruticola<-c("0801","0802","0803","0804","0808.40","0809.10","0809.30","0809.40","0810.30","0810.5","0810.6","0810.7","0810.8","0810.9",
                         "0811.90","0812.90","0813.10","0813.20","0813.40.90","0813.50","0814","2006","2007.10","2007.99.10","2007.99.2","2007.99.90",
                         "2008.19","2008.20","2008.50","2008.70","2008.97","2008.99","2009.41","2009.49","2009.89","2009.9",
                         "2009.10","2009.11","2009.12","2009.13","2009.89.19","2009.89.90","2009.90")

##Sector oleaginoso
array_soja<-c("1201.10","1201.90","1208.10","1507.10","1507.90","1520.00.10","2103.10","2302.50","2304.00","2905.45","2923.20",
              "3504.00.20","3826.00")
array_mani<-c("1203.30","1202.41","1202.42","1508.10","1508.90","2008.11","2305.00")
array_girasol<-c("1206.00","1512.11.10","1512.19","2306.30.10")
array_olivicola<-c("0709.92.00","0711.20","1509.10","1509.90","1510.00","2005.70")

###Té, mate, azúcar, miel y tabaco
array_te<-c("0902","2101.20.10")
array_mate<-c("0903","2101.20.20")

array_azucarero<-c("1212.93","1701","1704","2207","2940")
no_azucarero<-c("1701.12")
array_list_azucarero<-list(array_azucarero,no_azucarero)
names(array_list_azucarero)<-c("Incluido","Excluido")
rm(array_azucarero,no_azucarero)

array_miel<-c("0106.41","0409.00","1521.90")
array_tabacalero<-c("24")

###Sector bovino
array_carne_y_cuero<-c("0102.90.00","0201","0202","0206.10","0206.2","0210.20.00","0504.00.11","1502.10.1","1601","1602.10","1602.20",
                       "1602.50.00","2301.10","4101","4104","4107","4201","4202.11","4202.21","4202.31","4202.91","4203","4205","6403.12",
                       "6403.19","6403.20","6403.40","6403.51","6403.59","6403.91","6403.99","6405.10","6406.90.10")
array_lacteo<-c("0401","0402","0403","0404","0405","0406","1901.10.10","1901.90.20")

array_avicola<-c("0105","0106.3","0207","0209.90","0407","0408","0505","1602.3")
array_equino<-c("0101.2","0205")

array_pesquero<-c("03","0511.91.90","1504","1604","1605","2301.20")
no_pesquero<-c("0301")
array_list_pesquero<-list(array_pesquero,no_pesquero)
names(array_list_pesquero)<-c("Incluido","Excluido")
rm(array_pesquero,no_pesquero)

array_textil<-c(50:63) %>% 
  as.character()
no_textil<-c("5301.10","5301.21.10")
array_list_textil<-list(array_textil,no_textil)
names(array_list_textil)<-c("Incluido","Excluido")
rm(array_textil,no_textil)

array_forestal<-c("1209.99","1212.92","1212.94","1212.99.90","1301.20","1301.90","1302.19.30","1302.20","1302.31","1302.32.1","1302.32.20",
                  "1401.10","1401.20","1401.90","1511.10","1511.90","1513.1","1513.21.10","1513.21.20","1513.29.10","1513.29.20","1515.90.21",
                  "1515.90.22","1515.90.90","1521.10","1702.20","3201","3203.00.1","3301.29.12","3301.29.13","3301.29.15","3301.29.18",
                  "3301.29.19","3703","3704.00","3803.00","3805.10","3805.90","3806","3807.00","3912.11","3912.20","3912.31","3912.39.20",
                  "3912.39.30","3912.39.90","3912.90","4001","44","45","46","47","48","9401.30.10",
                  "9401.40.10","9401.6","9401.90.10","9403.3","9403.4","9403.5","9403.6","9403.90")
array_petrolero_petroquimico<-c("2709","2710","2711","2713","2901","2902.1","2902.2","2902.3","2902.4","2902.5","2902.6")
temp<-c(12:23)
temp2<-paste0("2903.",temp)
array_petrolero_petroquimico<-c(array_petrolero_petroquimico,temp2,"2905.11","2905.12.20","2905.13","2905.14.10",
                                "2905.14.20","2905.31","2905.32","2907.11","2909.19.10","2912.11","2912.12","2914.11","2914.12","2914.13","2915.21",
                                "2915.31","2915.32","2916.11.20","2916.12","2916.14.10","2917.12.10","2917.14","2917.35","2917.37","2918.14","2921.22",
                                "2922.11","2922.12","2922.15","2926.10","2929.10.2","2933.71","3102.10","3102.21","3102.30","3105.30","3901","3902","3903","3904","3905",
                                "3907.20.3","3907.6","3908.10.13","3908.10.14","3908.10.23","3908.10.24","4002.11","4002.19.40","3902.20","4002.49",
                                "4002.59","4002.70","9998.01.00")
rm(temp,temp2)
array_farmaceutico<-c("2916.39.20","2918.21","2918.22.1","2918.22.20","2918.23","2922.41","2922.42","2922.49.20","2922.49.6","2923.10",
                      "2923.30","2923.40","2923.90.60","2923.90.90","2924.11","2924.12","2924.19","2924.23","2924.24","2924.25",
                      "2924.29","2932.20","2933.11","2933.19","2933.21","2933.52","2933.53","2933.55","2933.59","2933.69","2934.30","2935",
                      "2936.21","2936.22","2936.23","2936.24","2936.25","2936.26","2936.27","2936.28","2936.29","2937","2938","2939","2941",
                      "30")

### Sector minero metalífero y litio
array_litio<-c("2825.20","2827.39.60","2833.29.20","2834.29.40","2836.91.00","8506.50")
array_siderurgico<-c("2601","2619.00.00","72","73")
array_aluminio<-c("2606","2620.40.00","2818.20","2818.30","76","8544.19.10")

array_oro_y_plata<-c("2616","7106","7107","7108","7112.30.10","7113.1","7114.1")
no_oro_y_plata<-c("7108.20.00")
array_list_oro_y_plata<-list(array_oro_y_plata,no_oro_y_plata)
names(array_list_oro_y_plata)<-c("Incluido","Excluido")
rm(array_oro_y_plata,no_oro_y_plata)


array_plomo<-c("2607","2620.2","78")
array_otros_minerales_metaliferos<-c("2602","2603","2604","2605")
temp<-c(2607:2615) %>% 
  as.character()
temp2<-c(8101:8109) %>% 
  as.character()
array_otros_minerales_metaliferos<-c(array_otros_minerales_metaliferos,temp,temp2,
                                     "2617","2620","2621","75","78","79","80","8111","74")

array_automotriz<-c("4011.10","4011.20","4012","4013.10","7009.10","8301.20","8302.30","8407.34.90","8408.20","8409.91","8409.99","8413.30",
                    "8415.20","8421.23","8421.31","8483.10","8483.20","8483.30","8483.40","8483.50","8483.60","8483.90",
                    "8511","8512","8527.2","8702","8703","8704","8706","8707","8708","8716.3","8716.40",
                    "8716.80","8716.90","9029.20.10","9031.80.40","9032.89.2","9401.20")
no_automotriz<-c("8483.10.50","8512.10.00","8704.10","8707.90.10","8708.29.1","8708.30.1","8708.40.1","8708.50.1","8708.50.91","8708.70.10","8708.94.1")
array_list_automotriz<-list(array_automotriz,no_automotriz)
names(array_list_automotriz)<-c("Incluido","Excluido")
rm(array_automotriz,no_automotriz)


digitos<-c(2:8)


df_NCM_total<-clasificaciones_NCM %>%
  mutate(NCM_char=ifelse(NCM<10000000,as.character(paste0("0",NCM)), 
                         as.character(NCM)),
         complejo_INDEC=as.character(NA)
  )

total_complejos<-data.frame(ls(pattern="array*")) %>% 
  rename(nombre_abanico=1) %>% 
  mutate(complejo=gsub(pattern="array_",replacement="",nombre_abanico),
         complejo=gsub(pattern="list_",replacement="",complejo),
         complejo=gsub(pattern="\\_",replacement=" ",complejo)
  )

solo_plomo<-total_complejos %>% 
  subset(grepl(pattern="plomo",nombre_abanico))
solo_lista<-total_complejos %>% 
  subset(grepl(pattern="array_list*",nombre_abanico))
solo_abanico<-total_complejos %>% 
  subset(!grepl(pattern="plomo",nombre_abanico)) %>% 
  subset(!grepl(pattern="array_list*",nombre_abanico))
#Hay que primero lanzar la función en los complejos en que se borran nomencladores, si no por ejemplo las exportaciones de limón, que se borran de 
#otros cítricos, también se borrarían en el complejo limonero. 

total_complejos<-solo_lista %>%
  rbind(solo_plomo) %>% 
  rbind(solo_abanico)

rm(solo_lista,solo_abanico,solo_plomo)

complejos_INDEC<-function(indata,index){
  if(grepl(pattern="array_list*",total_complejos[[index,1]])){
    
    input_array<-get(total_complejos[[index,1]])
    
    sin_puntos<-input_array[[1]] %>% 
      gsub(pattern="\\.",replacement="")
    sin_puntos_no_es<-input_array[[2]] %>%
      gsub(pattern="\\.",replacement="") 
    
    for(i in digitos){
      indata<-indata %>% 
        mutate(verif=substr(NCM_char,start=1,stop=i),
               complejo_INDEC=ifelse(is.na(complejo_INDEC) & (verif %in% sin_puntos), total_complejos[[index,2]], 
                                     complejo_INDEC),
               complejo_INDEC=ifelse(verif %in% sin_puntos_no_es, NA, 
                                     complejo_INDEC)
               
        )
    }
    
  }
  if(!grepl(pattern="array_list*",total_complejos[[index,1]])){
    input_array<-get(total_complejos[[index,1]])
    sin_puntos<-input_array %>% 
      gsub(pattern="\\.",replacement="")
    print(sin_puntos)
    for(i in digitos){
      indata<-indata %>% 
        mutate(verif=substr(NCM_char,start=1,stop=i),
               complejo_INDEC=ifelse((is.na(complejo_INDEC) & verif %in% sin_puntos), total_complejos[[index,2]], 
                                     complejo_INDEC)
               
        )
    }
  }
  rm(input_array)
  outdata<-indata
}

for(i in 1:nrow(total_complejos)){
  df_NCM_total<-complejos_INDEC(df_NCM_total,i)
}


sector_cerealero<-c("maicero","triguero","cebada","arrocero")
sector_oleaginoso<-c("soja","girasol","mani","olivicola")
sector_bovino<-c("lacteo","carne y cuero")
sector_minero<-c("oro y plata","siderurgico","aluminio","litio","plomo","otros minerales metaliferos")
sector_fruticola<-c("uva","limon","peras manzanas", "otros citricos","arandanos","resto fruticola")
sector_horticola<-c("porotos","papa","ajo","garbanzos","resto horticola")
df_NCM_total<-df_NCM_total %>% 
  mutate(complejo_INDEC=ifelse(is.na(complejo_INDEC), "Otros complejos", 
                               complejo_INDEC),
         sector_INDEC=ifelse(complejo_INDEC %in% sector_cerealero, "cerealero", 
                             ifelse(complejo_INDEC %in% sector_oleaginoso, "oleaginoso", 
                                    ifelse(complejo_INDEC %in% sector_bovino, "bovino", 
                                           ifelse(complejo_INDEC %in% sector_minero, "minero", 
                                                  ifelse(complejo_INDEC %in% sector_fruticola, "fruticola", 
                                                         ifelse(complejo_INDEC %in% sector_horticola, "horticola", 
                                                                complejo_INDEC)
                                                  )
                                           )
                                    )
                             )
         )
  ) %>% 
  select(-c(verif))

table(df_NCM_total$complejo_INDEC)  

table(df_NCM_total$sector_INDEC)  


sin_complejo<-df_NCM_total %>% 
  subset(is.na(complejo_INDEC))
head(sin_complejo) #Verificamos que todos los NCM tienen un complejo exportador asignado 
rm(sin_complejo)
rm(list=ls(pattern="sector*"))
rm(list=ls(pattern="array*"))
rm(i,digitos,temp,temp2)
rm(clasificaciones_NCM)
##Así, df_NCM_total tiene la clasificación detallada con NCM a 8 dígitos, el complejo exportador y el sector exportador
    #según INDEC, y el rubro de Intercambio Comercial Argentino.  Se puede luego fusionar con el df nomenclador
