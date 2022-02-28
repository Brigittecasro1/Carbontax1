#########################################################
##########        Brigitte Castañeda           ##########
#########               CARBON TAX             ##########
#########################################################
rm(list = ls())

## Carga de librerias
library()

lista = c('readr','readxl','lubridate', 'ggplot2','hrbrthemes','dplyr','plotly','tseries','fUnitRoots','forecast','FitAR')

for (i in 1:length(lista) ) {
  if(lista[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(lista[i])
  }
  lapply(lista[i], library, character.only = TRUE)
  
}
library(sqldf)
library(tidyr)
library(readr)
library(readxl)
library(haven)
library(write.xlsx)
library(openxlsx)


# Direcccion del archivo
setwd("C:/Users/USER/OneDrive - Universidad de los Andes (1)/2022-10/R- CT/Carbontax1")

#### 1- Carga de archivos
source("read_excel_title.R")

Data  = read_excel_title ("Data.xlsx", sheet =1 ) # datos de Wordl bank
IEA <- read_csv_title("IEA.csv") #IEA International Energy Agency
carbontax <- read_excel_title("carbontax.xlsx", sheet = 1) # carbon tax database
groupincome <- read_csv_title("groupincome.csv") # Group income
Data_Climate <-  read_csv_title("Data_Climate.csv") # Tabla del clima
DPI2020 <- read_dta("DPI2020.dta") #institutional data
OECD <- read_excel_title("OECD.xlsx", sheet = 1) # tabla OECD
carbonpricing <- read_excel("carbonpricing.xlsx")# tabla de precios del carbono
sharecarbontax <- read_excel("sharecarbontax.xlsx")# tabla de share

#tabla de indicadores DATA WB
Data <- gather(Data, year, value, Y2001:Y2020, factor_key=TRUE)
Data <- spread( Data[,c(1,2,4,5,6)], SERIES_CODE,value) #### excluimos la columna tres ya que esta se repite con la 4
str(Data)

#tabla de indicadores IEA
IEA <- gather(IEA, year, value, Y2001:Y2020, factor_key=TRUE)
IEA <- spread( IEA, SERIES_NAME1,value)
str(IEA)
#View(IEA_wide)
colnames(IEA)[3] = "YEAR1"
Data_ = Data
##########1.1-Unir tabla datawide y IEA_wide###########
Data = sqldf("
                  SELECT * FROM Data A
                  INNER JOIN (SELECT * FROM IEA B)  
                  ON COUNTRY_NAME = COUNTRY AND YEAR1 = year
                  ")

#View(Data)
unique(Data$COUNTRY_NAME)
gc()


###########1.2 - cambio de nombres a variables ##############
Data = Data[, c(1:43, 47:50)]
Lista1  = c(	'COUNTRY', 'COUNTRY_CODE', 'year', 'corruption_control', 'access_cooking', 'electricity_dirty', 'renewable_consumption', 'fuel_energy_consumption', 'energy_use', 'CO2emissions', 'CO2_fuel_consumption', 'greenhouse_emissions', 'CO2_manufacturing', 'CO2_others', 'CO2_transport', 'ryd_expenditure', 'gov_Effectiveness', 'carbon_damage', 'coal_rents', 'inflation_GDP', 'GDP_current', 'GDP_growth', 'GDP_capita', 'GDP_cap_growth', 'oil_rents', 'natresources_rents', 'GNI_current', 'GNI_growth', 'GNI_capita', 'GNI_cap_growth', 'rule_law', 'regulatory_quality', 'employ_agriculture', 'employ_vulnerable', 'employ_industry', 'employment_services', 'labor_total', 'unemployment_advancededu', 'unemployment_basicedu', 'population_growth', 'population', 'fuel_import', 'fuel_export', 'consumption_petroleum', 'Exports_petroleum', 'Imports_petroleum', 'Production_petroleum')
colnames(Data) = Lista1

Data$year <- as.numeric(lapply(Data$year, gsub, pattern='Y', replacement='') )


################################## ### here we got 47 Variables ####
#####1.3 agrego carbon tax #######
colnames(carbontax)
#### FROM LONG TO WIDE
carbontax <- spread( carbontax, TYPE, YEARI)

colnames(carbontax)[5] = 'CARBON_TAX'
colnames(carbontax)[6] = 'ETS_TAX'

##### INDIVIDUALIZANDO LOS TIPOS DE IMPUESTOS
carbontax_ct = sqldf("SELECT * FROM carbontax WHERE  CARBON_TAX IS NOT NULL  ")
carbontax_et = sqldf("SELECT * FROM carbontax WHERE  ETS_TAX IS NOT NULL  ")

### sELECCIONO VARIABLES DE INTERES
carbontax_ct = carbontax_ct[,c(1,5)]
carbontax_et = carbontax_et[,c(1,6)]
Data$year = unlist(Data$year)
 
################ Join Carbon Tax #### Data 47 y carbontax_et 6 
Data1 = sqldf("SELECT * , 
                  CASE WHEN YEAR  >= CARBON_TAX  then 1 else 0 end  HAS_CARB_TAX
                  FROM Data 
               LEFT JOIN (SELECT * FROM carbontax_ct)   
                  ON COUNTRY = JURISDICTION 
                ")
Data1 = Data1[,c(1:47,50)]
 
################### Joining the ets 
Data1 = sqldf("SELECT * , 
                  CASE WHEN YEAR  >= ETS_TAX  then 1 else 0 end  HAS_ETS_TAX
                  FROM Data1 
               LEFT JOIN (SELECT * FROM carbontax_et)   
                  ON COUNTRY = JURISDICTION 
                ")
Data1 = Data1[,c(1:48,51)]
str(Data1)

#############As numeric##############33
lista = 4:47
for (i in lista) {
  Data1[[i]] = as.numeric(Data1[[i]])
}

###### Protocolo de revision ######

Data_ = Data1

summary(Data_[[5]])
 

######Elimino tablas########### #### Data1 49

rm(carbontax)
rm(carbontax_ct)
rm(carbontax_et)
rm(IEA)
rm(Data)





##############################
###1.4 agrego group income ###
#View(groupincome)

Data1 = sqldf("
                  SELECT * FROM Data1 B
                  INNER JOIN (SELECT * FROM groupincome) A
                  ON A.COUNTRY = B.COUNTRY
                  ")


Data1 = Data1[,c(1:49,51:52)]

unique(Data1$COUNTRY)


############################## Data1 51 variables y  201 paises en 4020 registros
Data_ = Data1
###1.5 agrego Data Climate ###
#View(Data_Climate)
#tabla de indicadores WB
Data_Climate <- gather(Data_Climate, year, value, Y2001:Y2020, factor_key=TRUE)
Data_Climate <- subset(Data_Climate, is.na(Data_Climate$SERIES_NAME) == F)
Data_Climate <- spread(Data_Climate[,c(1,2,4,5,6)], SERIES_CODE, value) ## Quitamos la 3ra columna 
str(Data_Climate)
Data_Climate <- Data_Climate[, c(1:23, 26)]
#View(Data_Climate)

#colnames(Data_Climate)

Lista1  = c(	'COUNTRY_NAME',	'CODE_COUNTRY',	'year',	'agricul_land',	'arable_land',	'CO2_bunkerfuels',	'CO2_building',	'CO2_electricity',	'CO2_energy',	'CO2_Fugitive emissions',	'CO2_industrial',	'CO2_landuse',	'CO2_manufacturing',	'CO2_otherfuel',	'CO2_transportation',	'oilcon_percap',	'solar_potential',	'total_energytax',	'GHG_growth',	'GHG_percap',	'animal_protein',	'mineral_rents',	'natgas_rents',	'gini')
   
colnames(Data_Climate) = Lista1
Data_Climate$year <- as.numeric(lapply(Data_Climate$year, gsub, pattern='Y', replacement='') )
# Data_Climate$year <- unlist(Data_Climate$year)
str(Data_Climate)
str(Data1)

Data2 = sqldf("
                  SELECT * FROM Data1 B
                  INNER JOIN (SELECT * FROM Data_Climate) A
                  ON B.COUNTRY_CODE = A.CODE_COUNTRY 
                  AND B.year = A.year 
                  ")

colnames(Data2)
Data2 = Data2[,c(1:51,55:75)]


######Elimino tablas###########  
rm(Data1)
rm(groupincome)
rm(Data_Climate)


Data_ = Data2



#######################################    Data2 72 variables y  201 paises en 4020 registros
###1.6 agrego Datos institucionales ###
###TABLA ADICIONAL###

glimpse(DPI2020)
lista = colnames(DPI2020)
nulos = -999
length(lista)

for (i in 1:length(lista)) {
  DPI2020[[i]] = ifelse(DPI2020[[i]] == nulos , NA ,DPI2020[[i]] )
  }
DPI2020 <- subset(DPI2020, DPI2020$year>2000)

glimpse(Data2)

DPI2020 = DPI2020[,c(1:5,11:12, 18:21, 25)]
colnames(DPI2020)
Lista1  = c('COUNTRY_NAME',	'CODE',	'year1',	'politicalsys',	'chiefyear_office',	'military',	'defmin',	'party_nationalist',	'party_rural',	'party_regional',	'party_religious',	'total_seats')
colnames(DPI2020) = Lista1
str(DPI2020)
unique(DPI2020$CODE)
unique(Data2$COUNTRY_CODE)
DPI2020 = subset(DPI2020, DPI2020$CODE != "0")
 

table(unique(Data2$COUNTRY_CODE) %in% unique(DPI2020$CODE)  ) # COMPROBAR CUANTOS CODIGOS NO ESTAN EN LAS TABLAS
excluyente = data.frame("Data2" = unique(Data2$COUNTRY_CODE), "COUNTRY" = unique(Data2$COUNTRY) ) # COMPROBAR CUANTOS CODIGOS NO ESTAN EN LAS TABLAS
excluyente$no_in_DPI2020 = excluyente$Data2 %in%  unique(DPI2020$CODE) # COMPROBAR CUANTOS CODIGOS NO ESTAN EN LAS TABLAS



Data_ = Data2

Data3 = sqldf("
                  SELECT * FROM Data2 B
                  INNER JOIN (SELECT * FROM DPI2020 ) A
                  ON B.COUNTRY_CODE = A.CODE
                  AND B.year = A.year1 
                  ")


colnames(Data3)
(unique(Data3$COUNTRY))

rm(a)
rm(DPI2020)
rm(excluyente)

##opcional  #######################################   
Data3 = Data3[,c(1:68, 70:72,76:84)]

rm(Data2)

Data_ = Data3


#######################################    Data3 80 variables y  169 paises en 4020 registros
###1.8 agrego Datos OECD ###
###TABLA ADICIONAL###
#### FROM LONG TO WIDE
colnames(OECD)[2] = 'OECD'
OECD = sqldf("SELECT * FROM OECD WHERE  OECD IS NOT NULL  ")
OECD$OECD =  as.numeric(OECD$OECD)
str(OECD)
 
Data4 = sqldf("SELECT * , 
                  CASE WHEN YEAR  >= OECD  then 1 else 0 end  OECD
                  FROM Data3 
               LEFT JOIN (SELECT * FROM OECD)   
                  ON UPPER(COUNTRY) = UPPER(PAIS) 
                ")
Data4 = Data4[,c(1:80, 83)]

str(Data4)

lista = c(52:71, 80 ) #As numeric####
for (i in lista) {
  Data4[[i]] = as.numeric(Data4[[i]])
}

lista = c(72:79 , 81 ) #As factor ####
for (i in lista) {
  Data4[[i]] = as.factor(Data4[[i]])
}
str(Data4)
#####elimino tablas####

rm(OECD)
rm(Data3)



Data_ = Data4
(unique(Data4$COUNTRY))

#######################################    Data4 81 variables y  169 paises en 3370 registros 

###########################################
###1.9 agrego Datos adicionales del Carbon price ETS ###
###TABLA ADICIONAL###
#### FROM LONG TO WIDE
carbonpricing <- gather(carbonpricing, year, value, Y1990:Y2021, factor_key=TRUE)
carbonpricing$year <- as.numeric(lapply(carbonpricing$year, gsub, pattern='Y', replacement=''))

carbonpricing  = carbonpricing %>% subset(carbonpricing$year >= 2001 &    carbonpricing$year <= 2020 ) ## seleccionamos registros del 2001 al 2020
colnames(carbonpricing)[3] = 'carbon_price'
str(carbonpricing)

carbonpricing = data.frame(carbonpricing)

Data5 = sqldf("
                  SELECT * FROM Data4 B
                  LEFT JOIN (SELECT * FROM carbonpricing) A
                  ON B.COUNTRY = A.Pais
                  AND B.year = A.year 
                  ")

summary(Data5[[84]]) 
 
Data5[[84]] = ifelse( is.na(Data5[[84]] ) == T  ,  0  ,Data5[[84]] )
glimpse(Data5)
Data5 = Data5[,c(1:81,84)] 
rm(Data4)
Data_ = Data5



########################################### ###  Data5 82 variables y  169 paises en 3370 registros 
###2.0 agrego Datos adicionales del share Carbon  
###TABLA ADICIONAL###

sharecarbontax <- gather(sharecarbontax, year, value, Y1990:Y2021, factor_key=TRUE)
sharecarbontax$value <- as.numeric(lapply(sharecarbontax$value, gsub, pattern='%', replacement=''))/100

sharecarbontax$year <- as.numeric(lapply(sharecarbontax$year, gsub, pattern='Y', replacement=''))

sharecarbontax  = sharecarbontax %>% subset(sharecarbontax$year >= 2001 &    sharecarbontax$year <= 2020 ) ## seleccionamos registros del 2001 al 2020
colnames(sharecarbontax)[3] = 'fraccion_share_tax'
str(sharecarbontax)
  
Data6 = sqldf("
                  SELECT * FROM Data5 B
                  LEFT JOIN (SELECT Pais, year as year_, fraccion_share_tax FROM sharecarbontax) A
                  ON B.COUNTRY = A.Pais
                  AND B.year = A.year_ 
                  ")

summary(Data6[[85]]) 

Data6[[85]] = ifelse( is.na(Data6[[85]] ) == T  ,  0  ,Data6[[85]] )
glimpse(Data6)

Data6 = Data6[,c(1:82,85)] 

rm(Data5)
Data6$rate_coverage = Data6$carbon_price * Data6$fraccion_share_tax
summary(Data6$rate_coverage)

Data_ = Data6



################################################################ Data6 84 variables y  169 paises en 3370 registros 
###2.1 Seleccion de tamano de la muestra  
###TABLA con Upper middle  income y High Income###

 
Data7 = Data6 %>%  subset (Data6$INCOME_GROUP == 'High income' |  

                             Data6$INCOME_GROUP == 'Upper middle income'   )


table(Data7$INCOME_GROUP)

unique(Data7$COUNTRY)

Data_ = Data7
write.csv(Data_, file ="tablaresultado.csv")
write.xlsx(Data_, file ="tablaresultado.xlsx")


################################################################ Data6 84 variables y  94 paises en 1880  registros 
#######1.7-  limpieza de datos tabla todos los paises ##########

apply(is.na(Data7), 2, mean)
VARna <- apply(is.na(Data7), 2, mean) 
VARna
lista = (unique(Data7$COUNTRY))
country_nulls = data.frame()
for (i in lista) {
  temproal =  data.frame('country' = i ,
                         'nulos' =  table(is.na(subset(Data7,
                                                       Data7$COUNTRY  ==  i)))[[2]] ,
                         'No nulos' =  table(is.na(subset(Data7,
                                                          Data7$COUNTRY  ==  i)))[[1]]
  )
  country_nulls = rbind(temproal, country_nulls)
} 


country_nulls$null_frac = country_nulls$nulos/(country_nulls$nulos + country_nulls$No.nulos)



##CONTEO de NA'S###
Data0 = Data7
lista = (country_nulls %>% subset(country_nulls$null_frac >= 0.15)) 
lista = list(lista$country)
lista[[1]]

for (i  in lista[[1]]) {
  Data0 = Data0 %>% subset( Data0$COUNTRY != i    )
}

unique(Data0$COUNTRY)
################# Aca se reduce la muestra de paises de 94 a 81

 

VARna <- data.frame("logico" = apply(is.na(Data0), 2, mean)>0.32) 
VARna = VARna %>% subset( logico == T)

Data0 = Data0 %>% dplyr::select(-row.names(VARna))

################# Aca se reduce la muestra de 84 variables a 79 y 1620 reg
 glimpse(Data0)
####regresion de variables####
library(forecast)

### Treatment each numerical variable 


### Prueba de funcking algoritmo
Data5 = Data0  
str(Data5)
Data5$HAS_CARB_TAX = as.factor(Data5$HAS_CARB_TAX)
Data5$HAS_ETS_TAX = as.factor(Data5$HAS_ETS_TAX)
Data6=Data5
for (j in (4:ncol(Data5)) ) {
  
  ##########  chosing the variable
  test = Data5[,c(2,3,j)]
  real_name = colnames(test)[3]
  print(real_name)
  colnames(test)[3] = 'value'
  if (is.numeric(Data5[,c(2,3,j)][[3]])== T) {
  
    ##########  chosing the country for variable j
    
    lista = (unique(test$COUNTRY_CODE))
    pais = data.frame()
    for (i in lista) {
      country_name_for_loops = i
      print(country_name_for_loops)
      temp_country = subset(test , test$COUNTRY_CODE == country_name_for_loops )
      not_ts = ifelse(
        (table(is.na(temp_country$value))['FALSE']  == nrow(temp_country) ) == T,  0 , 
        subset (data.frame(table(is.na(temp_country[[3]]) )) , data.frame(table(is.na(temp_country[[3]]) )) [[1]] == T  )[1,2]
      )
      # not_ts = subset (data.frame(table(is.na(temp_country[[3]]) )) , data.frame(table(is.na(temp_country[[3]]) )) [[1]] == T  )[1,2]
      not_ts
      colnames(temp_country)
      if (not_ts > 10 | is.na(not_ts) ) {
        temp_country[[3]] = -999
        
      } else {
        
        anios =  temp_country %>% dplyr::select(-c('COUNTRY_CODE','value'))
        row.names(temp_country) = temp_country[,2]
        temp_country = temp_country %>% dplyr::select(-c('COUNTRY_CODE','year'))
        H = ifelse( (table(is.na(temp_country$value))['FALSE']  == nrow(temp_country) ) == T,  0 , table(is.na(temp_country))[[2]] )
        #H = table(is.na(temp_country))[[2]] 
        if (H== 0) {
          temp_country = cbind(temp_country, anios)
          temp_country$COUNTRY_CODE = country_name_for_loops
          temp_country = temp_country[, c(3,2,1)]
        } else {
          a = data.frame( forecast(auto.arima(temp_country),h=H))[1]
          
          colnames(a)[1] = 'value'
          rownames(temp_country) <- 1:nrow(temp_country)
          temp_country = na.omit(temp_country)
          #### appending forecast of each country for var j
          temp_country = rbind(na.omit(temp_country), a)
          
          temp_country = cbind(temp_country, anios)
          #### renaming
          temp_country$COUNTRY_CODE = country_name_for_loops
          temp_country = temp_country[, c(3,2,1)]
          
        }
      }
      ### appending countries of variable j
    pais = rbind(temp_country, pais)
    }
    
    colnames(pais)[3] = real_name 
    
  } else {
    
    message("The variable your trying to fit is not numeric")
    
  }
  ########### Left Join
  Data6 =  dplyr::left_join(Data6, pais, by = c('COUNTRY_CODE', 'year'))
}
colnames(Data6)
summary(Data6)
Data6 = Data6[, c(1:79) ]

 
Data6$OECD = as.factor(Data6$OECD)

 
###omit nas##########

for (i in 1:length(colnames(Data6))) {
  Data6[[i]]  = ifelse(Data6[[i]] == -999, NA, Data6[[i]])
}

lista = (unique(Data6$COUNTRY))
country_nulls = data.frame()
i =lista[2]

 


table(is.na(subset(Data6, Data6$COUNTRY  ==  lista[2] ))) 
ifelse( table(is.na(subset(Data6, Data6$COUNTRY  ==  i)))['FALSE']  == 1330, 0, table(is.na(subset(Data6, Data6$COUNTRY  ==  i)))[[2]] )

for (i in lista) {
  print(i)
  temproal =  data.frame('country' = i ,
                         'nulos' =  ifelse( table(is.na(subset(Data6, Data6$COUNTRY  ==  i)))['FALSE']  == 1330, 0,  table(is.na(subset(Data6, Data6$COUNTRY  ==  i)))[[2]])  ,
                         'No nulos' =  table(is.na(subset(Data6, Data6$COUNTRY  ==  i)))['FALSE'] 
  )
  country_nulls = rbind(temproal, country_nulls)
} 

country_nulls$null_frac = country_nulls$nulos/(country_nulls$nulos + country_nulls$No.nulos)
to_get = subset(country_nulls  ,   country_nulls$null_frac<= 0.10)

paises_to_drop = to_get$country
Data7 = data.frame()
for (i  in paises_to_drop) {
  print(i)
  temp = subset(Data6 , Data6$COUNTRY == i)
  Data7 = rbind(Data7, temp)
}
###
lista = (unique(Data7$COUNTRY))
country_nulls = data.frame()
for (i in lista) {
  print(i)
  temproal =  data.frame('country' = i ,
                         'nulos' =  ifelse( table(is.na(subset(Data7, Data7$COUNTRY  ==  i)))['FALSE']  == 1330, 0,  table(is.na(subset(Data7, Data7$COUNTRY  ==  i)))[[2]])  ,
                         'No nulos' =  table(is.na(subset(Data7, Data7$COUNTRY  ==  i)))['FALSE'] 
  )
  country_nulls = rbind(temproal, country_nulls)
} 

country_nulls$null_frac = country_nulls$nulos/(country_nulls$nulos + country_nulls$No.nulos)

unique(Data7$COUNTRY)
write.csv(Data7, "compilado_paises")


###Elimino tablas###

rm(Data2)
rm(Data3)
rm(Data4)
rm(Data5)
rm(pais)
rm(temp)
rm(temp_country)
rm(test)
rm(country_nulls)
rm(temproal)
rm(to_get)
rm(VARna)


############################
###    Tabla de L.A.     ###
###########################
groupincome <- read_excel_title("groupincome.xlsx", sheet = 1)
Data8 = sqldf("
                  SELECT * FROM Data7 B
                  INNER JOIN (SELECT * FROM groupincome) A
                  ON A.COUNTRY = B.COUNTRY
                  ")
colnames(Data8)
unique(Data8$INCOME_GROUP)
Data8 = Data8[,c(1:68,70:71)]

DataLA = sqldf("
                  SELECT * FROM Data8 WHERE REGION ='Latin America & Caribbean'
                  ")

unique(DataLA$COUNTRY)
colnames(DataLA)

DataLA= DataLA[,c(7:18,20:33,35:57,60:64,66:68)]
write.xlsx(DataLA, 'DataLA.xlsx', overwrite = T)


##agrego carbon e ingreso
Dataincome = sqldf("
                  SELECT * FROM Data8 WHERE INCOME_GROUP ='High income' 
                  OR INCOME_GROUP ='Upper middle income'
                  ")
unique(Dataincome$COUNTRY)

#####lista de carbon tax#######
carbontax <- read_excel_title("carbontax.xlsx", sheet = 1)
#View(carbontax)
colnames(carbontax)
#### FROM LONG TO WIDE
carbontax <- spread( carbontax, TYPE, YEARI)

colnames(carbontax)[5] = 'CARBON_TAX'
colnames(carbontax)[6] = 'ETS_TAX'

##### INDIVIDUALIZANDO LOS TIPOS DE IMPUESTOS
carbontax_ct = sqldf("SELECT * FROM carbontax WHERE  CARBON_TAX IS NOT NULL  ")
carbontax_et = sqldf("SELECT * FROM carbontax WHERE  ETS_TAX IS NOT NULL  ")

### sELECCIONO VARIABLES DE INTERES
carbontax_ct = carbontax_ct[,c(1,5)]
carbontax_et = carbontax_et[,c(1,6)]
Data$year = unlist(Data$year)

################ Join Carbon Tax
Data1 = sqldf("SELECT * , 
                  CASE WHEN YEAR  >= CARBON_TAX  then 1 else 0 end  HAS_CARB_TAX
                  FROM Dataincome 
               LEFT JOIN (SELECT * FROM carbontax_ct)   
                  ON COUNTRY = JURISDICTION 
                ")
Data1 = Data1[,c(1:70,73)]

################### Joining the ets 
Data1 = sqldf("SELECT * , 
                  CASE WHEN YEAR  >= ETS_TAX  then 1 else 0 end  HAS_ETS_TAX
                  FROM Data1 
               LEFT JOIN (SELECT * FROM carbontax_et)   
                  ON COUNTRY = JURISDICTION 
                ")
Data1 = Data1[,c(1:71,74)]

#############As numeric##############33
lista = c(4:68, 71:72)
for (i in lista) {
  Data1[[i]] = as.numeric(Data1[[i]])
}
write.xlsx(Data1, 'Datahuincome.xlsx') 
###########################################

### Correlacion LA##########
correlacionLA  =data.frame(cor(DataLA[,c(7:55)]))
library(openxlsx)

write.xlsx(correlacionLA, 'correlacionLA.xlsx', overwrite = T) 

DataLA$exportador  = ifelse(Data8$Exports_petroleum == 0, "0", "1" )

summary(lm(data = DataLA, log(GDP_current)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  factor(OECD)   ))
summary(lm(data = DataLA, log(GDP_capita)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  factor(OECD)  ))
model1  = (lm(data = DataLA, log(GDP_capita)~log(greenhouse_emissions) + factor(HAS_CARB_TAX)  +  corruption_control  + exportador   ))


### Correlacion##########
correlacion  =data.frame(cor(Data8[,c(7:70)]))
library(openxlsx)
 
write.xlsx(correlacion, 'C:/Users/USER/Documents/Doctorado/202120/Seminario/Data/outputs/correlacion.xlsx' ) 
Data8$exportador  = ifelse(Data8$Exports_petroleum == 0, "0", "1" )

summary(lm(data = Data8, log(GDP_current)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  factor(OECD)   ))
summary(lm(data = Data8, log(GDP_capita)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  factor(OECD)  ))
model1  = (lm(data = Data8, log(GDP_capita)~log(greenhouse_emissions) + factor(HAS_CARB_TAX)  +  corruption_control  + exportador   ))


########################################################################
############## Importante            ################
######################################################################

######Analisis por nivel de ingreso#########





#No hay impuestos al carbono#

rm(Data_middle2)
rm(Data_middle)
rm(Data_low)
rm(a)
rm(correlacion)
rm(correlacion1)
rm(correlacion2)
rm(correlacion3)
rm(Data5)
rm(model1)
rm(model2)
rm(model3)
rm(anios)


######Analisis Metcalf 2020#########

paises= c('Finland',	'Poland',	'Norway',	'Sweden',	'Denmark',	'Slovenia',	'Estonia',	'Latvia',	'Switzerland',	'Ireland',	'Iceland',	'United Kingdom',	'Spain',	'France',	'Portugal')
Data_met = data.frame()
for (i in paises) {
  temporal = subset(Data1, COUNTRY == i)
  Data_met = rbind(Data_met,temporal)
}
rm(temporal)

Data_met = Data_met[,c(1:5,7,10:12,16:27,33:42)]
colnames(Data_met)

#install.packages("dplyr")
library(dplyr)

data_dplyr = Data_met %>%
  group_by(COUNTRY) %>%
  dplyr::mutate(lag1= dplyr::lead(GDP_current, n=6, default =NA)) %>%
  as.data.frame()

summary(lm(data= data_dplyr[,c(4:14,18:32)], log(GDP_current)~ .) )
options(scipen = 999)
summary(lm(data= data_dplyr[,c(4:14,18:32)], log(GDP_current)~ .) )


library(sqldf)
####Analisis paises no Metcalf

Data3 = sqldf("SELECT *  
                  FROM Data2 
               INNER JOIN (SELECT JURISDICTION FROM carbontax_ct)   
                  ON COUNTRY = JURISDICTION 
                ")

paises= c('Finland',	'Poland',	'Norway',	'Sweden',	'Denmark',	'Slovenia',	'Estonia',	'Latvia',	'Switzerland',	'Ireland',	'Iceland',	'United Kingdom',	'Spain',	'France',	'Portugal')
Data4 = data.frame()

for (i in paises) {
  temporal = subset(Data3, COUNTRY != i)
  Data4 = rbind(Data4,temporal)
}
Data4 = sqldf("SELECT distinct  * FROM Data4 ")
Data4 = Data4[,c(1:31)]
library(dplyr)

data_dplyr = Data4 %>%
  group_by(COUNTRY) %>%
  dplyr::mutate(lag1= dplyr::lead(GDP_current, n=6, default =NA)) %>%
  as.data.frame()

summary(lm(data= data_dplyr[,c(4:14,18:31)], log(GDP_current)~ .) )
summary(lm(data= data_dplyr[,c(4:13,15,18:31)],log(GDP_growth)~ .) )
summary(lm(data= data_dplyr[,c(4:13,16,18:31)],log(GDP_capita)~ .) )
options(scipen = 999)




###########Export##############
Data2 = Data1[,c(1:5,7,10:12,16:27,33:42)]
colnames(Data2)
lista = 4:31
for (i in lista) {
  Data2[[i]] = as.numeric(Data2[[i]])
}
write.csv(Data2, "~/Doctorado/202120/Seminario/Data/DATA2.csv")
summary(Data2$oil_rents)



########CORRELACION##################
Data22 =Data2[,c(3:29)]
res = cor(Data2[,c(4:9)])
round(res, 2)
str(Data2)




###################################################
#2. Selecciono muestra de paises a estudiar     ###
###################################################
Data$year = unlist(Data$year)
Paises_a = unique(Data$COUNTRY)
Paises_b = unique(groupincome$COUNTRY_NAME)

Paises_b = data.frame('paises_b' = Paises_b)
Paises_a = data.frame('Paises_a' = Paises_a)

Paises_b$paises_no_a =  Paises_b$paises_b %in% Paises_a$Paises_a
Paises_a$paises_no_b =  Paises_a$Paises_a %in% Paises_b$paises_b


Dataa = sqldf("
                  SELECT * FROM Data B
                  INNER JOIN groupincome A
                  ON B.COUNTRY = A.COUNTRY_NAME
                  ") #uno tabla Data y GROUP INCOME
glimpse(Data)
colnames(Data)
Paises_c = unique(Dataa$COUNTRY)
Data = Data[,c(1:36,38:39)] #Elimino variable country repetida

Datahuincome = sqldf("
                  SELECT * FROM Data WHERE INCOME_GROUP ='High income' 
                  OR INCOME_GROUP ='Upper middle income'
                  ")

unique(Datahuincome$COUNTRY_NAME)
colnames(Datahuincome)

write.xlsx(Datahuincome, 'Datahuincome.xlsx', overwrite = T) # exportar tabla de datos ingreso alto y medio alto

glimpse(data_)
