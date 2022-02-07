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

#######################
#### Carga del archivo
#######################
# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/
# Direcccion del archivo

library(readr)
library(readxl)
source("~/Doctorado/202120/Seminario/R/read_excel_title.R")

Data  = read_excel_title ("~/Doctorado/202120/Seminario/Data/Data.xlsx", sheet =1 )

#tabla de indicadores WB
Data <- gather(Data, year, value, Y2001:Y2020, factor_key=TRUE)
Data <- spread( Data[,c(1,2,4,5,6)], SERIES_CODE,value)
str(Data)
#View(Data)

library(readr)
IEA <- read_csv_title("~/Doctorado/202120/Seminario/Data/IEA.csv")
#View(IEA)
#tabla de indicadores IEA
IEA <- gather(IEA, year, value, Y2001:Y2021, factor_key=TRUE)
IEA <- spread( IEA, SERIES_NAME1,value)
str(IEA)
#View(IEA_wide)

colnames(IEA)[2] = "YEAR1"

library(sqldf)

##Unir tabla datawide y IEA_wide##
Data = sqldf("
                  SELECT * FROM Data 
                  INNER JOIN (SELECT * FROM IEA)  
                  ON COUNTRY = COUNTRY_NAME AND YEAR1 = year
                  ")


View(Data)
colnames(Data)

#########################3
Lista1  = c(	'COUNTRY',	'COUNTRY_CODE',	'year',	'corruption_control',	'access_cooking',	'electricity_dirty',	'renewable_consumption',	'fuel_energy_consumption',	'energy_use',	'CO2emissions',	'CO2_fuel_consumption',	'greenhouse_emissions',	'CO2_manufacturing',	'CO2_others',	'CO2_transport',	'r&d_expenditure',	'government_Effectiveness',	'carbon_damage',	'coal_rents',	'GDP_current',	'GDP_growth',	'GDP_capita',	'GDP_capitagrowth',	'oil_rents',	'natresources_rents',	'rule_law',	'regulatory_quality',	'employment_agriculture',	'employment_industry',	'employment_services',	'unemployment_advancededu',	'unemployment_basicedu',	'population_growth',	'population',	'fuel_import',	'fuel_export',	'del1',	'del2',	'consumption_petroleum',	'Exports_petroleum',	'Imports_petroleum',	'Production_petroleum')
colnames(Data) = Lista1
Data = Data[, c(1:36, 39:42)]
Data$year <- lapply(Data$year, gsub, pattern='Y', replacement='')

#####lista de carbon tax#######
carbontax <- read_excel_title("~/Doctorado/202120/Seminario/Data/carbontax.xlsx", sheet = 1)
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
                  FROM Data 
               LEFT JOIN (SELECT * FROM carbontax_ct)   
                  ON COUNTRY = JURISDICTION 
                ")
Data1 = Data1[,c(1:40,43)]
 
################### Joining the ets 
Data1 = sqldf("SELECT * , 
                  CASE WHEN YEAR  >= ETS_TAX  then 1 else 0 end  HAS_ETS_TAX
                  FROM Data1 
               LEFT JOIN (SELECT * FROM carbontax_et)   
                  ON COUNTRY = JURISDICTION 
                ")
Data1 = Data1[,c(1:41,44)]

#############As numeric##############33
lista = 4:40
for (i in lista) {
  Data1[[i]] = as.numeric(Data1[[i]])
}


######Elimino tablas###########
rm(carbontax)
rm(carbontax_ct)
rm(carbontax_et)
rm(IEA)
#rm(Data)


###adicional variables###
library(readxl)
groupincome <- read_excel_title("~/Doctorado/202120/Seminario/Data/groupincome.xlsx", sheet = 1)
#View(groupincome)

Data1 = sqldf("
                  SELECT * FROM Data1 B
                  INNER JOIN (SELECT * FROM groupincome) A
                  ON A.COUNTRY = B.COUNTRY
                  ")


Data1 = Data1[,c(1:42,44:45)]

###TABLA ADICIONAL###
library(readr)
Data_Climate <-  read_csv_title("Doctorado/202120/Seminario/Data/Data_Climate.csv")
#View(Data_Climate)
#tabla de indicadores WB
Data_Climate <- gather(Data_Climate, year, value, Y2001:Y2020, factor_key=TRUE)
Data_Climate <- subset(Data_Climate, is.na(Data_Climate$SERIES_NAME) == F)
Data_Climate <- spread(Data_Climate[,c(1,2,4,5,6)], SERIES_CODE, value)
str(Data_Climate)
Data_Climate <- Data_Climate[, c(1:23, 26)]
View(Data_Climate)

#colnames(Data_Climate)

Lista1  = c(	'COUNTRY_NAME',	'CODE_COUNTRY',	'year',	'agricul_land',	'arable_land',	'CO2_bunkerfuels',	'CO2_building',	'CO2_electricity',	'CO2_energy',	'CO2_Fugitive emissions',	'CO2_industrial',	'CO2_landuse',	'CO2_manufacturing',	'CO2_otherfuel',	'CO2_transportation',	'oilcon_percap',	'solar_potential',	'total_energytax',	'GHG_growth',	'GHG_percap',	'animal_protein',	'mineral_rents',	'natgas_rents',	'gini')
     
colnames(Data_Climate) = Lista1
Data_Climate$year <- lapply(Data_Climate$year, gsub, pattern='Y', replacement='')
Data_Climate$year <- unlist(Data_Climate$year)
str(Data_Climate)
str(Data1)
Data2 = sqldf("
                  SELECT * FROM Data1 B
                  LEFT JOIN (SELECT * FROM Data_Climate) A
                  ON B.COUNTRY_CODE = A.CODE_COUNTRY 
                  AND B.year = A.year 
                  ")

colnames(Data2)
Data2 = Data2[,c(1:44,48:68)]

######Elimino tablas###########
rm(groupincome)
rm(Data_Climate)


###TABLA ADICIONAL###
library(haven)
DPI2020 <- read_dta("Doctorado/202120/Seminario/Data/DPI2020.dta")
glimpse(DPI2020)
lista = colnames(DPI2020)
nulos = -999
length(lista)
DPI2020[[34]] = ifelse(DPI2020[[34]] == nulos , NA ,DPI2020[[34]] )

for (i in 1:length(lista)) {
  DPI2020[[i]] = ifelse(DPI2020[[i]] == nulos , NA ,DPI2020[[i]] )
  }
DPI2020 <- subset(DPI2020, DPI2020$year>2000)
VARna <- apply(is.na(DPI2020), 2, mean) 
VARna
a = data.frame( 'logico' =  (apply(is.na(DPI2020), 2, mean)>0.10))
lista  = row.names(subset(a, a$logico ==T))
 
DPI2020  = DPI2020 %>%  dplyr::select( - lista  )
 glimpse(DPI2020)

DPI2020 = DPI2020[,c(1:5,8:9, 11:16, 18, 25, 50, 56:57, 59, 61, 63)]

colnames(DPI2020)

Lista1  = c('COUNTRY_NAME',	'COUNTRY_CODE',	'year1',	'politicalsys',	'chiefyear_office',	'military',	'defmin',	'party_orientation',	'party_nationalist',	'party_rural',	'party_religional',	'party_religious',	'total_seats',	'seat_1gov',	'seat_2gov',	'gov_seats',	'fractionalization',	'govfrac',	'system_tenure',	'checks',	'stability',	'mineral_rents')


colnames(DPI2020) = Lista1
str(DPI2020)

Data3 = sqldf("
                  SELECT * FROM Data2 B
                  LEFT JOIN (SELECT * FROM DPI2020) A
                  ON B.COUNTRY_CODE = A.COUNTRY_CODE
                  AND B.year = A.year1 
                  ")

colnames(Data3)
rm(a)
rm(DPI2020)

#######limpieza de datos##########
apply(is.na(Data3), 2, mean)
VARna <- apply(is.na(Data3), 2, mean) 
VARna
lista = (unique(Data3$COUNTRY))
country_nulls = data.frame()
for (i in lista) {
  temproal =  data.frame('country' = i ,
             'nulos' =  table(is.na(subset(Data3,
                                           Data3$COUNTRY  ==  i)))[[2]] ,
             'No nulos' =  table(is.na(subset(Data3,
                                           Data3$COUNTRY  ==  i)))[[1]]
             )
  country_nulls = rbind(temproal, country_nulls)
} 

country_nulls$null_frac = country_nulls$nulos/(country_nulls$nulos + country_nulls$No.nulos)
rm(temproal)

##elimino ano 2020###

Data4 = subset(Data3, year != '2020')
lista = (unique(Data4$COUNTRY))
country_nulls = data.frame()
for (i in lista) {
  temproal =  data.frame('country' = i ,
                         'nulos' =  table(is.na(subset(Data4,
                                                       Data4$COUNTRY  ==  i)))[[2]] ,
                         'No nulos' =  table(is.na(subset(Data4,
                                                          Data4$COUNTRY  ==  i)))[[1]]
  )
  country_nulls = rbind(temproal, country_nulls)
} 

country_nulls$null_frac = country_nulls$nulos/(country_nulls$nulos + country_nulls$No.nulos)
rm(temproal)
####tabla oecd####
OECD <- read_excel_title("~/Doctorado/202120/Seminario/Data/OECD.xlsx", sheet = 1)
colnames(OECD)
#### FROM LONG TO WIDE
colnames(OECD)[2] = 'OECD'
OECD = sqldf("SELECT * FROM OECD WHERE  OECD IS NOT NULL  ")
str(Data4)
Data4 = sqldf("SELECT * , 
                  CASE WHEN YEAR  >= OECD  then 1 else 0 end  OECD
                  FROM Data4 
               LEFT JOIN (SELECT * FROM OECD)   
                  ON UPPER(COUNTRY) = UPPER(PAIS) 
                ")
Data4 = Data4[,c(1:65, 69:87)]
str(Data4)
#############As numeric##############33
lista = 45:65
for (i in lista) {
  Data4[[i]] = as.numeric(Data4[[i]])
}

lista = 41:42
for (i in lista) {
  Data4[[i]] = as.character(Data4[[i]])
}

#####elimino tablas####

rm(OECD)
rm(Data1)
rm(Data2)
rm(Data)
rm(Data3)

##CONTEO de NA'S###
Data5 = Data4

VARna <- data.frame("logico" = apply(is.na(Data5), 2, mean)>0.4) 
VARna = VARna %>% subset( logico == T)
Data5 = Data5 %>% dplyr::select(-row.names(VARna))


lista = (unique(Data5$COUNTRY))
country_nulls = data.frame()
for (i in lista) {
  temproal =  data.frame('country' = i ,
                         'nulos' =  table(is.na(subset(Data5,
                                                       Data5$COUNTRY  ==  i)))[[2]] ,
                         'No nulos' =  table(is.na(subset(Data5,
                                                          Data5$COUNTRY  ==  i)))[[1]]
  )
  country_nulls = rbind(temproal, country_nulls)
} 

country_nulls$null_frac = country_nulls$nulos/(country_nulls$nulos + country_nulls$No.nulos)
rm(temproal)

VARna <- data.frame("logico" = apply(is.na(Data5), 2, mean)>0.24) 
VARna = VARna %>% subset( logico == T)
Data5 = Data5 %>% dplyr::select(-row.names(VARna))

str(Data5)

####regresion de variables####
library(forecast)

### Treatment each numerica variable 
is.numeric(Data5[,c(2,3,j)][[3]])
 
Data6 = Data5[,c(1,2,3,34,35,70)]

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

Data6 = Data6[, c(1:34, 38:71) ]

 
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
Data8 = na.omit(Data7)

### Correlacion##########
correlacion  =data.frame(cor(Data8[,c(7:70)]))
library(openxlsx)
 
write.xlsx(correlacion, 'C:/Users/USER/Documents/Doctorado/202120/Seminario/Data/outputs/correlacion.xlsx' ) 
Data8$exportador  = ifelse(Data8$Exports_petroleum == 0, "0", "1" )

summary(lm(data = Data8, log(GDP_current)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  factor(OECD)   ))
summary(lm(data = Data8, log(GDP_capita)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  factor(OECD)  ))
model1  = (lm(data = Data8, log(GDP_capita)~log(greenhouse_emissions) + factor(HAS_CARB_TAX)  +  corruption_control  + exportador   ))

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


########################################################################
############## Importante            ################
######################################################################

######Analisis por nivel de ingreso#########

######con paises de ingresos altos#####
Data_high = sqldf("
        SELECT * FROM Data8
        WHERE INCOME_GROUP = 'High income' 
        ")
correlacion1  =data.frame(cor(Data_high[,c(7:70)]))

write.xlsx(correlacion1, 'C:/Users/USER/Documents/Doctorado/202120/Seminario/Data/outputs/correlacion1.xlsx')
summary(lm(data = Data_high, log(GDP_current)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))
summary(lm(data = Data_high, log(GDP_capita)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))

model2  = (lm(data = Data_high, log(GDP_capita)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))



Data_low = sqldf("
        SELECT * FROM Data8
        WHERE INCOME_GROUP != 'High income' 
        ")

correlacion2  =data.frame(cor(Data_low[,c(7:70)]))

write.xlsx(correlacion2, 'C:/Users/USER/Documents/Doctorado/202120/Seminario/Data/outputs/correlacion2.xlsx')
summary(lm(data = Data_low, log(GDP_current)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))
summary(lm(data = Data_low, log(GDP_capita)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))

model3 = (lm(data = Data_low, log(GDP_capita)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))
install.packages('memisc')
library(memisc)
mtable("modelo 1 " = model1, "modelo 2" = model2, "modelo 3" = model3)
tabla  = (mtable("modelo 1 " = model1, "modelo 2" = model2, "modelo 3" = model3))
ltx.m145 <- toLatex(tabla,ddigits=5)
writeLines(ltx.m145 )
######solo con países de ingreso medio#########
Data_middle = sqldf("
        SELECT * FROM Data8
        WHERE INCOME_GROUP = 'Upper middle income' 
        ")
correlacion3  =data.frame(cor(Data_middle[,c(7:70)]))

write.xlsx(correlacion3, 'C:/Users/USER/Documents/Doctorado/202120/Seminario/Data/outputs/correlacion3.xlsx')
summary(lm(data = Data_middle, log(GDP_current)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))
summary(lm(data = Data_middle, log(GDP_capita)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))


######Analisis pais dependiente no dependiente#########
Petrostate = sqldf("
        SELECT * FROM Data8
        WHERE oil_rents > 7 
        ")
correlacion3  =data.frame(cor(Data_middle[,c(7:70)]))
write.xlsx(correlacion3, 'C:/Users/USER/Documents/Doctorado/202120/Seminario/Data/outputs/correlacion3.xlsx')
summary(lm(data = Petrostate, log(GDP_current)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))
summary(lm(data = Petrostate, log(GDP_capita)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))

dependiente = sqldf("
        SELECT * FROM Data8
        WHERE oil_rents > 3 
        ")
summary(lm(data = dependiente, log(GDP_current)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))
summary(lm(data = dependiente, log(GDP_capita)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))
###############
high_consump = sqldf("
        SELECT * FROM Data8
        WHERE oil_rents > 3 
        ")
summary(lm(data = dependiente, log(GDP_current)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))
summary(lm(data = dependiente, log(GDP_capita)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))
###############


save.image("~/Doctorado/202120/Seminario/R/Analisis.R.RData")



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

r