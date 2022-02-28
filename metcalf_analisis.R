##################################################
# 
#*     IRFS and dynamic causal effects of carbon tax on gdp growth
#*     Results for Metcalf-Stock AER P&P (2020)
#*      Builds on e1123_mid19_2.do and Table6.do
#*
getwd()
Data_met <- read_csv("tablaresultado.csv")

Data_met = Data_met[,c(2,3,4,84,23,38,49,83)]
data_NAME = Data_met[,c(1, 2, 3, 4, 7,8)]
unique(data_NAME$COUNTRY)
write.xlsx(data_NAME, file ="data_met", overwrite = T)


Data_met0<- read_csv("GDPcountries.csv") #TABLA DE MAS AÑOS GDP Y LABOR
#tabla DE gdp CON MAS AÑOS
Data_met0 <- gather(Data_met0, year, value, Y1990:Y2020, factor_key=TRUE)
Data_met0 <- spread( Data_met0[,c(1,2,3,5,6)], Series_Name,value) #### excluimos la columna tres ya que esta se repite con la 4
str(Data_met0)
Data_met0$year <- as.numeric(lapply(Data_met0$year, gsub, pattern='Y', replacement='') )

Data_met1 = sqldf("SELECT *   FROM Data_met0 A
               LEFT JOIN (SELECT * FROM data_NAME) B   
                  ON COUNTRY = Country_Name AND A.year = B.year
                ")

unique(Data_met2$Country_Name)
Data_met2 = Data_met1 %>% subset( Data_met1$Country_Name %in% data_NAME$COUNTRY) #dejar solo 94 paises en la tabla 
Data_met2 = Data_met2[,c(1:7, 11:13)]
#elimino tablas
rm(Data_met)
rm(a)
rm(Data_met0)
rm(VARna)

################ cREANDO VARIABLES DE METCALF
Data_met2$ctaxever = ifelse(Data_met2$Country_Name %in% carbontax_ct$JURISDICTION, "1", "0") 
Data_met3 = sqldf("SELECT *   FROM Data_met2 A
               LEFT JOIN (SELECT CARBON_TAX, JURISDICTION FROM carbontax_ct) B   
                  ON JURISDICTION = Country_Name
                ")

glimpse(Data_met3)
Data_met3 = Data_met3 %>% dplyr::select(-c("ctaxyear", "JURISDICTION"))
Data_met3 = Data_met3[-c(4, 5)]

write.xlsx(Data_met3, file ="data_met", overwrite = T)

########## analisis################ Data9 79 variables y  94 paises en 1620  registros 
########con paises de ingresos altos#####
Data_high = sqldf("
        SELECT * FROM Data9
        WHERE INCOME_GROUP = 'High income' 
        ")
correlacion1  =data.frame(cor(Data_high[,c(7:70)]))

write.xlsx(correlacion1, 'C:/Users/USER/Documents/Doctorado/202120/Seminario/Data/outputs/correlacion1.xlsx')

summary(lm(data = Data_high, log(GDP_growth)~log(greenhouse_emissions) * factor(HAS_CARB_TAX) *  corruption_control   ))
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
######solo con pa?ses de ingreso medio#########
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
  temporal = subset(Data9, COUNTRY == i)
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

summary(lm(data= data_dplyr[,c(4:20,25)], log(GDP_current)~ .) )
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
