
#### Análisis de las emisiones de CO2 en México  

#---Primera parte---

# CO2 emissions, kg per purchasing power parity $ GDP in Mexico 1990-2015


#Los siguientes datos fueron obtenidos de: http://agenda2030.mx/ODSind.html?ind=ODS009000300050&cveind=48&cveCob=99&lang=en#/BasicData
#Estos datos son útiles para analizar la cantidad total de dióxido de carbono emitido anualmente (derivado de los sectores de energía, por procesos industriales y uso de productos, 
#agricultura, silvicultura y otros usos de la tierra y residuos), por PIB a precios constantes base 2010 expresado en Paridad de Poder de Compra. (dólares).

# El indicador es el cociente de  las emisiones de dióxido de carbono derivadas de los sectores anteriores,entre el PIB expresado en paridad de poder adquisitivo a precios constantes en un año específico.
# El resultado está expresado en kilogramos de CO2 por dólar por paridad de poder adquisitivo.

#Lo anterior se analiza para relacionar las emisiones con la parte económica de ingresos. Donde podríamos 
#observar la correlación que hay entre las emisiones de cada sector y el indicador.

#Se contempla el periodo de :1990-2015.


#Estos se encuentran descargados previamente en nuestro directorio de trabajo
#setwd("C:/Users/Carolina/Desktop/CO2_emissions ODS Mexico/Datos")


#Cargamos las librerias necesarias 
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(plyr)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(tidyr)))

#Leemos los archivos del directorio para importarlos a R.
carbon_ind       <- read.csv("carbon_industrial.csv")
carbon_land      <- read.csv("carbon_land.csv")
carbondioxide_em <- read.csv("carbondioxide_em.csv")
waste_em         <- read.csv("waste_em.csv")
per_purchasing   <- read.csv("per_purchasing.csv")
power_parity     <- read.csv("Power_Parity.csv")
total_index      <- read.csv("tot_(Gg_of_CO2).csv")

#Hay que unir en una sola base de datos para realizar el análisis.

#Para la base de datos relacionada con las emisiones de CO2 tenemos:
datos_comp <- merge(x=carbon_ind, y=carbon_land,by="Period")
datos_comp <- merge(x=datos_comp, y=waste_em, by="Period")
datos_comp <- merge(x=datos_comp, y=carbondioxide_em, by="Period")
datos_comp <- merge(x=datos_comp, y=total_index, by="Period")
#La base de datos: datos_comp contiene información de las emisiones de dióxido de carbono de
#de los sectores de energía, por procesos industriales y uso de productos, 
#agricultura, silvicultura y otros usos de la tierra y residuos así como el total. 

#Es importante observar las características de la base de datos para conocer el tipo de datos que contiene
# y con ello poder analizarla 

str(datos_comp) #Nos damos cuenta de que las columnas son de tipo caracter y necesitamos que sean numéricas
Industrial <- gsub(",","",datos_comp$Carbon_dioxide_emissions_by_industrial_processes_and_use_of_products)
Land       <- gsub(",","",datos_comp$Carbon_dioxide_emissions_from_agriculture._forestry_and_other_land_uses)    
Waste      <- gsub(",","",datos_comp$Emissions_of_carbon_dioxide_by_waste)
Energy     <- gsub(",","",datos_comp$Emissions_of_carbon_dioxide_by_energy)
Total      <- gsub(",","",datos_comp$Total_emissions_.Gg_of_CO2.e)

#convirtiendo los valores de caracter a numéricos 
Industrial <- as.numeric(Industrial)
Land       <- as.numeric(Land)
Waste      <- as.numeric(Waste)
Energy     <- as.numeric(Energy)
Total      <- as.numeric(Total)

#Agregando las columnas para modificar el dataframe
datos_c <-  cbind(datos_comp, Industrial,Land,Waste,Energy,Total)
names(datos_c)
datos_c <- datos_c[,c(1,7:11)]
head(datos_c)
str(datos_c)

#Por otro lado tenemos la parte económica con la cual se calcula el índice que se propone para el análisis con base 
#en el PIB conciderando precios constantes base 2010 expresado en Paridad de Poder de Compra. (dólares).

#En la tabla power_parity se encuentran los datos del PIB per capita a precios constantes de 2010
#En la tabla per_purchasing se encuentra el indicador, el cual se obtiene dividiendo las emisiones de dióxido de carbono derivadas de los sectores anteriores entre el PIB expresado 
#en paridad de poder adquisitivo a precios constantes en un año específico. El resultado se expresa en kilogramos de CO2 por dólar por paridad de poder adquisitivo.

econo_comp <- merge(x=total_index, y=power_parity,by="Period")
econo_comp <- merge(x=econo_comp, y=per_purchasing,by="Period")

PIB_per_capita <- gsub(",","",econo_comp$GDP_at_constant_prices_base_2010_expressed_in_Purchasing_Power_Parity_.dollars.)
Indicator      <- gsub(",","",econo_comp$CO2_emissions._kg_per_purchasing_power_parity_._GDP)

PIB_per_capita <- as.numeric(PIB_per_capita)
Indicator      <- as.numeric(Indicator)

econo_c  <- cbind(econo_comp,Total,PIB_per_capita,Indicator)
names(econo_c)
econo_c <- econo_c[,c(1,5:7)]
head(econo_c);str(econo_c)



#--- Análisis gráfico -----

## SERIES DE TIEMPO DE EMISIONES POR SECTOR 
ts.ind   <- ts(datos_c$Industrial, st=1990, fr=1)
ts.land  <- ts(datos_c$Land,st=1990,fr=1)
ts.waste <- ts(datos_c$Waste,st=1990,fr=1)
ts.ene   <- ts(datos_c$Energy,st=1990,fr=1)
ts.tot   <- ts(datos_c$Total,st=1990,fr=1)
#Para verlas en un gráfico único 
ts_juntos <- ts.plot(ts.ind,ts.land,ts.waste,ts.ene, 
                     gpars=list(xlab="Año", ylab="emisiones", lty=c(1:3)),
                     main = expression(paste("CO"[2], " kg")))

## intersection (only overlapping times) of the time series
dat_int <- ts.intersect(ts.ind, ts.land,ts.waste,ts.ene)
## dimensions of common-time data
dim(dat_int)

## plot the ts
plot(dat_int, main = "Series de tiempo emisiones por sector (de energía,
     desechos, uso de suelo e industrial)",cex.main= 1,adj=0.5, yax.flip = TRUE, 
     col= "blue", type = "o", pch = 12, xlab="Año")


ts.pib        <- ts(econo_c$PIB_per_capita, st=1990, fr=1)
ts.indicador  <- ts(econo_c$Indicator, st=1990, fr=1) 


ts.plot(ts.tot)
ts.plot(ts.pib)
ts.plot(ts.indicador)

## intersection (only overlapping times)
eco_int <- ts.intersect(ts.tot, ts.pib, ts.indicador)


plot(eco_int, main = "Series de tiempo del total de emisiones, PIB per cápita y el indicador",
     cex.main= 1,adj=0.5, yax.flip = TRUE, 
     col= "red", type = "o", pch = 12, xlab="Año")

#--- Gráficos----

#Realizamos la gráfica por sector para la contribución a las emisiones

colors <- c("Total" = "gold","Industrial"="blue", "Energia" ="red", "Desechos" = "purple", "Uso de suelo"="forestgreen") 
            
sector <- ggplot(datos_c, aes(x=Period)) + 
  geom_line(aes(y=Industrial, color = "Industrial"),size=1) + 
  geom_line(aes(y = Land, color = "Uso de suelo"),size=1)+
  geom_line(aes(y = Waste, color = "Desechos"),size=1)+
  geom_line(aes(y = Energy, color = "Energia"),size=1)+
  geom_line(aes(y = Total, color = "Total"),size=1)+
  labs( 
    y = expression(paste("Emisiones de ","CO"[2],"(kg)" )),
    title = expression(paste("Emisiones anuales de ","CO"[2]," por sector" )),
    color = "Sectores")+
  scale_x_continuous(breaks = pretty(datos_c$Year,n=20),name="Año")+
  theme(plot.title = element_text(size=12))  +
  scale_color_manual(values=colors)+ 
  theme_bw()

sector

#Para hacer más clara la gráfica de la serie de tiempo del total de emisiones de co2 
p.tot <- ggplot(datos_c, aes(x=Period, y=Total)) + 
  geom_point(aes(x=Period), size=2)+
  geom_line(color="blue",size=1) + 
  labs(y = "Acumulado de emisiones (en kg) por año",
       title = expression(paste("Total de emisiones anuales ","CO"[2], "del acumulado por sector" )) )+
  scale_x_continuous(breaks = pretty(datos_c$Period,n=10),name="Año")+
  theme(plot.title = element_text(size=15))  +
  theme_light()

p.tot

# Gráfica de PIB per cápita (dolares)

g.pib <- ggplot(econo_c, aes(x=Period, y=PIB_per_capita)) + 
  geom_point(aes(x=Period), size=2)+
  geom_line(color="forestgreen",size=1) + 
  labs(y = "PIB per cápita en dolares",
       title = expression(paste("PIB per cápita (dolares) anual a cambio de moneda fijo en 2011" )) )+
  scale_x_continuous(breaks = pretty(datos_c$Period,n=10),name="Año")+
  theme(plot.title = element_text(size=15))  +
  theme_light()

g.pib

# Gráfica del indicador 

g.ind <- ggplot(econo_c, aes(x=Period, y=Indicator)) + 
  geom_point(aes(x=Period), size=2)+
  geom_line(color="orange",size=1) + 
  labs(y = expression(paste("Total de emisiones anuales de  ","CO"[2], " / PIB") ),
       title = expression(paste("Emisiones anuales de  ","CO"[2], " respecto al PIB per cápita") ) )+
  scale_x_continuous(breaks = pretty(datos_c$Period,n=10),name="Año")+
  theme(plot.title = element_text(size=15))  +
  theme_light()

g.ind

##Nos podemos plantear la siguiente pregunta, ¿cuál es el sector que hace mejor su trabajo? Esto en el sentido
#de cuál de ellos ha crecido más económicamente de forma sustentable, esto es, cuál de ellos genera mayores ganancias
#con el menor número de emisiones.


#Para realizar el análisis por sector, utilizaremos la tabla en la cual se encuentra el dato 
#PIB per capita, esto con el objetivo de observar las contribuciones de cada sector

#Veamos cuánto aporta cada sector al valor del indicador (porcentaje) durante 1990-2015
Periodo   <- datos_c$Period
Ind_PIB   <- datos_c$Industrial / econo_c$PIB_per_capita
Waste_PIB <- datos_c$Waste / econo_c$PIB_per_capita
Land_PIB  <- datos_c$Land / econo_c$PIB_per_capita
Ener_PIB  <- datos_c$Energy /  econo_c$PIB_per_capita
Indicador_PIB <- econo_c$Indicator

#Cuánto aporta en promedio de emisiones cada sector a las emisiones totales 
# en el periodo 1990-2015
Ind_per <- datos_PIB[,1]
Year <- datos_c$Year
Prom_Ind   <- mean(datos_c$Industrial)
Prom_Land  <- mean(datos_c$Land)
Prom_Waste <- mean(datos_c$Waste)
Prom_Ener  <- mean(datos_c$Energy)

## Gráfica del promedio de emisiones por sector 
Sectores <- c("Industry", "Land", "Waste", "Energy")
Promedio_emisiones <- c(Prom_Ind,Prom_Land,Prom_Waste,Prom_Ener)

barplot(height=Promedio_emisiones, names=Sectores,
        col=c('red','blue','forestgreen','orange'),cex.names = 0.9,hor=1,las=1,
        main=expression(paste("Aportación a emisiones de  ","CO"[2], "por sector durante 1990-2015")), 
        ylab="Sector",xlab= expression(paste("Emisiones de  ","CO"[2], " (kg)")))

####
datos_PIB <- cbind.data.frame(Periodo,Ind_PIB,Land_PIB, Waste_PIB, Ener_PIB, Indicador_PIB )
head(datos_PIB) 

(datos_PIB[,2]/datos_PIB[,6])*100
(datos_PIB[,3]/datos_PIB[,6])*100
(datos_PIB[,4]/datos_PIB[,6])*100
(datos_PIB[,5]/datos_PIB[,6])*100

#Se necesita crear un df para ir almacenando los porcentajes de aportación
#respecto al total
v <- as.data.frame(c(1:length(datos_PIB$Periodo)))
for (i in 2:5){
  v[,paste("contribución", i)] <- (datos_PIB[,i]/datos_PIB[,6])*100
}
v_tra <- as.data.frame(t(v))
v_tra[,"Contribuciones"] <- c("total",colnames(v[-1]))
par(mar=c(11,4,4,4))

#Gráfica de porcentaje de aportación al indicador por sector
sector <- c("Industry", "Land", "Waste", "Energy")

barplot(height = v_tra$V1[-1] , names = sector, col=c('red','blue','forestgreen','orange'), horiz = F, las=2,
        ylab="Porcentaje",font.lab = 1, col.lab = "black", cex.lab = 1,
        xlab= "Sector",main = "Porcentaje de aportación por sector al indicador durante 1990-2015 ")



#################################
################################

#Los siguientes datos fueron obtenidos de: https://ourworldindata.org/co2/country/mexico?country=~MEX#per-capita-how-much-co2-does-the-average-person-emit
#Hannah Ritchie and Max Roser (2017) - "CO2 and Greenhouse Gas Emissions". Published online at OurWorldInData.org.
#Retrieved from: 'https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions' [Online Resource]

#Cargando nuevos datos en donde se utilizan otros indicadores para realizar el análisis:
global_em_percap       <- read.csv("global_co2_emissions_per_capita.csv")
global_co2_source      <- read.csv("global_co2_source.csv")
co2_emissions_and_gdp  <- read.csv("co2-emissions-and-gdp.csv")
#Nos interesan los datos de México
mex_em_percap   <- filter(global_em_percap,Code == "MEX")
str(mex_em_percap)
names(mex_em_percap)

#Observamos los valores útiles para México
mex_source_co2   <- filter(global_co2_source,Code == "MEX", Year >= 1900)
str(mex_source_co2)
names(mex_source_co2)


mex_graf_percap <- ggplot(mex_em_percap, aes(x=Year, y=Per.capita.CO2.emissions)) + 
  geom_point(aes(x=Year))+
  geom_line( color="blue") + 
  labs( y = "emisiones (ton/año)",
       title = expression(paste("Total de emisiones anuales en México de ","CO"[2] , " per cápita")) )+
  scale_x_continuous(breaks = pretty(mex_em_percap$Year,n=10),name="Año")+
  theme(plot.title = element_text(size=12))  +
  theme_bw()

mex_graf_percap



## Emisiones de CO2 por tipo de combustible 

names(mex_source_co2)
x          <- mex_source_co2$Year
Petroleo   <- mex_source_co2$CO2.emissions.from.oil
Carbon     <- mex_source_co2$CO2.emissions.from.coal
Gas        <- mex_source_co2$CO2.emissions.from.gas
Quemadores <- mex_source_co2$CO2.emissions.from.flaring
Cemento    <- mex_source_co2$CO2.emissions.from.cement
otros      <- mex_source_co2$CO2.emissions.from.other.industry  

colors <- c("Petroleo"="blue", "Carbon" ="red", "Gas" = "purple", "Quemadores"="forestgreen", "otros"= "yellow", 
"Cemento" = "black")

mex_co2_source <- ggplot(mex_source_co2, aes(x=Year)) + 
  geom_line(aes(y = Petroleo, color="Petroleo"), size=1) + 
  geom_line(aes(y = Carbon, color = "Carbon"), size= 1)+
  geom_line(aes(y = Gas, color = "Gas"),size =1)+
  geom_line(aes(y = Quemadores, color = "Quemadores"), size=1)+
  geom_line(aes(y = Cemento, color = "Cemento"), size=1)+
  geom_line(aes(y = otros, color = "otros"),size=1)+
  labs( y = "Emisiones (mill ton)",
    title = expression(paste("Emisiones de ","CO"[2], " por tipo de combustible" )), 
    color = "Combustible")+
  scale_x_continuous(breaks = pretty(mex_source_co2$Year,n=10),name="Año")+
  theme(plot.title = element_text(size=12))  +
  scale_color_manual(values=colors)+
  theme_bw()

mex_co2_source

#########
#comparaciones de emisiones de CO2 respecto al PIB
#En esta sección volvemos a analizar el cambio en porcentajes de emisión respecto 
#a la generación económica de ese periodo

#Observamos los valores útiles para México
mex_co2_emissions_and_gdp   <- filter(co2_emissions_and_gdp,Code == "MEX", Year >= 1990,Year <=2017)
str(mex_co2_emissions_and_gdp)
names(mex_co2_emissions_and_gdp )
head(mex_co2_emissions_and_gdp)

Year <- mex_co2_emissions_and_gdp$Year
GDP_per_capita <- mex_co2_emissions_and_gdp$GDP.per.capita..PPP..constant.2011.international...
Per_capita_CO2 <- mex_co2_emissions_and_gdp$Per.capita.CO2.emissions
Per_capita_consumption <- mex_co2_emissions_and_gdp$Per.capita.consumption.based.CO2.emissions
mex_co2_emissions_and_gdp <- cbind(Year,GDP_per_capita,Per_capita_CO2,Per_capita_consumption)
                                   
head(mex_co2_emissions_and_gdp)
mex_co2_emissions_and_gdp <- as.data.frame(mex_co2_emissions_and_gdp)
mex_co2_emissions_and_gdp <- mutate(mex_co2_emissions_and_gdp, nuevo1 = mex_co2_emissions_and_gdp$GDP_per_capita[1])
mex_co2_emissions_and_gdp <- mutate(mex_co2_emissions_and_gdp, nuevo2 = mex_co2_emissions_and_gdp$Per_capita_CO2[1])
mex_co2_emissions_and_gdp <- mutate(mex_co2_emissions_and_gdp, nuevo3 = mex_co2_emissions_and_gdp$Per_capita_consumption[1])

mex_co2_emissions_and_gdp <- mutate(mex_co2_emissions_and_gdp, pGDP_per_capita = (GDP_per_capita - nuevo1)*100/nuevo1, 
                                   pPer_capita_CO2 = (Per_capita_CO2 - nuevo2)*100/nuevo2, 
                                   pPer_capita_consumption = (Per_capita_consumption  - nuevo3)*100/nuevo3 )
p_mex_co2_emissions_and_gdp <- mex_co2_emissions_and_gdp[,c(1,8:10)]

################## Gráfico de porcentajes 
colors <- c("GDP_per_capita"="blue", "Per_capita_pro" ="red", "Per_capita_con" = "forestgreen")

porc_co2_gdp <- ggplot(p_mex_co2_emissions_and_gdp, aes(x=Year)) + 
  geom_line(aes(y = pGDP_per_capita, color="GDP_per_capita"), size=1) + 
  geom_line(aes(y = pPer_capita_CO2, color = "Per_capita_pro"), size= 1)+
  geom_line(aes(y = pPer_capita_consumption, color = "Per_capita_con"),size =1)+
  labs( y = "Porcentaje",
        title = expression(paste("Porcentajes de emisiones de ","CO"[2], " relacionado al PIB" )), 
        color = "Valores")+
  scale_x_continuous(breaks = pretty(p_mex_co2_emissions_and_gdp$Year,n=10),name="Año")+
  theme(plot.title = element_text(size=12))  +
  scale_color_manual(values=colors)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

porc_co2_gdp

