

# Registro Administrativo de la Industria Automotriz de Vehículos Pesados 
#Venta por fuente de energia 2018 Ene - 2020 Dic. 

#Los datos mostrados a continuación fueron obtenidos de:
# INEGI. Datos primarios, Registro administrativo de la industria automotriz de vehículos pesados
# https://www.inegi.org.mx/datosprimarios/iavp/#Datos_abiertos
library('plyr')
library("dplyr")
library('tidyr')
library('ggplot2')
library('forecast')

getwd()
setwd('/home/carlos/Documents/BeduCursoR/proyecto_vehiculo/conjunto_de_datos')

#Leemos los datos
pesados_2018 <- read.csv("raiavp_vta_energ_mensual_tr_cifra_2018.csv")
pesados_2019 <- read.csv("raiavp_vta_energ_mensual_tr_cifra_2019.csv")
pesados_2020 <- read.csv("raiavp_vta_energ_mensual_tr_cifra_2020.csv")

#Conocemos la estructura de nuestra base de datos y vemos los nombres de las columnas
str(pesados_2018)
names(pesados_2018)
names(pesados_2019)
names(pesados_2020)

#Eliminamos las columnas que no nos serán utiles
head(pesados_2018)
pesados_2018$PROD_EST <- NULL
pesados_2018$COBERTURA <- NULL
pesados_2019$PROD_EST <- NULL
pesados_2019$COBERTURA <- NULL
pesados_2020$PROD_EST <- NULL
pesados_2020$COBERTURA <- NULL

#Ahora uniremos los tres en un mismo dataframe
pesados <- rbind(pesados_2018, pesados_2019, pesados_2020)
pesados

#Es necesario cargar los archivos adicional correspondiente a ID_MES
getwd()
setwd("../catalogos")
entidad <- read.csv("tc_mes.csv")

#Le hacemos un merge con nuestro DF principal
datos_pesados <- merge(x = entidad, y=pesados, by ="ID_MES")
datos_pesados

#Ordenamos las columnas
datos_pesados <- datos_pesados[, c(1, 3, 2, 4, 5, 6 , 7, 8)]
datos_pesados <- arrange(datos_pesados, ANIO, ID_MES)

#Arreglando la fecha y el formato
?unite
fecha_pesados <- datos_pesados %>% unite("FECHA",ANIO:ID_MES,sep="-",remove = F,na.rm=T)
d <- rep(1,dim(fecha_pesados)[1])
fecha_pesados <- cbind(fecha_pesados,d)
colnames(fecha_pesados)
fecha_pesados <- fecha_pesados[,c(1,10,2,3,4,5,6,7,8,9)]
fecha_pesados <- fecha_pesados %>% unite("FECHA_COMP",FECHA:d,sep="-",remove = F,na.rm=T) #creando la fecha completa 
colnames(fecha_pesados)
fecha_pesados <- fecha_pesados[,c(1,4,5,6,7,8,9,10,11)]
fecha_pesados
fecha_pesados <- mutate(fecha_pesados,FECHA_COMP = as.Date(FECHA_COMP,"%Y-%m-%d"))
fecha_pesados <- mutate(fecha_pesados, fecha= format.Date(FECHA_COMP,"%Y-%m"))
colnames(fecha_pesados)
datos_compactos_pesados<- fecha_pesados[,c(10,5,6,7,8,9)]
datos_compactos_pesados

#########################################################################
#ANALISIS EXPLORATORIO DE DATOS(EDA)
#Venta nacional de vehículos pesados( menudeo)
?ddply
ventas_pesados_menudeo <- ddply(datos_compactos_pesados, .(fecha) ,summarise,
                                ventas_totales= sum(MENUDEO ))
ts.ventas.menudeo <- ts(ventas_pesados_menudeo$ventas_totales,st=2018,fr=12)
ts.plot(ts.ventas.menudeo)

#Venta nacional de vehículos pesados(mayoreo )
ventas_pesados_mayoreo <- ddply(datos_compactos_pesados, .(fecha) ,summarise,
                                ventas_totales= sum(MAYOREO ))
ts.ventas.mayoreo <- ts(ventas_pesados_mayoreo$ventas_totales,st=2018,fr=12)
ts.plot(ts.ventas.mayoreo)

#Venta nacional de vehículos pesados(mayoreo y menudeo)
ventas_pesados <- ddply(datos_compactos_pesados, .(fecha) ,summarise, ventas_totales= sum(MENUDEO + MAYOREO))
ts.ventas <- ts(ventas_pesados$ventas_totales,st=2016,fr=12)
ts.plot(ts.ventas)


#Venta total nacional por marca (menudeo)
venta_menudeo_marca <- ddply(datos_compactos_pesados, .(MARCA) ,summarise,
                 venta_total_menudeo= sum(MENUDEO))
venta_menudeo_marca

grafico_menudeo_marca <- ggplot(venta_menudeo_marca, aes(x = MARCA, y = venta_total_menudeo)) + 
  geom_bar (stat="identity") +
  ggtitle('Venta total nacional de de vehículos pesados por marca 
  2018 Ene -2020 Dic
          (Menudeo)')+
  xlab("Marcas")+
  ylab("Ventas Totales")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
grafico_menudeo_marca

#Venta nacional por marca (mayoreo)
venta_mayoreo_marca <- ddply(datos_compactos_pesados, .(MARCA) ,summarise,
                             venta_total_mayoreo= sum(MAYOREO))
venta_mayoreo_marca

grafico_mayoreo_marca <- ggplot(venta_mayoreo_marca, aes(x = MARCA, y = venta_total_mayoreo)) + 
  geom_bar (stat="identity") +
  ggtitle('Venta total nacional de de vehículos pesados por marca 
  2018 Ene -2020 Dic
          (Mayoreo)')+
  xlab("Marcas")+
  ylab("Ventas Totales")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
grafico_mayoreo_marca


#Venta nacional por marca (mayoreo + menudeo)
venta_total_marca <- ddply(datos_compactos_pesados, .(MARCA) ,summarise,
                             venta_total= sum(MAYOREO + MENUDEO))
venta_total_marca

grafico_venta_total_marca <- ggplot(venta_total_marca, aes(x = MARCA, y = venta_total)) + 
  geom_bar (stat="identity") +
  ggtitle('Venta total nacional de de vehículos pesados por marca 
  2018 Ene -2020 Dic
          (Mayoreo+Menudeo)')+
  xlab("Marcas")+
  ylab("Ventas Totales")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
grafico_venta_total_marca

#Venta nacional por fuente de energia
head(datos_compactos_pesados)
venta_fuente_energia <- ddply(datos_compactos_pesados,.(FUENTE_DE_ENERGIA) ,summarise,
                           venta_menudeo= sum(MENUDEO), venta_mayoreo=sum(MAYOREO))
venta_fuente_energia  

grafico_venta_fuente_energia <- ggplot(venta_fuente_energia, aes(x = FUENTE_DE_ENERGIA, y = venta_menudeo)) + 
  geom_bar (stat="identity") +
  ggtitle('Venta total nacional de de vehículos pesados por Fuente de energia 
  2018 Ene -2020 Dic')+
  xlab("Fuente de Energia")+
  ylab("Ventas")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
grafico_venta_fuente_energia

#Grafico mas complejo de fuente de energia y Segmento
grafico_energia_y_segmento <- ggplot(datos_compactos_pesados, aes(x = FUENTE_DE_ENERGIA, y = MENUDEO)) + 
  geom_bar (stat="identity") +
  ggtitle('Venta total nacional de de vehículos pesados por Fuente de energia 
  2018 Ene -2020 Dic')+
  xlab("Fuente de Energia")+
  ylab("Ventas")+
  facet_wrap(.~ SEGMENTO)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
grafico_energia_y_segmento


#Otro grafico complejo comparango marcas, segmento y ventas
grafico_marca_y_segmento <- ggplot(datos_compactos_pesados, aes(x = MARCA, y = MENUDEO, fill= FUENTE_DE_ENERGIA)) + 
  geom_bar (stat="identity") +
  ggtitle('Venta total nacional de de vehículos pesados por marca
  (Separados por segmento)
  2018 Ene -2020 Dic')+
  xlab("Marca")+
  ylab("Ventas")+
  facet_wrap(.~ SEGMENTO)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
grafico_marca_y_segmento

#Filtramos los electricos e hibridos
Electricos_e_hibridos  <- datos_compactos_pesados %>% filter((FUENTE_DE_ENERGIA== "Eléctrico")| (FUENTE_DE_ENERGIA=="Híbrido"))

#Grafico con la principales marcas provedoras de vehiculos pesados electricos e hibridos
grafico_electricos_hibridos <- ggplot(Electricos_e_hibridos, aes(x = MARCA, y = MENUDEO)) + 
  geom_bar (stat="identity") +
  ggtitle('Principales provedores de vehiculos electricos e hibridos 
  2018 Ene -2020 Dic')+
  xlab("Marca")+
  ylab("Ventas")+
  facet_wrap(.~ FUENTE_DE_ENERGIA)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

grafico_electricos_hibridos

#Time series de ventas de vehiculos hibridos                
ventas_hibridos  <- datos_compactos_pesados %>% filter(FUENTE_DE_ENERGIA=="Híbrido")
head(ventas_hibridos)
ts.ventas.hibridos <- ts(ventas_hibridos$MENUDEO,st=2018,fr=12)
ts.plot(ts.ventas.hibridos)


#################################################################333
#FORECASTING
#Despues realizaremos una prediccion de los proximos 6 meses para la venta de
#vehiculos hibridos pesados
autoarima_hibrido <- auto.arima(ts.ventas.hibridos)
forecast_hibrido <- forecast(autoarima_hibrido, h = 6)
forecast_hibrido
plot(forecast_hibrido)
#Al graficar los residuos vemos que hay mucha varianza apartir del segundo
#semestre del 2019
plot(forecast_hibrido$residuals)
qqnorm(forecast_hibrido$residuals)
#Al analizar el grafico de autocorrelacion vemos que solo el primero es el que 
#esta fuera del rango
acf(forecast_hibrido$residuals)
pacf(forecast_hibrido$residuals)

#Al analizar la exactitud de la prediccion, vemos que MAPE(error porcentual medio
#absoluto) es infinito, esto se debe a que varios valores que son igual a 0 en
#los meses en los que no se vendio ningun auto hibrido, ademas contemplando la alta
#varianza y la poca cantidad de datos.
accuracy(forecast_hibrido)

#Prediccion de ventas de vehiculos pesados en los proximos 6 meses (contemplando
#todas las fuentes de energia)

autoarima_ventas_total <- auto.arima(ts.ventas.menudeo)
autoarima_ventas_total
forecast_ventas <- forecast(autoarima_ventas_total, h = 6)
forecast_ventas
plot(forecast_ventas)

#vemos que tambien hay mucha varianza
plot(forecast_ventas$residuals)
qqnorm(forecast_ventas$residuals)
acf(forecast_ventas$residuals)

#Y ocurre el mismo problema con la exactitud
accuracy(forecast_hibrido)

