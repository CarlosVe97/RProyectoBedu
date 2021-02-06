
# Registro Administrativo de la Industria Automotriz de Vehículos Ligeros. 
#Venta de vehículos híbridos y eléctricos a nivel nacional en el periodo 2016 Ene -2020 Oct. 

#Los datos mostrados a continuación fueron obtenidos de:
# INEGI. Datos primarios, Registro administrativo de la industria automotriz de vehículos ligeros
# https://www.inegi.org.mx/datosprimarios/iavl/#Datos_abiertos

#Estos se encuentran descargados previamente en nuestro directorio de trabajo
#setwd("C:/Users/Carolina/Desktop/Híbridos 2016-2020/Datos")

#Cargamos las librerias necesarias 
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(plyr)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(tidyr)))

#Leemos los archivos del directorio para importarlos a R.
h_2016 <- read.csv("hibrido_2016.csv")
h_2017 <- read.csv("hibrido_2017.csv")
h_2018 <- read.csv("hibrido_2018.csv")
h_2019 <- read.csv("hibrido_2019.csv")
h_2020 <- read.csv("hibrido_2020.csv")

names(h_2016);names(h_2017);names(h_2018);names(h_2019);names(h_2020)
str(h_2016) #Conocemos la estructura de nuestra base de datos 

#Con ello nos damos cuenta que las columnas $ ..PROD_EST y  $ COBERTURA no son útiles

#Es conveniente tener todos los archivos en un único archivo 
hlist <- list(h_2016,h_2017,h_2018,h_2019,h_2020)
util  <- lapply(hlist, select, ANIO:VEH_HIBRIDAS)

data_h <- do.call(rbind, util) #Combinando en un único dataframe

#Es necesario cargar los archivos adicional correspondiente a ID_ENTIDAD
entidad <- read.csv("entidad_federativa.csv")

#De tal forma que podemos unir ambas bases de datos para que quere en una sola
datos_hibrid <- merge(x=data_h, y=entidad, by="ID_ENTIDAD")

datos_hibrid <- arrange(datos_hibrid,ANIO,ID_MES) #Primero ordenamos por año y mes


names(datos_hibrid) #nombre de las columnas y el orden de aparición
datos_hibrid <- datos_hibrid[, c(2,3,1,7,4,5,6)] #nuevo orden de las columnas
head(datos_hibrid)

#Lo guardamos en una base de datos para tenerlo disponible
#hibrid_vehicle <- write.csv()

#Venta vehículos a nivel nacional. Es útil arreglar la fecha
fecha_hibrid <- datos_hibrid %>% unite("FECHA",ANIO:ID_MES,sep="-",remove = F,na.rm=T)
fecha_hibrid <- fecha_hibrid[,c(1,5,6,7,8)] #ordenando las columnas
d <- rep(1,dim(fecha_hibrid)[1]) #Para poder transformar a formato de fecha, un truco es argregar una columna numérica y unirla
fecha_hibrid <- cbind(fecha_hibrid,d)
fecha_hibrid <- fecha_hibrid[,c(1,6,2,3,4,5)]
fecha_hibrid <- fecha_hibrid %>% unite("FECHA_COMP",FECHA:d,sep="-",remove = F,na.rm=T) #creando la fecha completa 
fecha_hibrid <- fecha_hibrid[,c(1,4,5,6,7)]

fecha_hibrid <- mutate(fecha_hibrid,FECHA_COMP = as.Date(FECHA_COMP,"%Y-%m-%d")) #cambiando el formato
fecha_hibrid <- mutate(fecha_hibrid, fecha= format.Date(FECHA_COMP,"%Y-%m")) # dejando la fecha real
fecha_hibrid <- fecha_hibrid[,c(6,2,3,4,5)]


#........... Análisis gráfico ...............

#### SERIES DE TIEMPO DEL PERIODO ENE 2016- OCT 2020

#Venta nacional de vehículos eléctricos
elec_nac <- ddply(fecha_hibrid, .(fecha) ,summarise, ELEC_PROM= mean(VEH_ELECTR))
ts.h <- ts(elec_nac$ELEC_PROM,st=2016,fr=12)
ts.plot(ts.h)

#Venta nacional de vehículos híbridos plug-in
hp_nac <- ddply(fecha_hibrid, .(fecha) ,summarise, PLUGIN_PROM= mean(VEH_HIBRIDAS_PLUGIN))
ts.plugin <- ts(hp_nac$PLUGIN_PROM,st=2016,fr=12)
ts.plot(ts.plugin)

#Venta nacional de vehículos híbridos
hib_nac <- ddply(fecha_hibrid, .(fecha) ,summarise, HIB_PROM= mean(VEH_HIBRIDAS))
ts.hibrid <- ts(hib_nac$HIB_PROM,st=2016,fr=12)
ts.plot(ts.hibrid)


#### Gráficos de barras de ventas de vehículos por entidad federativa #####


#Por entidad federativa las ventas de automóviles eléctricos 
ele_ent <- ddply(fecha_hibrid, .(DESC_ENTIDAD) ,summarise, ELEC_PROM= mean(VEH_ELECTR))


electric <- ggplot(ele_ent, aes(x = DESC_ENTIDAD, y = ELEC_PROM)) + 
  geom_bar (stat="identity") +
  ggtitle('Venta nacional de de vehículos eléctricos durante el periodo 2016 Ene -2020 Oct ')+
  xlab("Entidades federativas")+
  ylab("Promedio de ventas")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

electric

#Por entidad federativa las ventas de automóviles híbridos-plugin
plug_ent<-ddply(fecha_hibrid, .(DESC_ENTIDAD) ,summarise, PLUGIN_PROM= mean(VEH_HIBRIDAS_PLUGIN))

hibrid_plugin <- ggplot(plug_ent, aes(x = DESC_ENTIDAD, y = PLUGIN_PROM)) + 
  geom_bar (stat="identity") +
  ggtitle('Venta nacional de de vehículos plugin-híbridos  durante el periodo 2016 Ene -2020 Oct ')+
  xlab("Entidades federativas")+
  ylab("Promedio de ventas")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

hibrid_plugin


#Por entidad federativa las ventas de automóviles híbridos-plugin
hib_ent<- ddply(fecha_hibrid, .(DESC_ENTIDAD) ,summarise, HIB_PROM= mean(VEH_HIBRIDAS)) 

hibrid_graf <- ggplot(hib_ent, aes(x = DESC_ENTIDAD, y = HIB_PROM)) + 
  geom_bar (stat="identity") +
  ggtitle('Venta nacional de de vehículos híbridos  durante el periodo 2016 Ene -2020 Oct ')+
  xlab("Entidades federativas")+
  ylab("Promedio de ventas")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

hibrid_graf