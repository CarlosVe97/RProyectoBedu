# RegistroInventario de emisiones
# Emisiones en la CDMX, EDOMEX, Tizayuca y Zona Metropolitana del Valle de México en el año 2016
# divido por categoría de emisión.

#Los datos mostrados a continuación fueron obtenidos de:
# Gobierno de la ciudad de México - Calidad del Aire. Datos, Inventario de emisiones
# http://www.aire.cdmx.gob.mx/default.php?opc=%27aKBhnmc=&r=b3BlbmRhdGEvaW52ZW50YXJpb19lbWlzaW9uZXMvSW52ZW50YXJpb19FbWlzaW9uZXMvSUVfMjAxNi5jc3Y=

#Estos se encuentran descargados previamente en nuestro directorio de trabajo
setwd("C:/Users/Linette/Documents/BEDU/Programacion-con-R-Santander-master/Proyecto/Emisiones16/datos")


#Cargamos las librerias necesarias 
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(plyr)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(tidyr)))

#Leemos el archivo para importarlo a R.
ie_2016 <- read.csv("IE_2016.csv")

#Conocemos la estructura de nuestra base de datos 
names(ie_2016)
str(ie_2016) 

#Es necesario cargar los archivos adicionales correspondientes a:

#ID_ENTIDAD
entidad <- read.csv("cat_entidad.csv")
entidad <- select(entidad, id_entidad:cve_entidad)
#ID_CATEGORIA
categoria <- read.csv("cat_categoria.csv")

#De tal forma que podemos unir las bases de datos para que quede en una sola
em_16 <- merge(x=ie_2016, y=entidad, by="id_entidad")
em_16 <- merge(x=em_16, y=categoria, by="id_categoria")

#Nuevo orden de columnas
em_16 <- em_16[, c(1,20,2,19,3:18)]
head(em_16)

#Orden por categoría, y entidad
em_16 <- arrange(em_16,id_categoria,id_entidad) 

#write.csv(em_16,"em_16.csv")
#........... Análisis gráfico ...............

#Nombres de las emisiones en un arreglo
aux <- names(em_16)
nom_emisiones <- aux[5:19]

#Nombre de entidades
nom_entidades <- c("CDMX","EDOMEX","TIZA")

#-----------------------------------------------------------

#Gráfica de barras  ------ Emisión de CO2 de todas las categorías  ------

filtrado <- filter(em_16, id_entidad == 4)
todo_CO2 <- select(filtrado,id_categoria, nom_categoria, Valor_CO2)
todo_CO2 <- as.data.frame(todo_CO2)

m_todo_co2 <- mean(todo_CO2$Valor_CO2)
maximo <- max(todo_CO2$Valor_CO2)
nom_maximo <- select((filter(todo_CO2, Valor_CO2 == maximo)), nom_categoria)

#Gráfica
ggplot(todo_CO2, aes(x=id_categoria, y=Valor_CO2)) +
  ggtitle('Emisión de CO2 generado en el 2016 (ZMVM)', paste("Media=",m_todo_co2, "   " , " Máximo  ~  ", nom_maximo[1] ,"=", maximo)) +
  xlab("Categoría") +
  ylab("Toneladas al año")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8), fill = "blue")+
  geom_hline(yintercept =  m_todo_co2, col = "purple", lwd = 1, lty =2) 
  #theme(axis.text.x = element_text(angle = 90))


#-----------------------------------------------------------

# Gráfico de barras           --- Autos particulares --- 

#Filtrado para la categoría de autos particulares
filtrado <- filter(em_16, id_categoria == 80, id_entidad != 4)
particulares <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:3) {
  for (j in 1:15) {
    
    elementos[[length(elementos)+1]] <- particulares[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creación del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,3),
                     registros=lista)

#Gráfica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de autos particulares en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al año")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#-----------------------------------------------------------


# Gráfico de barras            --- Vagonetas y Combis --- 

#Filtrado para la categoría de vagonetas y combis
filtrado <- filter(em_16, id_categoria == 83, id_entidad != 4)
combis <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:3) {
  for (j in 1:15) {
    
    elementos[[length(elementos)+1]] <- combis[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creación del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,3),
                     registros=lista)

#Gráfica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de Vagonetas y Combis en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al año")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#-----------------------------------------------------------

# Gráfico de barras              --- Autobuses --- 

#Filtrado para la categoría de autobuses
filtrado <- filter(em_16, id_categoria == 87, id_entidad != 4)
autobuses <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:3) {
  for (j in 1:15) {
    
    elementos[[length(elementos)+1]] <- autobuses[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creación del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,3),
                     registros=lista)

#Gráfica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de Autobuses en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al año")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#-----------------------------------------------------------


# Gráfico de barras             --- Motocicletas --- 

#Filtrado para la categoría de motocicletas
filtrado <- filter(em_16, id_categoria == 89, id_entidad != 4)
motos <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:4) {
  for (j in 1:16) {
    
    elementos[[length(elementos)+1]] <- motos[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creación del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,4),
                     registros=lista)

#Gráfica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de Motocicletas en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al año")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#-----------------------------------------------------------


# Grafica de barras  ------  Comparación de emisión CO2 entre vehículos dividido en las 3 zonas ----

#Filtrado de los datos que se desean graficar
filtrado <- filter(em_16, id_categoria > 79 & id_categoria < 91, id_entidad != 4)
co2 <- select(filtrado,nom_categoria,cve_entidad,Valor_CO2)
co2 <- as.data.frame(co2)

#Promedio de CO2 por tipo de vehículo
m_co2veh <- mean(co2$Valor_CO2)

#Gráfica
ggplot(co2, aes(x=nom_categoria, y=Valor_CO2, fill=cve_entidad)) +
  ggtitle('Emisión de CO2 generado por zonas', paste("Media =",m_co2veh)) +
  xlab("Vehículo") +
  ylab("Toneladas al año")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8))+
  geom_hline(yintercept =  m_co2veh, col = "purple", lwd = 1, lty =2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#-----------------------------------------------------------

# Gráfico de barras   -------- Comparación de emisión CO2 entre vehículos en la ZMVM ----------------
filtrado <- filter(em_16, id_categoria > 79 & id_categoria < 91, id_entidad == 4)
zmvm_co2 <- select(filtrado,nom_categoria, Valor_CO2)
zmvm_co2 <- as.data.frame(zmvm_co2)

m_co2 <- mean(zmvm_co2$Valor_CO2)

#Gráfica
ggplot(zmvm_co2, aes(x=nom_categoria, y=Valor_CO2)) +
  ggtitle('Emisión de CO2 generado en la ZMVM', paste("Media=",m_co2)) +
  xlab("Vehículo") +
  ylab("Toneladas al año")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8), fill = rainbow(11))+
  geom_hline(yintercept =  m_co2, col = "purple", lwd = 1, lty =2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

# ----- PORCENTAJES Y SUMA  DE CO2 (VEHICULOS) -----------------

#porcentajes
slices <- select(zmvm_co2,Valor_CO2)
pct <- round((slices/sum(slices)*100), digits = 3)
porcentajes <- cbind(zmvm_co2,Porcentaje=pct) #DATA FRAME DE PORCENTAJES

suma_co2 <- sum(zmvm_co2[,2]) #SUMA D CO2 POR VEHÍCULOS 



