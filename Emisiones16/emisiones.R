# RegistroInventario de emisiones
# Emisiones en la CDMX, EDOMEX, Tizayuca y Zona Metropolitana del Valle de M�xico en el a�o 2016
# divido por categor�a de emisi�n.

#Los datos mostrados a continuaci�n fueron obtenidos de:
# Gobierno de la ciudad de M�xico - Calidad del Aire. Datos, Inventario de emisiones
# http://www.aire.cdmx.gob.mx/default.php?opc=%27aKBhnmc=&r=b3BlbmRhdGEvaW52ZW50YXJpb19lbWlzaW9uZXMvSW52ZW50YXJpb19FbWlzaW9uZXMvSUVfMjAxNi5jc3Y=

#Estos se encuentran descargados previamente en nuestro directorio de trabajo
#setwd("C:/Users/Linette/Documents/BEDU/Programacion-con-R-Santander-master/Proyecto/Emisiones16")


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

#........... An�lisis gr�fico ...............

#Nombres de las emisiones en un arreglo
aux <- names(em_16)
nom_emisiones <- aux[5:19]

#Nombre de entidades
nom_entidades <- c("ZMVM","CDMX","EDOMEX","TIZA")



# Histograma por entidad            --- Autos particulares --- 

#Filtrado para la categor�a de autos particulares
filtrado <- filter(em_16, id_categoria == 80)
particulares <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:4) {
  for (j in 1:16) {
    
    elementos[[length(elementos)+1]] <- particulares[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creaci�n del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,4),
                     registros=lista)

#Gr�fica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de autos particulares en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al a�o")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8))
#-----------------------------------------------------------

# Histograma por entidad            --- Camionetas (SUV) --- 

#Filtrado para la categor�a de autos particulares
filtrado <- filter(em_16, id_categoria == 81)
camioneta <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:4) {
  for (j in 1:16) {
    
    elementos[[length(elementos)+1]] <- camioneta[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creaci�n del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,4),
                     registros=lista)

#Gr�fica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de Camionetas (SUV) en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al a�o")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8))
#-----------------------------------------------------------

# Histograma por entidad            --- Taxis --- 

#Filtrado para la categor�a de autos particulares
filtrado <- filter(em_16, id_categoria == 82)
taxis <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:4) {
  for (j in 1:16) {
    
    elementos[[length(elementos)+1]] <- taxis[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creaci�n del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,4),
                     registros=lista)

#Gr�fica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de Taxis en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al a�o")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8))
#-----------------------------------------------------------

# Histograma por entidad            --- Vagonetas y Combis --- 

#Filtrado para la categor�a de autos particulares
filtrado <- filter(em_16, id_categoria == 83)
combis <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:4) {
  for (j in 1:16) {
    
    elementos[[length(elementos)+1]] <- combis[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creaci�n del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,4),
                     registros=lista)

#Gr�fica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de Vagonetas y Combis en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al a�o")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8))
#-----------------------------------------------------------

# Histograma por entidad            --- Microbuses --- 

#Filtrado para la categor�a de autos particulares
filtrado <- filter(em_16, id_categoria == 84)
microbuses <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:4) {
  for (j in 1:16) {
    
    elementos[[length(elementos)+1]] <- microbuses[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creaci�n del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,4),
                     registros=lista)

#Gr�fica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de Microbuses en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al a�o")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8))
#-----------------------------------------------------------

# Histograma por entidad            --- Pick up y veh. de carga hasta 3.8 t --- 

#Filtrado para la categor�a de autos particulares
filtrado <- filter(em_16, id_categoria == 85)
pickup <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:4) {
  for (j in 1:16) {
    
    elementos[[length(elementos)+1]] <- pickup[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creaci�n del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,4),
                     registros=lista)

#Gr�fica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de Pick up y veh. de carga hasta 3.8 t en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al a�o")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8))
#-----------------------------------------------------------

# Histograma por entidad            --- Tractocamiones --- 

#Filtrado para la categor�a de autos particulares
filtrado <- filter(em_16, id_categoria == 86)
tractocamiones <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:4) {
  for (j in 1:16) {
    
    elementos[[length(elementos)+1]] <- tractocamiones[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creaci�n del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,4),
                     registros=lista)

#Gr�fica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de Tractocamiones en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al a�o")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8))
#-----------------------------------------------------------

# Histograma por entidad            --- Autobuses --- 

#Filtrado para la categor�a de autos particulares
filtrado <- filter(em_16, id_categoria == 87)
autobuses <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:4) {
  for (j in 1:16) {
    
    elementos[[length(elementos)+1]] <- autobuses[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creaci�n del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,4),
                     registros=lista)

#Gr�fica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de Autobuses en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al a�o")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8))
#-----------------------------------------------------------

# Histograma por entidad            --- Veh�culos de carga mayores a 3.8 t. --- 

#Filtrado para la categor�a de autos particulares
filtrado <- filter(em_16, id_categoria == 88)
carga <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:4) {
  for (j in 1:16) {
    
    elementos[[length(elementos)+1]] <- carga[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creaci�n del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,4),
                     registros=lista)

#Gr�fica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de Veh�culos de carga mayores a 3.8 t. en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al a�o")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8))
#-----------------------------------------------------------

# Histograma por entidad            --- Motocicletas --- 

#Filtrado para la categor�a de autos particulares
filtrado <- filter(em_16, id_categoria == 89)
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

#Creaci�n del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,4),
                     registros=lista)

#Gr�fica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de Motocicletas en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al a�o")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8))
#-----------------------------------------------------------

# Histograma por entidad            --- Metrobuses/Mexib�s --- 

#Filtrado para la categor�a de autos particulares
filtrado <- filter(em_16, id_categoria == 90)
metrobus <- select(filtrado,Valor_PM10:Valor_HFC)

#Elementos de emisiones en un arreglo
elementos <- list()

for (i in 1:4) {
  for (j in 1:16) {
    
    elementos[[length(elementos)+1]] <- metrobus[i,j]
    
  }
}

lista <- do.call(rbind, elementos)
head(lista)

#Creaci�n del data frame para el histograma agrupado 
survey <- data.frame(zonas=rep(nom_entidades,each= 15),
                     emisiones=rep(nom_emisiones,4),
                     registros=lista)

#Gr�fica
ggplot(survey, aes(x=emisiones, y=registros, fill=zonas)) +
  ggtitle('Emisiones de Metrobuses/Mexib�s en el 2016') +
  xlab("Emisiones") +
  ylab("Toneladas al a�o")+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8))
#-----------------------------------------------------------