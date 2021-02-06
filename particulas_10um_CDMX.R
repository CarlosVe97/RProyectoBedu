library(readxl)
library('xts')
library(tidyverse)
library(lubridate)
library(timetk)
library(dplyr)
library(PerformanceAnalytics)
library(zoo)

getwd()
setwd('/20RAMA')
dir()

# Este bloque de codigo es para importar el excel y darle formato a las fechas
# y la hora, juntandolos en una columna llamada Date
particulas_10 <- read_xls("2020PM10.xls", na="-99")
particulas_10 <- as.data.frame(particulas_10)
tail(particulas_10)
str(particulas_10)
particulas_10$FECHA <- as.Date(particulas_10$FECHA, format="%Y-%m-%d")
paste(particulas_10$HORA, "00", sep=":")
particulas_10$HORA <- paste(particulas_10$HORA, "00", sep=":")
particulas_10$Date <- as.POSIXct(paste(particulas_10$FECHA, particulas_10$HORA), format="%Y-%m-%d %H:%M")
tail(particulas_10)
#Una vez que tenemos el DF con formato, realizamos un TS
#Primero hacemos un subset con las columnas que queremos trabajar

particulas_10sub=particulas_10[,c("Date","ATI")]
particulas_10sub <- na.omit(particulas_10sub)
particulas_10xts<-as.xts(x=particulas_10sub[,"ATI"],order.by=particulas_10sub[,"Date"])
colnames(particulas_10xts)=colnames(particulas_10sub)[-1]

particulas_10BJU=particulas_10[,c("Date","BJU")]
particulas_10BJU <- na.omit(particulas_10BJU)
particulas_10BJUxts<-as.xts(x=particulas_10BJU[,"BJU"],order.by=particulas_10BJU[,"Date"])
colnames(particulas_10BJUxts)=colnames(particulas_10BJU)[-1]


#Ya que tenemos el xts graficamos datos de BJU y ATI
ggplot(data = NULL) +
  geom_line(data = particulas_10BJUxts, aes(x = Index, y = BJU), color='Blue') +
  geom_line(data = particulas_10xts, aes(x = Index, y = ATI), color='Red')
