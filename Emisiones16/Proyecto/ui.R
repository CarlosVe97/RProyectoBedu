#Ejemplo 2. Normal con Labels
setwd("C:/Users/Linette/Documents/BEDU/Programacion-con-R-Santander-master/Proyecto/Emisiones16/proyecto")


library(class)
library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)

#-----------------------------------------------------------------

em_16 <- read.csv("em_16.csv")

#Nombres de las emisiones en un arreglo
aux <- names(em_16)
nom_emisiones <- aux[5:19]

#Nombre de entidades
nom_entidades <- c("CDMX","EDOMEX","TIZA")


#--------------------------------------------------------

shinyUI(
    pageWithSidebar(
        headerPanel("Proyecto"),
        sidebarPanel(
            p("Emisiones"), 
            selectInput("x", "Seleccione la variable: ",
                        choices = names(em_16[6:20])),
            p("Zonas"), 
            selectInput("y", "Seleccione la vaariable: ",
                        choices = c("CDMX","EDOMEX","TIZA","ZMVM")),
            p("Vehiculo"), 
            selectInput("z", "Seleccione la vaariable: ",
                        choices = c("CDMX","EDOMEX","TIZA"))
            
        ),
        mainPanel(
            
            
            #Agregando pestaÃ±as
            tabsetPanel(
                tabPanel("Emisiones 2016",   #Pestaña de Plots
                         h3(textOutput("output_text")), 
                         plotOutput("output_plot"), 
                ),
                
                tabPanel("Vehiculos - CO2 ",   #Pestaña de Plots
                         h3(textOutput("output_text2")), 
                         plotOutput("output_plot2"), 
                ),
                
                
                tabPanel("imágenes",  #Pestaña de imágenes
                         img( src = "cor_mtcars.png", 
                              height = 450, width = 450)
                ), 
                
                
                tabPanel("Dplyr", verbatimTextOutput("summary")),
                
                tabPanel("Table", tableOutput("table")),
                tabPanel("Data Table", dataTableOutput("data_table"))
            )
        )
    )
    
)

