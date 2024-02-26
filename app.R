
##Lista de paquetes que se necesitan
packages = c("shiny","DT","data.table","sf",
             "tidyverse","viridis","scales","ggspatial")

## Instalar y/o cargar paquetes
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

rm(list=ls())
#setwd("F:\\repositorio\\shynyapp\\map_maker_cdmx")

alcaldia<-st_read("shp\\alcaldias.shp")%>%
  st_transform('+proj=longlat +datum=WGS84')%>%
  mutate(NOMGEO=case_when(CVEGEO=="09003"~"Coyoacán",
                          CVEGEO=="09010"~"Álvaro Obregón",
                          CVEGEO=="09011"~"Tláhuac",
                          CVEGEO=="09015"~"Cuauhtémoc",
                          CVEGEO=="09002"~"Azcapotzalco",
                          CVEGEO=="09004"~"Cuajimalpa de Morelos",
                          CVEGEO=="09005"~"Gustavo A. Madero",
                          CVEGEO=="09006"~"Iztacalco",
                          CVEGEO=="09007"~"Iztapalapa",
                          CVEGEO=="09008"~"La Magdalena Contreras",
                          CVEGEO=="09009"~"Milpa Alta",
                          CVEGEO=="09012"~"Tlalpan",
                          CVEGEO=="09013"~"Xochimilco",
                          CVEGEO=="09014"~"Benito Juárez",
                          CVEGEO=="09016"~"Miguel Hidalgo",
                          CVEGEO=="09017"~"Venustiano Carranza",
                          TRUE~"Sin dato"))


ui <- fluidPage(
  
  titlePanel("Generador de mapas a nivel de alcaldías, Ciudad de México"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput("valores","Inserta valores"),
      
      textInput("indicador","Nombre del indicador"),
      
      numericInput("direccion","Invertir colores",1,min = -1,max = 1,step = 2),
      
      actionButton("boton", "Crear mapa"),
      
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
      tags$footer(strong("Daniel Acosta Chávez")),
      tags$footer("https://www.linkedin.com/in/acosta-chavez-daniel/"),
      tags$footer("https://rpubs.com/Dacosta"),
      tags$footer("https://github.com/DacostaGeo")
      
    ), # Close sidebarPanel
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("Mapa",
                 h3("Intrucciones"),
                 
                 p("Copie y pege de un archivo de hoja de cálculo como Excel o programa similar 
                   los valores que desea usar para elaborar un mapa temático a nivel de 
                   alcaldías para la Ciudad de México. La información que se ingrese debe 
                   estar en orden por claves geoestadísticas, es decir, empezando por la 
                   alcaldía Azcapotzalco (09002) y terminando con la alcaldía Venustiano 
                   Carranza (09017)."),
                 
                 br(),
                 
                 h5(strong("Valores de ejemplo: porcentaje de población en situación de pobreza, CONEVAL 2020")),
                 
                 br(),
                 
                 p("24.2 27.1 32.5 33.8 25.2 43.9 42.5 54.7 37.7 42.4 39.7 48.2 7.9 20.9 13.5 30.0"),
                 
                 br(),
                 br(),
                 
                 plotOutput("map")
                 
        ), # Close tabPanel
        
        tabPanel("Datos",
                 dataTableOutput("table")
        )
      ) # Close tabsetPanel
      
    ) # Close mainpanel
    
  ) # Close sidebarLayout
)

server <- function(input, output, session) {
  
  assing_values<-eventReactive(input$boton, {
    
    req(input$valores)
    req(input$indicador)
    
    valores<-as.numeric(unlist(strsplit(input$valores," ")))
    
    CVEGEO<-c("09002","09003","09004","09005","09006","09007",
              "09008","09009","09010","09011","09012","09013",
              "09014","09015","09016","09017")
    
    tab<-data.frame(CVEGEO,valores)
    
    alcaldia<-alcaldia%>%
      left_join(tab,by="CVEGEO")
    
    }) # Close server
  
#########################################################################
  
  output$map<-renderPlot({
    
    ggplot() +
      geom_sf(aes(fill = valores),color="gray40",lwd=0.005,data=assing_values())+
      scale_fill_viridis_c(option = "D",direction = input$direccion)+
      theme_bw()+annotation_scale()+
      annotation_north_arrow(location='tr',height = unit(1, "cm"),width = unit(1, "cm"))+
      labs(x="Longitud",y="Latitud",fill=input$indicador)
    
  }) # Close renderPlot
  

#########################################################################
  
  output$table <- renderDataTable({
    
    req(input$valores)
    
    dataframe<-assing_values()%>%
      st_drop_geometry()
  
  }) # Close renderDataTable
  
######################################################################### 
  
} # Close server


shinyApp(ui, server)


