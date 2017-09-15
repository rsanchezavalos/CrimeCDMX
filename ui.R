library(leaflet)


navbarPage("Crimen en la CDMX", id="nav",

  tabPanel("General",
    div(class="outer",style='width:100%;text-align:center;',
      tags$head(
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      leafletOutput("map", width="100%", height="100%"),
        
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 40, left = 0, right = "auto", bottom = "auto",
        width = '35%', height = '100%',

        h2("Explora el crímen por Cuadrante y Georreferencia"),
        h4("Selecciona el nivel de agregación del mapa"),
        div(radioButtons("map_style", "",
                         c("Cuadrante" = "cuadrant",
                           "Georreferencia" = "lat_long")), 
            style ="margin-left:20%;font-size:100%;"),
        br(),
        h4("Puedes dara click en el cuadrante/georreferencia para observar su información o realiza un filtro."),
        br(),
        div(selectInput("map_crime", "Tipo de Crimen", c("All", unique(as.character(data_lat_long$crime))),width = "100%"),
          selectInput("map_cuadrante", "Cuadrante", c("All", unique(as.character(data_lat_long$cuadrante))),width = "100%"),
          sliderInput("map_year", "Año:", min = 2013, max = 2016, value = c(2013,2016),width = "100%")),
        br(),
        fluidRow(
          column(6,h5(paste0("Número total de Crimenes:"),textOutput("crime_text")), h3(textOutput("total_crimenes"))),
          column(6,h5("Tasa de Crímenes por 100 mil H."), h3(textOutput("total_tasa")))),
                 
        br(),
        div(
          h5("Top 5 Crímenes"),
          plotOutput("top5", height = 100),
          h5("Serie de Tiempo (Dame un segundo - tardo en cargar ;)"),
          plotOutput("timeseries_1", height = 200),
          style = "margin:auto;font-size:120%;text-align:center;")
        ) 
    )
  ),
  
  tabPanel("Temporal",
           div(class="outer_2",style='width:100%;text-align:center;',
               tags$head(
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = FALSE, top = 40, left = 0, right = "auto", bottom = "auto",
                         width = '35%', height = '100%',
                         h2("Análisis Temporal"),
                         h4("Selecciona filtros para el análisis"),
                         br(),
                         div(selectInput("map_crime_2", "Tipo de Crimen", c("All", unique(as.character(data_lat_long$crime))),width = "100%"),
                             selectInput("map_cuadrante_2", "Cuadrante", c("All", unique(as.character(data_lat_long$cuadrante))),width = "100%"),
                             sliderInput("map_year_2", "Año:", min = 2013, max = 2016, value = c(2013,2016),width = "100%")),
                         br(),
                         leafletOutput("map_2")
                         ),
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = FALSE, top = 60, left = '35%', right = "auto", bottom = "auto",
                         width = '65%', height = '100%',
                         br(),
                         br(),
                         div(h4("Serie de Tiempo", height = 'auto'),
                             br(),
                             plotOutput("geom_bar_2", height = 800))
                         ))
  ),
  
  tabPanel("Análisis por Delito",
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = FALSE, top = 40, left = 0, right = "auto", bottom = "auto",
                         width = '35%', height = '100%',

                         h2("Análisis por Delito"),
                         h4("Selecciona filtros para el análisis"),
                         br(),
                         div(selectInput("map_crime_3", "Tipo de Crimen", c("All", unique(as.character(data_lat_long$crime))),width = "100%"),
                             sliderInput("map_year_3", "Año:", min = 2013, max = 2016, value = c(2013,2016),width = "100%")),
                         br()
                         ),
           
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = FALSE, top = 60, left = '35%', right = "auto", bottom = "auto",
                         width = '65%', height = '100%',
                         div(h4("Cuadrantes con Mayor Número de Crimenes"),
                             br(),
                             # plotOutput("geom_bar_3", height = 800),
                             plotOutput("top_cuadrante_3", height = 800)
                             
                             ))
           
  ),  
  
  tabPanel("FullData",
           div(   
      
      fluidRow(
        column(4,
               selectInput("year",
                           "Año:",
                           c("All",
                             unique(as.character(data_lat_long$year))))
        ),
        column(4,
               selectInput("type_crime",
                           "Tipo de Crimen:",
                           c("All",
                             unique(as.character(data_lat_long$crime))))
        ),
        column(4,
               selectInput("type_cuadrante",
                           "Cuadrante:",
                           c("All",
                             unique(as.character(data_lat_long$cuadrante))))
        )
      )
    ),
    hr(),
    DT::dataTableOutput("table"),
    br()
    
    # Button TODO()
    # downloadButton("CrimeCDMX", "Download")
    
  ),

  conditionalPanel("false", icon("crosshair"))
)
