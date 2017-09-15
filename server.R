library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(tidyverse)
library(stringr)


values <- reactiveValues()

function(input, output, session) {

  # Set up Base map
  output$map <- renderLeaflet({
    leaflet(cuadrantes) %>% 
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      setView(lng = -99.3022374, lat = 19.3731912, zoom = 11)
    })
  
  # Set up Base map 2
  output$map_2 <- renderLeaflet({
    leaflet(cuadrantes) %>% 
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      setView(lng = -99.10, lat = 19.3731912, zoom = 10)
  })
  
  
  # Reactive DB Filter
  InBounds <- reactive({
    data <- violencia_cuadrantes
    
    if (input$map_year != "All") {
      data <- filter(data, year >= input$map_year[1] &  year <= input$map_year[2])
    }
    if (input$map_crime != "All") {
      data <- data[data$crime == input$map_crime,]
    }
    if (input$map_cuadrante != "All") {
      data <- data[data$cuadrante == input$map_cuadrante,]
    }
    data
  })

  InBounds_2 <- reactive({
    # Filter DB reactive
    data <- violencia_cuadrantes
    
    if (input$map_year_2 != "All") {
      data <- filter(data, year >= input$map_year_2[1] &  year <= input$map_year_2[2])
    }
    if (input$map_crime_2 != "All") {
      data <- data[data$crime == input$map_crime_2,]
    }
    if (input$map_cuadrante_2 != "All") {
      data <- data[data$cuadrante == input$map_cuadrante_2,]
    }
    data
  })

  InBounds_3 <- reactive({
    # Filter DB reactive
    data <- violencia_cuadrantes
    
    if (input$map_year_3 != "All") {
      data <- filter(data, year >= input$map_year_3[1] &  year <= input$map_year_3[2])
    }
    if (input$map_crime_3 != "All") {
      data <- data[data$crime == input$map_crime_3,]
    }
    data
  })
  
  # Reactive DB Filter (to reduce points in map)
  RenderInBounds <- reactive({
      if (is.null(input$map_bounds))
        return(data_lat_long[FALSE,])
      
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
  
      data <- data_lat_long
      
      if (input$map_year != "All") {
        data <- filter(data, year >= input$map_year[1] &  year <= input$map_year[2])
      }
      if (input$map_crime != "All") {
        data <- data[data$crime == input$map_crime,]
      }
      if (input$map_cuadrante != "All") {
        data <- data[data$cuadrante == input$map_cuadrante,]
      }
      
      if (nrow(data) > 7000){
        return(sample_n(subset(data,
                               latitude >= latRng[1] & latitude <= latRng[2] &
                                 longitude >= lngRng[1] & longitude <= lngRng[2]), 6000))
      } else{
        return(data)
      }
  })
  
  
  # Get top 5 crimes in the InBound
  output$top5 <- renderPlot({
    if (nrow(InBounds()) == 0)
      return(NULL)

    f <- InBounds() %>% group_by(crime) %>% summarise(count = sum(count)) %>%
      arrange(-count) %>% head(5) %>% arrange(count)
    par(mar=c(5.1, 18 ,0.1 ,0.1))
    ggplot(f, aes(x=reorder(crime, count) , y=count, color=crime)) +
      geom_bar(position="dodge",stat="identity") + 
      coord_flip() +  theme(legend.position="none") 
  })
  
  # Create DB and general stats
  temp <- reactive({
    InBounds() %>% group_by(cuadrante) %>%
      summarise(count=sum(count), poblacion = max(population))
  })
  output$crime_text <- renderText({input$map_crime})
  output$total_tasa <- renderText({
    formatC(sum(temp()$count, na.rm = TRUE)/sum(temp()$poblacion,
                                                na.rm = TRUE)*100000, format="d", big.mark=",")})
  output$total_crimenes <-renderText({
    formatC(sum(temp()$count,
                na.rm = TRUE), format="d", big.mark=",")})
  
  # TimeSeries 1 tab
  output$timeseries_1 <- renderPlot({
    if (nrow(InBounds()) == 0)
      return(NULL)
    InBounds() %>% group_by(date) %>% summarise(count = sum(count))  %>%
      ggplot(aes(as.Date(str_c(date, "-01")), count)) +  geom_line() +
      expand_limits(y = 0) + 
      ggtitle("Time Series") + geom_smooth() +  xlab("Fecha")
  })


  
  output$geom_bar_2 <- renderPlot({
    # If no cuadrantes are in view, don't plot
    if (nrow(InBounds_2()) == 0)
      return(NULL)
    
    if (input$map_crime_2 == "All") {
      InBounds_2() %>%  group_by(date, crime) %>% summarise(count = sum(count)) %>%
        ggplot(aes(date, count)) +
        geom_line() + 
        facet_wrap(~crime, scales = 'free')
    
    } else{
      par(mar=c(5.1, 18 ,0.1 ,0.1))
      InBounds_2() %>% group_by(date, crime) %>% summarise(count = sum(count))  %>%
        ggplot(aes(as.Date(str_c(date, "-01")), count, color=crime)) +  geom_line() +
        expand_limits(y = 0) + 
        ggtitle("Time Series") + geom_smooth() +  xlab("Fecha")
    }
  })


  
  output$top_cuadrante_3 <- renderPlot({
    # If no cuadrantes are in view, don't plot
    if (nrow(InBounds_3()) == 0)
      return(NULL)
    if (input$map_crime_3 != "a") {
      f <- InBounds_3() %>% group_by(cuadrante) %>% summarise(count = sum(count)) %>%
        arrange(-count) %>% head(5) %>% arrange(count)
      par(mar=c(5.1, 18 ,0.1 ,0.1))
      ggplot(f, aes(x=reorder(cuadrante, count) , y=count, color=cuadrante)) +
        geom_bar(position="dodge",stat="identity") + 
        coord_flip() +  theme(legend.position="none")}
    else{}
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({

    if (input$map_style == 'cuadrant'){
      # Mapa sección uno
      
      data = InBounds() %>% group_by(cuadrante) %>% 
        summarise(count = sum(count),
                  population = max(population),
                  tasa_100_mil = (sum(count)/max(population))*10^5) %>% merge(cuadrantes) 
      data$tasa_100_mil[data$tasa_100_mil == Inf] <- 0
      color_list <- data$tasa_100_mil[data$tasa_100_mil>0 & !is.na(data$tasa_100_mil) &
                                        !is.infinite(data$tasa_100_mil)]
      if (length(color_list)>=3){
        qpal <- colorQuantile("YlOrRd", color_list, n = 3)
      } else{
        color_list <- unique(hist(plot = FALSE, data$tasa_100_mil, breaks = 5)$breaks)
        qpal <- colorBin("red", data$tasa_100_mil, bins = color_list, pretty = FALSE)
      }
      cuadrantes <-merge(cuadrantes, data, by="cuadrante")
      color <- ~qpal(cuadrantes$tasa_100_mil)
      
      labels <- sprintf(
        "Cuadrante: <strong>%s</strong>
          <br/>Población Total en Cuadrante: <strong>%s</strong>
          <br/>Número de Crímenes: <strong>%s</strong>
          <br/>Tasa del Crimen por 100 mil Habitantes: <strong>%g </strong>
          ",
        cuadrantes$cuadrante, cuadrantes$population,cuadrantes$count,
        cuadrantes$tasa_100_mil
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map", data = cuadrantes) %>%
        clearShapes() %>%
        addPolygons(
          layerId=~cuadrante,
          fillColor = color,
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) 
          # %>%
          # addLegend(pal = qpal, values = ~color_list, opacity = 0.7, title = NULL,
          #          position = "bottomright")
                
      } else {
    leafletProxy("map", data = RenderInBounds()) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius = 40, layerId=~id, color='red',
        stroke=FALSE, fillOpacity=0.4)}
  })
  
  
  observe({
    # Mapa sección dos
    if (input$map_crime != 'perrito_faldero'){
      
      data = InBounds_2() %>% group_by(cuadrante) %>% 
        summarise(count = sum(count),
                  population = max(population),
                  tasa_100_mil = (sum(count)/max(population))*10^5) %>% 
        merge(cuadrantes) 
    data$tasa_100_mil[data$tasa_100_mil == Inf] <- 0
    color_list <- data$tasa_100_mil[data$tasa_100_mil>0 & !is.na(data$tasa_100_mil) & 
                                      !is.infinite(data$tasa_100_mil)]
    
    if (length(color_list)>=3){
      qpal <- colorQuantile("YlOrRd", color_list, n = 3)
    } else{
      color_list <- unique(hist(plot = FALSE, data$tasa_100_mil, breaks = 5)$breaks)
      qpal <- colorBin("red", data$tasa_100_mil, bins = color_list, pretty = FALSE)
      
    }
    color <- ~qpal(cuadrantes$tasa_100_mil)
    cuadrantes <-merge(cuadrantes, data, by="cuadrante")
    
    labels <- sprintf(
      "Cuadrante: <strong>%s</strong>
          <br/>Número de Crímenes: <strong>%s</strong>
          ",
      cuadrantes$cuadrante, cuadrantes$count) %>% lapply(htmltools::HTML)
    
    leafletProxy("map_2", data = cuadrantes) %>%
        addPolygons(
          layerId=~cuadrante,
          fillColor = color,
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))
    }
    else{
      print('test')
    }
  })
  


  # Show a popup at the given location
  observeEvent(input$map, 
               { updateSelectInput(session, "map_cuadrante",
                                   choices = c("All",
                                               unique(as.character(data_lat_long$cuadrante))),
                                   selected = 'All') })


  
  showPopup <- function(id, lat, lng) {
    if (input$map_style == 'cuadrant'){
      updateSelectInput(session, "map_cuadrante",
                        choices = c("All", 
                                    unique(as.character(data_lat_long$cuadrante))),
                        selected = id)
    }
    else{
      selected <- data_lat_long[data_lat_long$id == id,]
      content <- as.character(tagList(
        tags$h4("Cuadrante:", selected$cuadrante),
        tags$strong(HTML(sprintf("<strong>Crime:</strong> %s,<br/> <strong>Date:</strong> %s <br/> <strong>Hour:</strong> %s",
                                 selected$crime, selected$date, selected$hour
        ))), tags$br()
      ))
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = 'id')
    }
    
  }

  
  # When map is clicked, change map
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showPopup(event$id, event$lat, event$lng)
    })
  })
  
  showPopup_2 <- function(id, lat, lng) {
    print(id)
    updateSelectInput(session, "map_cuadrante_2",
                      choices = c("All",
                                  unique(as.character(data_lat_long$cuadrante))),
                      selected = id)
  }

  observe({
    # Todo - map shape click is not working
    leafletProxy("map_2") %>% clearPopups()
    event <- input$map_shape_click

    if (is.null(event))
      return()
    isolate({
      showPopup_2(event$id, event$lat, event$lng)
    })
  })
  

  
  ## Data Filter ###########################################

  
  output$table <- DT::renderDataTable(
    DT::datatable({
    data <- data_lat_long
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    if (input$type_crime != "All") {
      data <- data[data$crime == input$type_crime,]
    }
    if (input$type_cuadrante != "All") {
      data <- data[data$cuadrante == input$type_cuadrante,]
    }
    values$data <- data
    data
  }))
  
    # TODO
    #output$downloadData <- downloadHandler(
    #content = function() {
    #  print(as.tibble(values$data))
    #  write.csv(values$data, file='CrimeCDMX.csv')
    #}
  #)

    
}

