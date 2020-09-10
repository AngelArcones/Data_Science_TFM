
library(shiny)
library(leaflet)
library(rgdal)
library(raster)



list_maps <- list.files("./data", full.names = T)


clean_npp <- raster(grep("NPP", list_maps, value = T))
RF_prediction <- raster(grep("RF_35K", list_maps, value = T))
pred_245_2040 <- raster(grep("245_2021", list_maps, value = T))
pred_285_2040 <- raster(grep("585_2021", list_maps, value = T))
pred_245_2080 <- raster(grep("245_2061", list_maps, value = T))
pred_285_2080 <- raster(grep("585_2061", list_maps, value = T))


raster_vals <- values(clean_npp)
my_pal <- colorNumeric(palette = "YlGn", values(clean_npp),
                       na.color = "transparent")


choice_list <- list("NPP data (present)" = 0,
                    "Model prediction (present)" = 1,
                    "Prediction 2021-2040: stable emissions" = 2,
                    "Prediction 2021-2040: increasing emissions" = 3,
                    "Prediction 2061-2080: stable emissions" = 4,
                    "Prediction 2061-2080: increasing emissions" = 5)

#my_raster <- RF_prediction
# Define the visual aspect
ui <- fluidPage(

    # Application title
    titlePanel("Prediction of potential net primary productivity (NPP)"),

    selectInput("select_top", label = ("Select map"), 
                choices = choice_list, 
                selected = 0),
    leafletOutput("mymap_top"),
    hr(),
    selectInput("select_bottom", label = ("Select map"), 
                choices = choice_list, 
                selected = 1),
    
    
    leafletOutput("mymap_bottom")
    

)

# Define server logic
server <- function(input, output) {

    raster_selection_top <- reactive({
      if(input$select_top == 0){
        my_raster <- leaflet() %>%
          addProviderTiles(providers$OpenStreetMap) %>%
          addRasterImage(clean_npp, colors = "YlGn",opacity = 0.8) %>%
          addLegend(pal=my_pal, values = raster_vals,
                    title = "NPP\n(gr C / m^2 / year)")

      }else if(input$select_top == 1){
            #my_raster <- RF_prediction
            my_raster <- leaflet() %>%
                addProviderTiles(providers$OpenStreetMap) %>%
                addRasterImage(RF_prediction, colors = "YlGn", opacity = 0.8) %>%
              addLegend(pal=my_pal, values = raster_vals,
                        title = "NPP\n(gr C / m^2 / year)")

        }else if(input$select_top == 2){
            my_raster <- leaflet() %>%
                addProviderTiles(providers$OpenStreetMap) %>%
                addRasterImage(pred_245_2040, colors = "YlGn", opacity = 0.8) %>%
              addLegend(pal=my_pal, values = raster_vals,
                        title = "NPP\n(gr C / m^2 / year)")

            #my_raster <- pred_245_2040
        }else if(input$select_top == 3){
          my_raster <- leaflet() %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addRasterImage(pred_285_2040, colors = "YlGn",opacity = 0.8) %>%
            addLegend(pal=my_pal, values = raster_vals,
                      title = "NPP\n(gr C / m^2 / year)")

        }else if(input$select_top == 4){
          my_raster <- leaflet() %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addRasterImage(pred_245_2080, colors = "YlGn",opacity = 0.8) %>%
            addLegend(pal=my_pal, values = raster_vals,
                      title = "NPP\n(gr C / m^2 / year)")

        }else if(input$select_top == 5){
          my_raster <- leaflet() %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addRasterImage(pred_285_2080, colors = "YlGn",opacity = 0.8) %>%
            addLegend(pal=my_pal, values = raster_vals,
                      title = "NPP\n(gr C / m^2 / year)")

        }
        return(my_raster)
    })
    
    raster_selection_bottom <- reactive({
      if(input$select_bottom == 0){
        my_raster <- leaflet() %>%
          addProviderTiles(providers$OpenStreetMap) %>%
          addRasterImage(clean_npp, colors = "YlGn",opacity = 0.8) %>%
          addLegend(pal=my_pal, values = raster_vals,
                    title = "NPP\n(gr C / m^2 / year)")
        
      }else if(input$select_bottom == 1){
        #my_raster <- RF_prediction
        my_raster <- leaflet() %>%
          addProviderTiles(providers$OpenStreetMap) %>%
          addRasterImage(RF_prediction, colors = "YlGn", opacity = 0.8) %>%
          addLegend(pal=my_pal, values = raster_vals,
                    title = "NPP\n(gr C / m^2 / year)")
        
      }else if(input$select_bottom == 2){
        my_raster <- leaflet() %>%
          addProviderTiles(providers$OpenStreetMap) %>%
          addRasterImage(pred_245_2040, colors = "YlGn", opacity = 0.8) %>%
          addLegend(pal=my_pal, values = raster_vals,
                    title = "NPP\n(gr C / m^2 / year)")
        
        #my_raster <- pred_245_2040
      }else if(input$select_bottom == 3){
        my_raster <- leaflet() %>%
          addProviderTiles(providers$OpenStreetMap) %>%
          addRasterImage(pred_285_2040, colors = "YlGn",opacity = 0.8) %>%
          addLegend(pal=my_pal, values = raster_vals,
                    title = "NPP\n(gr C / m^2 / year)")
        
      }else if(input$select_bottom == 4){
        my_raster <- leaflet() %>%
          addProviderTiles(providers$OpenStreetMap) %>%
          addRasterImage(pred_245_2080, colors = "YlGn",opacity = 0.8) %>%
          addLegend(pal=my_pal, values = raster_vals,
                    title = "NPP\n(gr C / m^2 / year)")
        
      }else if(input$select_bottom == 5){
        my_raster <- leaflet() %>%
          addProviderTiles(providers$OpenStreetMap) %>%
          addRasterImage(pred_285_2080, colors = "YlGn",opacity = 0.8) %>%
          addLegend(pal=my_pal, values = raster_vals,
                    title = "NPP\n(gr C / m^2 / year)")
        
      }
      return(my_raster)
    })
  

    
    output$mymap_top <- renderLeaflet({raster_selection_top()})
    output$mymap_bottom <- renderLeaflet({raster_selection_bottom()})
    
    
        
            
}

# Run the application 
shinyApp(ui = ui, server = server)
