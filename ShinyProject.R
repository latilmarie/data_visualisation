library(tidyverse)
library(maps)
library(ggplot2)
library(reshape2)

data <- read.csv("Forest_and_Carbon.csv")

# data for maps------------------------------------------
data_maps <- data[, -c(1:1)]
data_maps <- data_maps[, -c(2:2)]
data_maps <- data_maps[, -c(4:36)]

data_maps$Value2020 <- data_maps$F2020
data_maps$F2020 <- NULL
data_maps$DataType <- data_maps$Indicator
data_maps$Indicator <- NULL

world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
head(world_data)

world_data["ISO3"] <- data_maps$ISO3[match(world_data$region, data_maps$Country)]

df <- rbind(data_maps)

worldMaps <- function(df, world_data, data_type){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.text = element_text(size = 14),
                       axis.title = element_text(size = 14),
                       strip.text = element_text(size = 14),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Select only the data that the user has selected to view
  plotdf <- df[ df$DataType == data_type,]
  plotdf <- plotdf[!is.na(plotdf$ISO3), ]
  
  # Add the data the user wants to see to the geographical world data
  world_data['DataType'] <- rep(data_type, nrow(world_data))
  world_data['Value2020'] <- plotdf$Value2020[match(world_data$ISO3, plotdf$ISO3)]
  
  # Create caption with the data source to show underneath the map
  capt <- paste0("Source: ", ifelse(data_type == "Carbon stocks in forests", "United Nations" , "www.climatedata.imf.org"))
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Value2020, group = group, 
                                 tooltip = sprintf("%s<br/>%s", ISO3, Value2020))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
    scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) + 
    labs(fill = data_type, color = data_type, title = NULL, x = NULL, y = NULL, caption = capt) + 
    my_theme()
  
  return(g)
}

library(shiny)
library(ggiraph)
library(markdown)

# Define the UI
ui=navbarPage(title = "Forest-Carbon App",
              tabPanel(title = "Home", 
                       "Description App or something like that"),
              tabPanel(title = "Interactive Map",
                       fluidPage(
                         # App title
                         titlePanel("Forest and Carbon Data"),
                         
                         # Sidebar layout with input and output definitions
                         sidebarLayout(
                           
                           # Sidebar panel for inputs 
                           sidebarPanel(
                             
                             # First input: Type of data
                             selectInput(inputId = "data_type",
                                         label = "Choose the type Indicator:",
                                         choices = list("Carbon stocks in forests" = "Carbon stocks in forests",
                                                        "Forest area" = "Forest area",
                                                        "Index of carbon stocks in forests" = "Index of carbon stocks in forests",
                                                        "Index of forest extent" = "Index of forest extent",
                                                        "Land area" = "Land area",
                                                        "Share of forest area" = "Share of forest area"
                                                        )),
                             
                           ),
                           mainPanel(
                             # Hide errors
                             tags$style(type = "text/css",
                                        ".shiny-output-error { visibility: hidden; }",
                                        ".shiny-output-error:before { visibility: hidden; }"),
                             
                             # Output: interactive world map
                             girafeOutput("distPlot")
                           )
                         )
                       )),
              tabPanel(title = "Interactive Map",
                       "content 3"),
              inverse = T
)

# server
server = function(input, output) {
  # Create the interactive world map
  output$distPlot <- renderGirafe({
    ggiraph(code = print(worldMaps(df, world_data, input$data_type)))
  })
  
  # Create the interactive world map
  output$distPlot <- renderGirafe({
    ggiraph(code = print(worldMaps(df, world_data, input$data_type)))
  })
}

# Finally, we can run our app by either clicking "Run App" in the top of our RStudio IDE, or by running
shinyApp(ui = ui, server = server)