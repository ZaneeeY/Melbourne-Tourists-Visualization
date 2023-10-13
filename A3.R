library('shiny')
library('ggplot2')
library('ggiraph')
library('leaflet')
library('dplyr')
library('tidyr')
library('shinyjs')

# Load the GEOM90007 Tableau in Shiny library
source('tableau-in-shiny-v1.0.R')

# Read files
tour_data <- read.csv('TourSpots.csv')

# Set up different icons and colors for public and private hospitals
tour_data$Icon <- 'camera'
tour_data$Color <- 'orange'
tour_data$Icon[tour_data$Price == 0] <- 'camera'
tour_data$Color[tour_data$Price == 0] <- 'red'

# Create the tour_tab popup
makeTourPopup <- function(row) {
  imageURL <- row$ImgURL
  words <- unlist(strsplit(row$Description, " "))  # Split the text into words
  
  # Create an HTML content string with line breaks every 5 words
  content <- paste(
    paste("<img src='", imageURL, "' alt='------Image Not Available------' width='300' height='200' />"), br(),
    strong(row$Name), br(),
    'Google:', row$Google, br(),
    'Trip Advisor:', row$TripAdvisor, br(),
    'Price:', row$Price, br(),
    'Type:', row$Type, br(),
    strong('Description:'), br()
  )
  
  # Insert line breaks every 10 words
  for (i in seq(1, length(words), 8)) {
    content <- paste(content, paste(words[i:min(i + 7, length(words))], collapse = " "), br())
  }
  
  return(HTML(content))
}

# Finalize the tour_tab popup
tour_data$Popup <- by(tour_data, seq_len(nrow(tour_data)), makeTourPopup)

##################
# USER INTERFACE #
##################

home_tab <- tabPanel(
  title='Home',
  h2('h2'),
  mainPanel(
    actionButton("btn_tour", "Go to Tour Spot"),
    actionButton("btn_restaurant", "Go to Restaurant"),
    actionButton("btn_weather", "Go to Weather"),
    uiOutput('plot_home')
  )
)

tour_tab <- tabPanel(
  title='Tour Spots',
  splitLayout(
    leafletOutput('map_tour', height = "800px"),
    tableauPublicViz(
      id='tableauViz',
      url='https://public.tableau.com/views/Book_16970122966280/Dashboard3?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link',
      height="1000px"
    ),
  )
)

restaurant_tab <- tabPanel(
  title='Restaurant',
  h2('h2')
)

restaurant_tab <- tabPanel(
  title='Restaurant',
  h2('h2')
)

weather_tab <- tabPanel(
  title='Weather',
  h2('h2')
)

ui <- navbarPage(
  header=setUpTableauInShiny(),
  id='home_page', 
  title='Melbourne Top 30 Spots',
  home_tab,
  tour_tab,
  restaurant_tab,
  weather_tab,
  tags$style(
    HTML(".leaflet-popup-content {
            max-height: 300px;
            overflow-y: auto;
          }")
  )
)

################
# SHINY SERVER #
################
server <- function(input, output, session) {
  observeEvent(input$btn_tour, {
    updateNavbarPage(session, "home_page", selected = "Tour Spots")
  })
  
  observeEvent(input$btn_restaurant, {
    updateNavbarPage(session, "home_page", selected = "Restaurant")
  })
  
  observeEvent(input$btn_weather, {
    updateNavbarPage(session, "home_page", selected = "Weather")
  })
  output$plot_home <- renderUI({
    
    
  })
  
  # Make tour map
  output$map_tour <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addAwesomeMarkers(
        data = tour_data,
        lng=~Longitude, lat=~Latitude,
        icon=~awesomeIcons(library='fa',
                           icon=Icon,
                           iconColor='#ffffff',
                           markerColor=Color),
        label=~Name,
        popup=~Popup,
        layerId=~Name) %>%
      setView(lng = 145.1291, lat = -37.9648, zoom = 9.2)
  })
  
  # React to clicks on the tour_tab marker
  observeEvent(input$map_tour_marker_click, {
    name <- input$map_tour_marker_click
    name <- name$id
    runjs(
      sprintf(
        'let viz = document.getElementById("tableauViz");
        let sheet = viz.workbook.activeSheet;
        sheet.applyFilterAsync("Name", ["%s"], FilterUpdateType.Replace);', name))
  })
  
  
}

#############
# Run Shiny #
#############
shinyApp(ui, server, options=list(launch.browser=TRUE))
