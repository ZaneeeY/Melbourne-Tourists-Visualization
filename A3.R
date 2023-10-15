library('shiny')
library('ggplot2')
library('ggiraph')
library('treemapify')
library('highcharter')
library('shinyjs')
library('leaflet')

source('source/tableau-in-shiny-v1.0.R')

## Restaurant
# Get data
restaurant_data <- read.csv('dataset/restaurant_dataset.csv')

names(restaurant_data)[names(restaurant_data) == "google_maps_rating"] <- "Rating on Google Maps(0~5)"
names(restaurant_data)[names(restaurant_data) == "rating"] <- "Rating on Tripadvisor(0~5)"
names(restaurant_data)[names(restaurant_data) == "price"] <- "Price on Google Maps(1～4)"
names(restaurant_data)[names(restaurant_data) == "food_rating"] <- "Food Rating on Google Maps(0～5)"

restaurant_data$tripadvisor_link <- sprintf("window.open('%s')", restaurant_data$tripadvisor_link)

# Generate variables for horizontal and vertical coordinates
x_y_vars <- setdiff(names(restaurant_data), c("name", "tripadvisor_link", "description", "cuisines", "mon", "tue", "wed", "thu", "fri", "sat", "sun"))

split_cuisines <- unlist(strsplit(restaurant_data[["cuisines"]], ", "))
unique_cuisines_list <- c("All", unique(split_cuisines))

## Tour Spots
tour_data <- read.csv('dataset/TourSpots.csv')
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

home_tab <- tabPanel(
  title='Home',
  h2('h2'),
  mainPanel(
    actionButton("btn_tour", "Go to Tour Spots"),
    actionButton("btn_restaurant", "Go to Restaurants"),
    actionButton("btn_weather", "Go to Weather"),
    uiOutput('plot_home')
  )
)

tour_tab <- tabPanel(
  title='Tour Spots',
  splitLayout(
    leafletOutput('map_tour', height = "800px"),
    tableauPublicViz(
      id='tableauViz_tour',
      url='https://public.tableau.com/views/Book_16970122966280/Dashboard3?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link',
      height="1000px"
    ),
  )
)

restaurant_tab <- tabPanel(
  title='Restaurants',
  h2('h2'),
  fluidPage(
    fluidRow(
      column(3,
             tableauPublicViz(
               id = 'tableauViz_restaurant',
               url = 'https://public.tableau.com/views/restaurant_word_cloud/Sheet1?:language=zh-CN&publish=yes&:display_count=n&:origin=viz_share_link',
               height = '300px'
             ),
      ),
      column(3,
             h3("Configuration", style = "font-weight: bold;"),
             # Allow the user to customize the horizontal and vertical coordinates according  to the information they want to get.
             # Among the values that can be used as coordinates are release year, runtime, IMDb rating, Meta score, number of votes and box office.
             # Kmeans example: https://shiny.posit.co/r/gallery/start-simple/kmeans-example/
             selectInput("xcol", "X-axis variable", x_y_vars),
             selectInput('selectedCuisine', 'Cuisines', unique_cuisines_list, selected="All", selectize=TRUE)
             
      ),
      mainPanel(
        highchartOutput('plot_restaurant')
      )
    )
  )
)

weather_tab <- tabPanel(
  title='Weather',
  h2('h2')
)

ui <- navbarPage(
  header = setUpTableauInShiny(),
  id='home_page', 
  title='Welcome to Melbourne',
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
    updateNavbarPage(session, "home_page", selected = "Restaurants")
  })
  
  observeEvent(input$btn_weather, {
    updateNavbarPage(session, "home_page", selected = "Weather")
  })
  
  output$plot_home <- renderUI({
    
  })
  
  selected_restaurant_data <- reactive({
    filtered_data <- restaurant_data[
      
    ]
    
    # If a specific cuisine is chosen, filter the rows accordingly
    #if (!is.null(input$selectedCuisine)) {
     # if(input$selectedCuisine == "All"){
      #  filtered_data <- restaurant_data
      #}else{
       # filtered_data <- filtered_data[grepl(input$selectedCuisine, filtered_data$cuisines), ]
      #}
    #}
    # If a specific cuisine is chosen in the tableau, filter the rows accordingly
    if (!is.null(input$tableauViz_restaurant_mark_selection_changed$all_splited_cuisines[1])) {
      filtered_data <- filtered_data[grepl(input$tableauViz_restaurant_mark_selection_changed$all_splited_cuisines[1], filtered_data$cuisines), ]
    }else{
      filtered_data <- restaurant_data
    }
    
    # filter relevant columns
    filtered_data[, c("name", "Rating on Google Maps(0~5)", "Rating on Tripadvisor(0~5)", "tripadvisor_link", input$xcol, "cuisines")]
  })
  
  output$plot_restaurant <- renderHighchart({
    
    
    name_var <- names(selected_restaurant_data())[1]
    google_maps_rating_var <- names(selected_restaurant_data())[2]
    rating_var <- names(selected_restaurant_data())[3]
    tripadvisor_link_var <- names(selected_restaurant_data())[4]
    x_var <- names(selected_restaurant_data())[5]
    cuisines_var <- names(selected_restaurant_data())[6]
    
    # Prepare the links in a format that's executable by JavaScript
    #selected_restaurant_data()[[tripadvisor_link_var]] <- sprintf("window.open('%s')", selected_restaurant_data()[[tripadvisor_link_var]])
    hchart(selected_restaurant_data(), "treemap", hcaes(
      name = !!sym(name_var), 
      value = !!sym(x_var), 
      color = !!sym(x_var),
      events.click = !!sym(tripadvisor_link_var)
    )) %>% 
      hc_tooltip(pointFormat = paste(
        "{point.name}<br>",
        "Tripadvisor Rating: {point.value}<br>",
        sprintf("Google Maps Rating: {point.%s}<br>", google_maps_rating_var),
        sprintf("Cuisines: {point.%s}<br>", cuisines_var),
        "Click for more details on Tripadvisor"
      )) %>%
      hc_plotOptions(
        series = list(
          point = list(
            events = list(
              click = JS("function() { eval(this.options.tripadvisor_link); }")
            )
          )
        )
      )
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
        'let viz = document.getElementById("tableauViz_tour");
        let sheet = viz.workbook.activeSheet;
        sheet.applyFilterAsync("Name", ["%s"], FilterUpdateType.Replace);', name))
  })
  
}

#############
# Run Shiny #
#############
shinyApp(ui, server, options = list(launch.browser=TRUE))
