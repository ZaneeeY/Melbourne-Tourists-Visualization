library('shiny')
library('ggplot2')
library('ggiraph')
library('treemapify')
library('highcharter')
library('shinyjs')
library('leaflet')
library('shinydashboard')

source('source/tableau-in-shiny-v1.0.R')

## Home
home_tab <- tabPanel(
  title='Home',
  fluidPage(
    fluidRow(
      style = "margin-left: 10%; margin-right: 10%; ",
      h1(style = "display: flex; justify-content: center;", 'Welcome to Melbourne'),
      p('Melbourne, the vibrant capital of Victoria, effortlessly merges modernity with heritage. Its diverse tapestry of attractions ranges from the lush Royal Botanic Gardens to the iconic Melbourne Cricket Ground. The city’s laneways, adorned with dynamic street art, house indie boutiques and renowned coffee shops. Melbourne\'s food scene is full of multicultural flavors, where you can taste a variety of cuisines, such as Italian, Chinese and Vietnamese. However, when packing, be prepared for its unpredictable "four seasons in one day" weather.')
    ),
    fluidRow(
      style = "margin-left: 10%; margin-right: 10%;",
      column(4,
         HTML(
           "<img src='https://images.pexels.com/photos/6998684/pexels-photo-6998684.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=1' alt='Custom Image' width='420' height='280' />
           <br>
            "
         ),
         br(),
         actionButton("btn_tour", "Explore Tour Spots")
      ),
      column(4,
             HTML(
               "<img src='https://images.pexels.com/photos/1310777/pexels-photo-1310777.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=1' alt='Custom Image' width='420' height='280' />
               <br>
                "
             ),
             br(),
             actionButton("btn_restaurant", "Explore Restaurants")
      ),
      column(4,
             HTML(
               "<img src='https://images.pexels.com/photos/1431822/pexels-photo-1431822.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=1' alt='Custom Image' width='420' height='280' />
               <br>
                "
             ),
             br(),
             actionButton("btn_weather", "Explore Weather")
      )
    )
  )
)

## Tour Spots
tour_data <- read.csv('dataset/TourSpots.csv')
# Set up different icons and colors for public and private hospitals
tour_data$Icon <- 'camera'
tour_data$Color <- 'orange'
tour_data$Icon[tour_data$Price == 0] <- 'camera'
tour_data$Color[tour_data$Price == 0] <- 'red'

tour_tab <- tabPanel(
  title='Tour Spots',
  fluidPage(
    fluidRow(
      style = "margin-left: 10%; margin-right: 10%;",
      h2(style = "display: flex; justify-content: center;", 'Tours Spots in Melbourne'),
      p('Melbourne, Australia, offers a captivating blend of country views, city walks, adventurous experiences, and the beauty of nature. Just a short drive from the bustling city center, you\'ll find the picturesque Yarra Valley, where rolling vineyards and lush landscapes create a stunning country escape. For those who prefer the urban vibe, Melbourne\'s city walks are a delight, with vibrant street art, historic architecture, and a thriving café culture that invites exploration. Adventure-seekers can embark on thrilling escapades like hot air ballooning over the Yarra Valley, or they can explore the nearby Great Ocean Road with its iconic Twelve Apostles. And when it\'s time to reconnect with nature, Melbourne doesn\'t disappoint. You can discover tranquil parks, gardens, and beaches, or venture into the nearby Dandenong Ranges for serene rainforest hikes. Melbourne truly offers something for every traveler\'s taste, making it a city of diverse and exciting experiences.'),
    ),
    fluidRow(
      style = "margin-left: 10%; margin-right: 10%;",
      column(4,
             HTML(
               "<img src='https://www.tripsavvy.com/thmb/EMZFJmSQwhPKMYjJao-PABUjbJs=/5387x3381/filters:no_upscale():max_bytes(150000):strip_icc()/IMG_7081-2-8ca655d68c9c42cbbfda9e5896d2839a.jpg' alt='Custom Image' width='370' height='260' />
               <br>
                "
              ),
             br(),
      ),
      column(4,
             HTML(
               "<img src='https://fletchers.net.au/_files/blog/whats-on-in-melbourne.jpg' alt='Custom Image' width='370' height='260' />
               <br>
                "
             ),
             br(),
      ),
      column(4,
             HTML(
               "<img src='https://cdn.concreteplayground.com/content/uploads/2016/07/Dandenong-Ranges-Flickr-Adrian-Mohedano.jpeg' alt='Custom Image' width='370' height='260' />
               <br>
                "
             ),
             br(),
      )
    ),
    fluidRow(
      style = "margin-left: 10%; margin-right: 10%;",
      h3("Tour Spots Location and Popularity")
    ),
    fluidRow(
      style = "margin-left: 10%; margin-right: 10%;",
          HTML(
          "<h4>Instruction of the usage</h4>
            <P>In the area below, you can explore the locations of various tourist attractions on the map by selecting different markers. By clicking on these markers, you can access an information popup that provides you with everything you need to know about that attraction. This includes the attraction's name, pricing, rating, type, and a brief description for your reference. Additionally, below this section, there is a foot traffic chart that appears based on the attraction you click on. This foot traffic chart can inform you about the attraction's visitor patterns over a week, showing the hourly visitor flow. This helps you plan your visit more efficiently. In this panel, there is also a price filter, which allows you to quickly filter attractions on the map between paid and free ones, making it easier for budget-conscious travelers to adapt to changes in their journey.</p>"
          ),
          radioButtons("price_filter", "Price Filter", 
                       choices = c("All" = 2, "Free" = 0, "Non-Free" = 1),
                       selected = 2),
        ),
        fluidRow(
          style = "margin-left: 10%; margin-right: 10%;",
          column(4,
              
                leafletOutput('map_tour', height = "600")
          ),
          column(8,
                tableauPublicViz(
                  id='tableauViz_tour',
                  url='https://public.tableau.com/views/Book_16970122966280/Sheet1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link'
                )
            
          )
          
        )
  )
)

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

## Restaurant
# Get data
restaurant_data <- read.csv('dataset/restaurant_dataset.csv')

names(restaurant_data)[names(restaurant_data) == "google_maps_rating"] <- "Rating on Google Maps (0~5)"
names(restaurant_data)[names(restaurant_data) == "rating"] <- "Rating on Tripadvisor (0~5)"
names(restaurant_data)[names(restaurant_data) == "price"] <- "Price on Google Maps (0~4)"
names(restaurant_data)[names(restaurant_data) == "food_rating"] <- "Food Rating on Tripadvisor (0~5)"
names(restaurant_data)[names(restaurant_data) == "service_rating"] <- "Service Rating on Tripadvisor (0~5)"
names(restaurant_data)[names(restaurant_data) == "value_rating"] <- "Value Rating on Tripadvisor (0~5)"
names(restaurant_data)[names(restaurant_data) == "atmosphere_rating"] <- "Atmosphere Rating on Tripadvisor (0~5)"
names(restaurant_data)[names(restaurant_data) == "number_comments"] <- "Number of Comments"

restaurant_data$tripadvisor_link <- sprintf("window.open('%s')", restaurant_data$tripadvisor_link)

# Generate variables for horizontal and vertical coordinates
x_vars <- setdiff(names(restaurant_data), c("name", "tripadvisor_link", "description", "cuisines"))

split_cuisines <- unlist(strsplit(restaurant_data[["cuisines"]], ", "))
unique_cuisines_list <- c(unique(split_cuisines))

cuisines_colors <- c(
  "Asian" = "#f28e2b",
  "Thai" = "#b6992d",
  "Vietnamese" = "#f1ce63",
  "Fusion" = "#e15759",
  "Italian" = "#d37295",
  "Bar" = "#59a14f",
  "Cafe" = "#b6992d",
  "Australian" = "#ffbe7d",
  "International" = "#bab0ac",
  "Pub" = "#d7b5a6",
  "Japanese" = "#fabfd2",
  "Sushi" = "#8cd17d",
  "Steakhouse" = "#ffbe7d",
  "Barbecue" = "#8cd17d",
  "Indian" = "#79706e",
  "Grill" = "#ff9d9a",
  "American" = "#a0cbe8",
  "European" = "#86bcb6",
  "Pizza" = "#9d7660",
  "Mediterranean" = "#d4a6c8",
  "Street Food" = "#59a14f",
  "Central American" = "#f1ce63",
  "Southern-Italian" = "#a0cbe8",
  "Sicilian" = "#4e79a7",
  "Chinese" = "#499894",
  "Malaysian" = "#b07aa1",
  "Spanish" = "#f28e2b"
)

restaurant_tab <- tabPanel(
  title='Restaurants',
  h2(style = "display: flex; justify-content: center;", 'Restaurants in Melbourne'),
  fluidPage(
    fluidRow(
      style = "margin-left: 10%; margin-right: 10%;",
      p("Melbourne, the cultural heart of Australia, serves as a canvas where global cuisines paint a vivid picture of its diverse heritage. Ensconced in its labyrinthine lanes and bustling streets are eateries that offer a culinary passport to the world. To guide your palate on this epicurean adventure, we've handpicked 35 standout restaurants from TripAdvisor, each echoing the nuances of distinct cuisines from around the globe. From the rustic charm of traditional dishes to avant-garde interpretations of classics, this visualization captures the essence of Melbourne's gastronomic grandeur. Embark on this curated journey and experience a taste of the city's most revered dining establishments.")
    ),
    fluidRow(
      style = "margin-left: 10%; margin-right: 10%;",
      column(5,
        style = "height: 600px; width: 800px; margin-top: 35px;",
        tableauPublicViz(
          id = 'tableauViz_restaurant',
          url = 'https://public.tableau.com/views/restaurant_word_cloud/wordcloud?:language=zh-CN&publish=yes&:display_count=n&:origin=viz_share_link',
          height = '100%'
        )
      ),
      column(4,
             h3("Word Cloud of Cuisine"),
             p("The word cloud for Melbourne cuisine gives you an idea of the type of cuisine being recommended. Each term, representing a distinct cuisine, is sized based on the number of restaurants that embrace its flavors. By clicking on a cuisine, the layered maps behind come alive, focusing solely on your selection. The bar map will highlight the chosen cuisine, the radar map will streamline its display, and the tree map will present restaurants that champion that specific types of cuisines. ")
      )
    ),
    fluidRow(
      style = "margin-left: 10%; margin-right: 10%; margin-top: 18px; ",
      h3("Google Maps Ratings vs. Tripadvisor Ratings"),
      p("Explore how Melbourne's diverse eateries fare in the eyes of discerning diners from both Google Maps and TripAdvisor. This bar chart provides a comprehensive view, breaking down average ratings by cuisine. Simply hover over each bar to glean specific rating details. If you want to know the ratings of your favorite cuisine, then you can click on the cuisine in the word cloud, after which the chart will highlight that particular dish, making it easy for you to identify it.")
    ),
    fluidRow(
      style = "margin-top: 20px; width: 100%;  display: flex; justify-content: center;",
      mainPanel(
        highchartOutput("bar_chart_ratings")
      )
    ),
    
    fluidRow(
      style = "margin-left: 10%;  margin-top: 18px; ",
      column(5,
        h3("Sub-ratings on Tripadvisor"),
        p("While overall ratings provide a snapshot, the nuances of a dining experience are captured in the sub-ratings. This radar chart intricately maps out TripAdvisor's sub-ratings—Food, Atmosphere, Service, and Value—for various cuisines. To declutter the visual and concentrate on what truly matters to you, utilize the interactive legend. By clicking on a cuisine, you can toggle its visibility on the chart. Moreover, selecting a cuisine from the word cloud narrows down the radar chart to that specific cuisine. Navigate this multifaceted tool and let it guide your next culinary adventure in Melbourne, tailored to your preferences.")
      ),
      column(7,
         style = "margin-top: 20px;",
         mainPanel(
           highchartOutput("radar_chart_restaurant")
         )
      )
    ),
    fluidRow(
      style = "margin-left: 10%;  margin-right: 10%; margin-top: 18px; ",
      h3("Comparison between Restaurants"),
      p("Dive into the intricacies of Melbourne's restaurant ratings with our versatile treemap. This visualization offers a panoramic view, dissecting ratings based on diverse variables—from overall feedback on TripAdvisor to pricing on Google Maps. Customize your exploration by selecting a specific variable from the dropdown. Curious about how a specific cuisine fares across all its eateries? Simply choose it from the word cloud, and the treemap will recalibrate to spotlight those restaurants. For a deeper dive, click on any restaurant block to be redirected to its detailed information on TripAdvisor. Use this comprehensive tool to chart your next dining experience in Melbourne, tailored to your tastes and interests.")
    ),
    fluidRow(
      style = "margin-left: 9%;  margin-top: 18px; margin-top: 20px;",
      column(3,
             selectInput("xcol", "Variable", x_vars)
      ),
      mainPanel(
        highchartOutput("plot_tree_map_restaurant")
      )
    )
  )
)

## Weather
weather_data <- read.csv("dataset/weather_dataset.csv")

weather_tab <- tabPanel(
  title='Weather',
  h2(style = "display: flex; justify-content: center;", 'Weather in Melbourne'),
  fluidPage(
    style = "margin-left: 10%; margin-right: 10%;",
    fluidRow(
     
      p('Melbourne, located in the southeast of Australia, is the capital of Victoria State. The city is famous for its changeable weather, often described as "four seasons in one day”. Melbourne\'s weather can be very volatile, so we scored the data based on historical data from 2020 to 2022 and advised visitors to prepare a variety of clothing for possible weather changes.'),
      h3("How to use"),
      p('Clicking on "Avg. Mean maximun tempture" or "Month" at the bottom will show you the weather scores, the higher the score the better for traveling.')
    ),
    fluidRow(
      tableauPublicViz(
        id='tableauViz_weather',
        url='https://public.tableau.com/views/weather_16977783909790/Sheet1?:language=zh-CN&publish=yes&:display_count=n&:origin=viz_share_link',
        height="580px"
      ),
      tags$br()
    ),
    fluidRow(
      column(3,
             tags$br(),
             box(title = "Average Precipitation Score", width = 12, status = "info", 
             verbatimTextOutput("avgPrecipScore"), solidHeader = TRUE, collapsible = TRUE),
             tags$br()
      ),
      column(3,
             tags$br(),
             box(title = "Average Temperature Score", width = 12, status = "warning", 
                 verbatimTextOutput("avgTempScore"), solidHeader = TRUE, collapsible = TRUE),
             tags$br()
      ),
      column(3,
             tags$br(),
             box(title = "Average Sun Exposure Score", width = 12, status = "success", 
                 verbatimTextOutput("avgSolarScore"), solidHeader = TRUE, collapsible = TRUE),
             tags$br()
      ),
      column(3,
             tags$br(),
             box(title = "Average Wind Speed Score", width = 12, status = "danger", 
                 verbatimTextOutput("avgWindScore"), solidHeader = TRUE, collapsible = TRUE),
             tags$br()
      )
    ),
    fluidRow(
      column(3,
             box(title = "Average Total Score", width = 12, status = "primary", 
                 verbatimTextOutput("avgTotalScore"), solidHeader = TRUE, collapsible = TRUE),
             tags$br()
             ),
      column(9,
             box(title = "Tips for visiting Melbourne", width = 36,
                 textOutput("selectedMonth"),
                 textOutput("precipAdvice"),
                 textOutput("tempAdvice"),
                 textOutput("solarAdvice"),
                 textOutput("windAdvice")
             ),
             tags$br()
      ),
    ),
    fluidRow(
      box(title = "Scoring Criteria", width = 12,
          tableOutput("scoringTable")
      )
    )
    
  )
)

## More Information
info_tab <- tabPanel(
  title='More Information',
  fluidPage(
    fluidRow(
      style = "margin-left: 10%; margin-right: 10%; ",
      h2(style = "display: flex; justify-content: center;", 'More Information'),
      h3('Source of Data'),
      p(a(href="https://www.tripadvisor.com.au/", target="_blank", "Tripadvisor")),
      p(a(href="https://www.google.com/maps/", target="_blank", "Google Maps")),
      p(a(href="http://www.bom.gov.au/?ref=logo", target="_blank", "Australian Government: Bureau of Meteorology")),
      h3('Source of Images'),
      h4('Home Page:'),
      p(a(href="https://images.pexels.com/photos/6998684/pexels-photo-6998684.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=1", target="_blank", "Tour Spot Photo")),
      p(a(href="https://images.pexels.com/photos/1310777/pexels-photo-1310777.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=1", target="_blank", "Food Photo")),
      p(a(href="https://images.pexels.com/photos/1431822/pexels-photo-1431822.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=1", target="_blank", "Clouds Photo")),
      h4('Tour Spots Page:'),
      p(a(href="https://www.tripsavvy.com/thmb/EMZFJmSQwhPKMYjJao-PABUjbJs=/5387x3381/filters:no_upscale():max_bytes(150000):strip_icc()/IMG_7081-2-8ca655d68c9c42cbbfda9e5896d2839a.jpg", target="_blank", "Attraction Photo 1")),
      p(a(href="https://fletchers.net.au/_files/blog/whats-on-in-melbourne.jpg", target="_blank", "Attraction Photo 2")),
      p(a(href="https://cdn.concreteplayground.com/content/uploads/2016/07/Dandenong-Ranges-Flickr-Adrian-Mohedano.jpeg", target="_blank", "Attraction Photo 3")),
      h3('Links to Charts of Tableau'),
      p(a(href="https://public.tableau.com/views/Book_16970122966280/Sheet1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link", target="_blank", "Attraction Footfall Bar Chart")),
      p(a(href="https://public.tableau.com/views/restaurant_word_cloud/Sheet1?:language=zh-CN&publish=yes&:display_count=n&:origin=viz_share_link", target="_blank", "Cuisine Word Cloud")),
      p(a(href="https://public.tableau.com/views/weather_16977783909790/Sheet1?:language=zh-CN&publish=yes&:display_count=n&:origin=viz_share_link", target="_blank", "Weather Dual Axis Chart"))
    )
    
  )
  
)

ui <- dashboardPage(
  dashboardHeader(title = "Melbourne Tour"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Home", tabName = "home"),
      menuItem("Tour Spots", tabName = "tour"),
      menuItem("Restaurants", tabName = "restaurant"),
      menuItem("Weather", tabName = "weather"),
      menuItem("More Information", tabName = "info")
    )
  ),
  dashboardBody(
    tags$head(
      setUpTableauInShiny()
    ),
    tabItems(
      tabItem(tabName = "home", home_tab),
      tabItem(tabName = "tour", tour_tab),
      tabItem(tabName = "restaurant", restaurant_tab),
      tabItem(tabName = "weather", weather_tab),
      tabItem(tabName = "info", info_tab)
    )
  )
)



################
# SHINY SERVER #
################
server <- function(input, output, session) {
  
  observeEvent(input$btn_tour, {
    updateTabItems(session, "sidebarmenu", selected = "tour")
  })
  
  observeEvent(input$btn_restaurant, {
    updateTabItems(session, "sidebarmenu", selected = "restaurant")
  })
  
  observeEvent(input$btn_weather, {
    updateTabItems(session, "sidebarmenu", selected = "weather")
  })
  
  
  ## Tour Spots
  # Make tour map
  output$map_tour <- renderLeaflet({
    validate(
      need(input$price_filter, "Please select a choice")
    )
    
    if (2 %in% input$price_filter) {
      filtered_tour_data <- tour_data
    } else if (0 %in% input$price_filter) {
      filtered_tour_data <- tour_data[tour_data$Price == 0, ]
    } else if (1 %in% input$price_filter) {
      filtered_tour_data <- tour_data[tour_data$Price > 0, ]
    }
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addAwesomeMarkers(
        data = filtered_tour_data,
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
  
  ## Restaurant
  selected_restaurant_data <- reactive({
    filtered_data <- restaurant_data[]
    # Filter the rows if a specific cuisine is selected in the tableau.
    if (!is.null(input$tableauViz_restaurant_mark_selection_changed$all_splited_cuisines[1])) {
      filtered_data <- filtered_data[grepl(input$tableauViz_restaurant_mark_selection_changed$all_splited_cuisines[1], filtered_data$cuisines), ]
    }else{
      filtered_data <- restaurant_data
    }
    
    # filter relevant columns
    filtered_data[, c("name", "tripadvisor_link", input$xcol, "cuisines", "Food Rating on Tripadvisor (0~5)", "Service Rating on Tripadvisor (0~5)", "Value Rating on Tripadvisor (0~5)", "Atmosphere Rating on Tripadvisor (0~5)")]
  })
  
  output$bar_chart_ratings <- renderHighchart({
    # Create a blank data frame at startup to store results.
    avg_ratings <- data.frame()
    
    google_maps_rating_var <- "Rating on Google Maps (0~5)"
    rating_var <- "Rating on Tripadvisor (0~5)"
    
    # Calculate average ratings for every unique cuisine
    for (cuisine in unique_cuisines_list) {
      filtered_data <- restaurant_data[grepl(paste0("\\b", cuisine, "\\b"), restaurant_data$cuisines), ]
      avg_google <- round(mean(filtered_data[[google_maps_rating_var]], na.rm = TRUE), 1)
      avg_tripadvisor <- round(mean(filtered_data[[rating_var]], na.rm = TRUE), 1)
      
      avg_ratings <- rbind(avg_ratings, 
                           data.frame(cuisines = cuisine, 
                                      GoogleMapsRating = avg_google, 
                                      TripAdvisorRating = avg_tripadvisor))
    }
    
    specific_cuisine <- input$tableauViz_restaurant_mark_selection_changed$all_splited_cuisines[1]

    # If specific_cuisine is not NULL, then set up the highlighted series
    if (!is.null(specific_cuisine)) {
        highlight_mask <- avg_ratings$cuisines %in% specific_cuisine
        
        highlighted_google_rating <- ifelse(highlight_mask, avg_ratings$GoogleMapsRating, NA)
        highlighted_trip_rating <- ifelse(highlight_mask, avg_ratings$TripAdvisorRating, NA)
    } else {
        highlight_mask <- avg_ratings$cuisines %in% " "
        # If specific_cuisine is NULL, then no bars should be highlighted
        highlighted_google_rating <- rep(NA, length(avg_ratings$GoogleMapsRating))
        highlighted_trip_rating <- rep(NA, length(avg_ratings$TripAdvisorRating))
    }
    
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Average Ratings by Cuisine") %>%
      hc_xAxis(categories = avg_ratings$cuisines) %>%
      hc_yAxis(title = list(text = "Average Rating")) %>%
      hc_add_series(name = "Google Maps Rating", 
                    data = ifelse(!highlight_mask, avg_ratings$GoogleMapsRating, NA),
                    color = "#7fc97f") %>%
      hc_add_series(name = "Tripadvisor Rating", 
                    data = ifelse(!highlight_mask, avg_ratings$TripAdvisorRating, NA),
                    color = "#beaed4") %>%
      # Series for the highlighted bars
      hc_add_series(name = "Google Maps Rating Selected", 
                  data = highlighted_google_rating,
                  color = "#fdc086",
                  borderColor = "#386cb0",
                  borderWidth = 2) %>%
      hc_add_series(name = "Tripadvisor Rating Selected", 
                    data = highlighted_trip_rating,
                    color = "#ffff99",
                    borderColor = "#386cb0",
                    borderWidth = 2) %>%
      
      hc_plotOptions(column = list(
        dataLabels = list(enabled = TRUE)
      ))
    
    
    return(hc)

  })
  
  output$radar_chart_restaurant <- renderHighchart({
    food_rating_var <- names(selected_restaurant_data())[5]
    service_rating_var <- names(selected_restaurant_data())[6]
    value_rating_var <- names(selected_restaurant_data())[7]
    atmosphere_rating_var <- names(selected_restaurant_data())[8]
    
    # Preparing data
    compute_cuisine_means <- function(cuisine) {
      filtered_data <- subset(selected_restaurant_data(), grepl(cuisine, cuisines))
      
      c(
        round(mean(filtered_data[[food_rating_var]], na.rm = TRUE), 1),
        round(mean(filtered_data[[service_rating_var]], na.rm = TRUE), 1),
        round(mean(filtered_data[[value_rating_var]], na.rm = TRUE), 1),
        round(mean(filtered_data[[atmosphere_rating_var]], na.rm = TRUE), 1)
      )
    }
    
    if (!is.null(input$tableauViz_restaurant_mark_selection_changed$all_splited_cuisines[1])) {
      radar_data <- lapply(input$tableauViz_restaurant_mark_selection_changed$all_splited_cuisines[1], compute_cuisine_means)
    } else {
      radar_data <- lapply(unique_cuisines_list, compute_cuisine_means)
    }
    
    radar_data <- do.call(rbind, radar_data)
    colnames(radar_data) <- c(food_rating_var, service_rating_var, value_rating_var, atmosphere_rating_var)
    
    # Radar Chart
    hc <- highchart() %>%
      hc_chart(polar = TRUE, type = "line") %>%
      hc_title(text = "Sub-ratings on Tripadvisor for Different Cuisines") %>%
      hc_xAxis(categories = colnames(radar_data), tickmarkPlacement = 'on', lineWidth = 0) %>%
      hc_yAxis(gridLineInterpolation = 'polygon', lineWidth = 0, min = 0, max = 5) %>%
      hc_legend(y = 20)  %>%
      hc_plotOptions(series = list(marker = list(symbol = 'circle'))) %>%
      hc_add_series_list(
        lapply(1:nrow(radar_data), function(i) {
          series_name <- if (!is.null(input$tableauViz_restaurant_mark_selection_changed$all_splited_cuisines[1])) {
            input$tableauViz_restaurant_mark_selection_changed$all_splited_cuisines[1]
          } else {
            unique_cuisines_list[i]
          }
          
          list(
            name = series_name,
            data = as.numeric(radar_data[i, ]),
            pointPlacement = 'on',
            color = cuisines_colors[[series_name]]
          )
        })
      )
    
    return(hc)
  })

  
  output$plot_tree_map_restaurant <- renderHighchart({
    name_var <- names(selected_restaurant_data())[1]

    tripadvisor_link_var <- names(selected_restaurant_data())[2]
    x_var <- names(selected_restaurant_data())[3]
    cuisines_var <- names(selected_restaurant_data())[4]
    
    hchart(selected_restaurant_data(), "treemap", hcaes(
      name = !!sym(name_var), 
      value = !!sym(x_var), 
      color = !!sym(x_var),
      events.click = !!sym(tripadvisor_link_var)
    )) %>% 
      hc_tooltip(pointFormat = paste(
        "{point.name}<br>",
        x_var, ": {point.value}<br>",
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
  
  ## Weather
  output$avgTotalScore <- renderText({
    validate(
      need(input$tableauViz_weather_mark_selection_changed$MONTH[1], "Please select a month")
    )
    month_name = month.name[input$tableauViz_weather_mark_selection_changed$MONTH[1]]
    selected_data <- weather_data[weather_data$Month == month_name, ]
    
    # Precipitation Score
    precipScore <- mean(ifelse(selected_data$Precipitation..mm. < 10, 10,
                               ifelse(selected_data$Precipitation..mm. >= 10 & selected_data$Precipitation..mm. < 50, 8,
                                      ifelse(selected_data$Precipitation..mm. >= 50 & selected_data$Precipitation..mm. < 100, 6, 4))))
    
    # Temperature Score considering both Max and Min temperature
    maxTempScore <- mean(ifelse(selected_data$Mean.maximun.temperture...C. > 20 & selected_data$Mean.maximun.temperture...C. <= 25, 10,
                                ifelse(selected_data$Mean.maximun.temperture...C. > 25 & selected_data$Mean.maximun.temperture...C. <= 30, 8,
                                       ifelse(selected_data$Mean.maximun.temperture...C. > 30 & selected_data$Mean.maximun.temperture...C. <= 35, 6, 4))))
    
    minTempScore <- mean(ifelse(selected_data$Mean.minimum.temperture...C. > 15 & selected_data$Mean.minimum.temperture...C. <= 20, 10,
                                ifelse(selected_data$Mean.minimum.temperture...C. > 10 & selected_data$Mean.minimum.temperture...C. <= 15, 8,
                                       ifelse(selected_data$Mean.minimum.temperture...C. > 5 & selected_data$Mean.minimum.temperture...C. <= 10, 6, 4))))
    
    tempScore <- (maxTempScore + minTempScore) / 2
    
    # Sun Exposure Score
    solarScore <- mean(ifelse(selected_data$Mean.daily.solar.exposure..MJ..m.m.. >= 20, 10,
                              ifelse(selected_data$Mean.daily.solar.exposure..MJ..m.m.. >= 15 & selected_data$Mean.daily.solar.exposure..MJ..m.m.. < 20, 8,
                                     ifelse(selected_data$Mean.daily.solar.exposure..MJ..m.m.. >= 10 & selected_data$Mean.daily.solar.exposure..MJ..m.m.. < 15, 6, 4))))
    
    # Wind Score
    windScore <- mean(ifelse(selected_data$Mean.daily.wind.run..km. < 400, 10,
                             ifelse(selected_data$Mean.daily.wind.run..km. >= 400 & selected_data$Mean.daily.wind.run..km. < 450, 8,
                                    ifelse(selected_data$Mean.daily.wind.run..km. >= 450 & selected_data$Mean.daily.wind.run..km. < 500, 6, 4))))
    
    # Compute total score with weights
    totalScore <- round((precipScore * 0.3) + (tempScore * 0.4) + (solarScore * 0.2) + (windScore * 0.1), 1)
    as.character(totalScore)
  })
  
  output$avgPrecipScore <- renderText({
    validate(
      need(input$tableauViz_weather_mark_selection_changed$MONTH[1], "Please select a month")
    )
    month_name = month.name[input$tableauViz_weather_mark_selection_changed$MONTH[1]]
    
    selected_data <- weather_data[weather_data$Month == month_name, ]
    precipScore <- mean(ifelse(selected_data$Precipitation..mm. < 10, 10,
                               ifelse(selected_data$Precipitation..mm. >= 10 & selected_data$Precipitation..mm. < 50, 8,
                                      ifelse(selected_data$Precipitation..mm. >= 50 & selected_data$Precipitation..mm. < 100, 6, 4))))
    as.character(round(precipScore, 1))
  })
  
  output$avgTempScore <- renderText({
    validate(
      need(input$tableauViz_weather_mark_selection_changed$MONTH[1], "Please select a month")
    )
    month_name = month.name[input$tableauViz_weather_mark_selection_changed$MONTH[1]]
    
    selected_data <- weather_data[weather_data$Month == month_name, ]
    maxTempScore <- mean(ifelse(selected_data$Mean.maximun.temperture...C. > 20 & selected_data$Mean.maximun.temperture...C. <= 25, 10,
                                ifelse(selected_data$Mean.maximun.temperture...C. > 25 & selected_data$Mean.maximun.temperture...C. <= 30, 8,
                                       ifelse(selected_data$Mean.maximun.temperture...C. > 30 & selected_data$Mean.maximun.temperture...C. <= 35, 6, 4))))
    minTempScore <- mean(ifelse(selected_data$Mean.minimum.temperture...C. > 15 & selected_data$Mean.minimum.temperture...C. <= 20, 10,
                                ifelse(selected_data$Mean.minimum.temperture...C. > 10 & selected_data$Mean.minimum.temperture...C. <= 15, 8,
                                       ifelse(selected_data$Mean.minimum.temperture...C. > 5 & selected_data$Mean.minimum.temperture...C. <= 10, 6, 4))))
    tempScore <- (maxTempScore + minTempScore) / 2
    as.character(round(tempScore, 1))
  })
  
  output$avgSolarScore <- renderText({
    validate(
      need(input$tableauViz_weather_mark_selection_changed$MONTH[1], "Please select a month")
    )
    month_name = month.name[input$tableauViz_weather_mark_selection_changed$MONTH[1]]
    
    selected_data <- weather_data[weather_data$Month == month_name, ]
    solarScore <- mean(ifelse(selected_data$Mean.daily.solar.exposure..MJ..m.m.. >= 20, 10,
                              ifelse(selected_data$Mean.daily.solar.exposure..MJ..m.m.. >= 15 & selected_data$Mean.daily.solar.exposure..MJ..m.m.. < 20, 8,
                                     ifelse(selected_data$Mean.daily.solar.exposure..MJ..m.m.. >= 10 & selected_data$Mean.daily.solar.exposure..MJ..m.m.. < 15, 6, 4))))
    as.character(round(solarScore, 1))
  })
  
  output$avgWindScore <- renderText({
    validate(
      need(input$tableauViz_weather_mark_selection_changed$MONTH[1], "Please select a month")
    )
    month_name = month.name[input$tableauViz_weather_mark_selection_changed$MONTH[1]]
    
    selected_data <- weather_data[weather_data$Month == month_name, ]
    windScore <- mean(ifelse(selected_data$Mean.daily.wind.run..km. < 400, 10,
                             ifelse(selected_data$Mean.daily.wind.run..km. >= 400 & selected_data$Mean.daily.wind.run..km. < 450, 8,
                                    ifelse(selected_data$Mean.daily.wind.run..km. >= 450 & selected_data$Mean.daily.wind.run..km. < 500, 6, 4))))
    as.character(round(windScore, 1))
  })
  
  # Display the selected month
  output$selectedMonth  <- renderText({
    # If no month is selected, an error message is displayed
    validate(
      need(input$tableauViz_weather_mark_selection_changed$MONTH[1], "Please select a month")
    )
    
    name_month <- month.name[input$tableauViz_weather_mark_selection_changed$MONTH[1]]
    
    return(sprintf("You've chosen %s, check out these tips:", name_month))
  })
  
  # Precipitation Advice
  output$precipAdvice <- renderText({
    # If no month is selected, an error message is displayed
    validate(
      need(input$tableauViz_weather_mark_selection_changed$MONTH[1], "")
    )
    
    name_month <- month.name[input$tableauViz_weather_mark_selection_changed$MONTH[1]]
    
    precipScore <- mean(ifelse(weather_data[weather_data$Month == name_month, ]$Precipitation..mm. < 10, 10,
                               ifelse(weather_data[weather_data$Month == name_month, ]$Precipitation..mm. >= 10 & weather_data[weather_data$Month == name_month, ]$Precipitation..mm. < 50, 8,
                                      ifelse(weather_data[weather_data$Month == name_month, ]$Precipitation..mm. >= 50 & weather_data[weather_data$Month == name_month, ]$Precipitation..mm. < 100, 6, 4))))
    if (precipScore > 8) {
      return("🌈 Low precipitation, good for traveling!")
    } else if (precipScore >= 7 && precipScore <= 8) {
      return("☁️ Light rain, carry an umbrella!")
    } else {
      return("🌧️ Heavy rain, bring rain gear!")
    }
  })
  
  # Temperature Advice
  output$tempAdvice <- renderText({
    # If no month is selected, an error message is displayed
    validate(
      need(input$tableauViz_weather_mark_selection_changed$MONTH[1], "")
    )
    
    name_month <- month.name[input$tableauViz_weather_mark_selection_changed$MONTH[1]]
    
    maxTemp <- mean(weather_data[weather_data$Month == name_month, ]$Mean.maximun.temperture...C.)
    minTemp <- mean(weather_data[weather_data$Month == name_month, ]$Mean.minimum.temperture...C.)
    
    if (maxTemp > 30) {
      return("🔥 Hot, dress cool!")
    } else if (maxTemp > 25 && maxTemp <= 30) {
      return("🍂 Mildly warm, dress appropriately!")
    } else if (minTemp < 10) {
      return("❄️ Cold, dress warmly!")
    } else if (minTemp >= 10 && minTemp <= 15) {
      return("🌾 Mildly cold, dress appropriately!")
    } else {
      return("🌡️ Comfortable temperature!")
    }
  })
  
  
  # Sun Exposure Advice
  output$solarAdvice <- renderText({
    # If no month is selected, an error message is displayed
    validate(
      need(input$tableauViz_weather_mark_selection_changed$MONTH[1], "")
    )
    
    name_month <- month.name[input$tableauViz_weather_mark_selection_changed$MONTH[1]]
    
    solarScore <- mean(ifelse(weather_data[weather_data$Month == name_month, ]$Mean.daily.solar.exposure..MJ..m.m.. >= 20, 10,
                              ifelse(weather_data[weather_data$Month == name_month, ]$Mean.daily.solar.exposure..MJ..m.m.. >= 15 & weather_data[weather_data$Month == name_month, ]$Mean.daily.solar.exposure..MJ..m.m.. < 20, 8,
                                     ifelse(weather_data[weather_data$Month == name_month, ]$Mean.daily.solar.exposure..MJ..m.m.. >= 10 & weather_data[weather_data$Month == name_month, ]$Mean.daily.solar.exposure..MJ..m.m.. < 15, 6, 4))))
    if (solarScore > 8) {
      return("☀️ High sun exposure, bring sunglasses!")
    } else if (solarScore >= 7 && solarScore <= 8) {
      return("⛅ Moderate sun, perfect for outdoor activities!")
    } else {
      return("☁️ Low sun exposure, might be cloudy!")
    }
  })
  
  # Wind Advice
  output$windAdvice <- renderText({
    # If no month is selected, an error message is displayed
    validate(
      need(input$tableauViz_weather_mark_selection_changed$MONTH[1], "")
    )
    name_month <- month.name[input$tableauViz_weather_mark_selection_changed$MONTH[1]]
    
    windScore <- mean(ifelse(weather_data[weather_data$Month == name_month, ]$Mean.daily.wind.run..km. < 400, 10,
                             ifelse(weather_data[weather_data$Month == name_month, ]$Mean.daily.wind.run..km. >= 400 & weather_data[weather_data$Month == name_month, ]$Mean.daily.wind.run..km. < 450, 8,
                                    ifelse(weather_data[weather_data$Month == name_month, ]$Mean.daily.wind.run..km. >= 450 & weather_data[weather_data$Month == name_month, ]$Mean.daily.wind.run..km. < 500, 6, 4))))
    if (windScore > 8) {
      return("🍃 Light breeze!")
    } else if (windScore >= 7 && windScore <= 8) {
      return("🌬️ Moderate wind, might be windy!")
    } else {
      return("💨 Strong wind, be cautious!")
    }
  })
  output$scoringTable <- renderTable({
    data.frame(
      Parameter = c("Precipitation", "Temperature (Max)", "Temperature (Min)", "Sun Exposure", "Wind Speed"),
      `10 Points` = c("<10mm", ">20 & <=25°C", ">15 & <=20°C", ">=20 MJ/m^2", "<400 km"),
      `8 Points` = c(">=10 & <50mm", ">25 & <=30°C", ">10 & <=15°C", ">=15 & <20 MJ/m^2", ">=400 & <450 km"),
      `6 Points` = c(">=50 & <100mm", ">30 & <=35°C", ">5 & <=10°C", ">=10 & <15 MJ/m^2", ">=450 & <500 km"),
      `4 Points` = c(">=100mm", "Others", "Others", "<10 MJ/m^2", ">=500 km")
    )
  })
  
  
}

#############
# Run Shiny #
#############
shinyApp(ui, server, options = list(launch.browser=TRUE))
