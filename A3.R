library('shiny')
library('ggplot2')
library('ggiraph')

home_tab <- tabPanel(
  title='Home',
  h2('h2'),
  mainPanel(
    actionButton("btn_senic", "Go to Senic Spot"),
    actionButton("btn_restaurant", "Go to Restaurant"),
    actionButton("btn_weather", "Go to Weather"),
    uiOutput('plot_home')
  )
)

senic_spot_tab <- tabPanel(
  title='Senic Spot',
  h2('h2')
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
  id='home_page', 
  title='IMDb Top 1000 Movies',
  home_tab,
  senic_spot_tab,
  restaurant_tab,
  weather_tab
)

################
# SHINY SERVER #
################
server <- function(input, output, session) {
  observeEvent(input$btn_senic, {
    updateNavbarPage(session, "home_page", selected = "Senic Spot")
  })
  
  observeEvent(input$btn_restaurant, {
    updateNavbarPage(session, "home_page", selected = "Restaurant")
  })
  
  observeEvent(input$btn_weather, {
    updateNavbarPage(session, "home_page", selected = "Weather")
  })
  output$plot_home <- renderUI({
    
    
  })
  
  
}

#############
# Run Shiny #
#############
shinyApp(ui, server)
