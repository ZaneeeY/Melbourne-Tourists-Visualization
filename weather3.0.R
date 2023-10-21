library(shiny)
library(shinydashboard)

# Read the data
df <- read.csv("weather_dataset - Sheet1 (1).csv")
ui <- dashboardPage(
  dashboardHeader(title = "Weather Analysis"),
  dashboardSidebar(
    selectInput("month", "Choose Month", unique(as.character(df$Month)))
  ),
  dashboardBody(
    fluidRow(
      box(title = "Average Total Score", width = 12, status = "primary", 
          verbatimTextOutput("avgTotalScore"), solidHeader = TRUE, collapsible = TRUE)
    ),
    fluidRow(
      box(title = "Average Precipitation Score", width = 3, status = "info", 
          verbatimTextOutput("avgPrecipScore"), solidHeader = TRUE, collapsible = TRUE),
      box(title = "Average Temperature Score", width = 3, status = "warning", 
          verbatimTextOutput("avgTempScore"), solidHeader = TRUE, collapsible = TRUE),
      box(title = "Average Sun Exposure Score", width = 3, status = "success", 
          verbatimTextOutput("avgSolarScore"), solidHeader = TRUE, collapsible = TRUE),
      box(title = "Average Wind Speed Score", width = 3, status = "danger", 
          verbatimTextOutput("avgWindScore"), solidHeader = TRUE, collapsible = TRUE)
    ),
    fluidRow(
      box(title = "Advices", width = 12,
          textOutput("precipAdvice"),
          textOutput("tempAdvice"),
          textOutput("solarAdvice"),
          textOutput("windAdvice")
      )
    ),
    fluidRow(
      box(title = "Scoring Criteria", width = 12,
          tableOutput("scoringTable")
      )
    )
  )
)

server <- function(input, output) {
  
  output$avgTotalScore <- renderText({
    selected_data <- df[df$Month == input$month, ]
    
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
    selected_data <- df[df$Month == input$month, ]
    precipScore <- mean(ifelse(selected_data$Precipitation..mm. < 10, 10,
                               ifelse(selected_data$Precipitation..mm. >= 10 & selected_data$Precipitation..mm. < 50, 8,
                                      ifelse(selected_data$Precipitation..mm. >= 50 & selected_data$Precipitation..mm. < 100, 6, 4))))
    as.character(round(precipScore, 1))
  })
  
  output$avgTempScore <- renderText({
    selected_data <- df[df$Month == input$month, ]
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
    selected_data <- df[df$Month == input$month, ]
    solarScore <- mean(ifelse(selected_data$Mean.daily.solar.exposure..MJ..m.m.. >= 20, 10,
                              ifelse(selected_data$Mean.daily.solar.exposure..MJ..m.m.. >= 15 & selected_data$Mean.daily.solar.exposure..MJ..m.m.. < 20, 8,
                                     ifelse(selected_data$Mean.daily.solar.exposure..MJ..m.m.. >= 10 & selected_data$Mean.daily.solar.exposure..MJ..m.m.. < 15, 6, 4))))
    as.character(round(solarScore, 1))
  })
  
  output$avgWindScore <- renderText({
    selected_data <- df[df$Month == input$month, ]
    windScore <- mean(ifelse(selected_data$Mean.daily.wind.run..km. < 400, 10,
                             ifelse(selected_data$Mean.daily.wind.run..km. >= 400 & selected_data$Mean.daily.wind.run..km. < 450, 8,
                                    ifelse(selected_data$Mean.daily.wind.run..km. >= 450 & selected_data$Mean.daily.wind.run..km. < 500, 6, 4))))
    as.character(round(windScore, 1))
  })
  # Precipitation Advice
  output$precipAdvice <- renderText({
    precipScore <- mean(ifelse(df[df$Month == input$month, ]$Precipitation..mm. < 10, 10,
                               ifelse(df[df$Month == input$month, ]$Precipitation..mm. >= 10 & df[df$Month == input$month, ]$Precipitation..mm. < 50, 8,
                                      ifelse(df[df$Month == input$month, ]$Precipitation..mm. >= 50 & df[df$Month == input$month, ]$Precipitation..mm. < 100, 6, 4))))
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
    maxTemp <- mean(df[df$Month == input$month, ]$Mean.maximun.temperture...C.)
    minTemp <- mean(df[df$Month == input$month, ]$Mean.minimum.temperture...C.)
    
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
    solarScore <- mean(ifelse(df[df$Month == input$month, ]$Mean.daily.solar.exposure..MJ..m.m.. >= 20, 10,
                              ifelse(df[df$Month == input$month, ]$Mean.daily.solar.exposure..MJ..m.m.. >= 15 & df[df$Month == input$month, ]$Mean.daily.solar.exposure..MJ..m.m.. < 20, 8,
                                     ifelse(df[df$Month == input$month, ]$Mean.daily.solar.exposure..MJ..m.m.. >= 10 & df[df$Month == input$month, ]$Mean.daily.solar.exposure..MJ..m.m.. < 15, 6, 4))))
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
    windScore <- mean(ifelse(df[df$Month == input$month, ]$Mean.daily.wind.run..km. < 400, 10,
                             ifelse(df[df$Month == input$month, ]$Mean.daily.wind.run..km. >= 400 & df[df$Month == input$month, ]$Mean.daily.wind.run..km. < 450, 8,
                                    ifelse(df[df$Month == input$month, ]$Mean.daily.wind.run..km. >= 450 & df[df$Month == input$month, ]$Mean.daily.wind.run..km. < 500, 6, 4))))
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


shinyApp(ui, server)
