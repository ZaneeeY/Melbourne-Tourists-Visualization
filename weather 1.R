library(shiny)


df <- read.csv("dataset/weather_dataset.csv")

ui <- fluidPage(
  selectInput("month", "Select month", unique(as.character(df$Month))),
  verbatimTextOutput("output")
)

server <- function(input, output) {
  output$output <- renderPrint({
    selected_data <- df[df$Month == input$month, ]
    
    # 打分
    precipScore <- mean(ifelse(selected_data$Precipitation..mm. < 50, 10, 5))
    tempScore <- mean(ifelse(selected_data$Mean.maximun.temperture...C. > 20 & selected_data$Mean.maximun.temperture...C. < 25, 10, 5))
    solarScore <- mean(ifelse(selected_data$Mean.daily.solar.exposure..MJ..m.m.. > 15, 10, 5))
    windScore <- mean(ifelse(selected_data$Mean.daily.wind.run..km. < 450, 10, 5))
    
    # 根据权重计算总分
    totalScore <- round((precipScore * 0.3) + (tempScore * 0.4) + (solarScore * 0.2) + (windScore * 0.1), 1)
    
    list(
      Average_Total_Score = totalScore,
      Average_Precipitation_Score = round(precipScore, 1),
      Average_Temperature_Score = round(tempScore, 1),
      Average_Sun_Exposure_Score = round(solarScore, 1),
      Average_Wind_Speed_Score = round(windScore, 1)
    )
  })
}



shinyApp(ui, server)
