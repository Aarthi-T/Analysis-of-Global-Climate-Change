library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(rsconnect)

natural_disasters <- read.csv("all_natural_disasters.csv")

natural_disasters_subset <- natural_disasters %>%
  select(Year,Disaster.Type,Continent) %>%
  group_by(Year,Disaster.Type,Continent) %>%
  summarise("Count_of_disasters" = n())

ui <- fluidPage(
  h1("Number of natural disasters from 1990 to Present", align = "center"),
  
  selectInput(
    inputId = "region",
    label = "Select region",
    choices = unique(natural_disasters_subset$Continent),
    selected = "Americas"
  ),
  
  plotlyOutput("plot")
)


server <- function(input, output) {
  
  selectedregion <- reactive({
    input$region
  })
  
  output$plot <- renderPlotly({
    natural_disasters_subset %>%
      filter(Continent == selectedregion()) %>%
      plot_ly(x= ~Year, y= ~Count_of_disasters, color = ~Disaster.Type,
              colors = c("#783027", "#f5c03b", "#EE9A49", "#CDAF95","#88e99a", "#6E8B3D","#104E8B","#838B83"),
              type = 'scatter', mode = 'markers',size = ~Count_of_disasters,
              text = ~paste("Disaster Type:",Disaster.Type, "<br>Year:", Year,"<br>Count:" ,Count_of_disasters),
              marker = list(sizemode = 'diameter', opacity = 0.6)) %>%
      layout(legend=list(title=list(text="Disaster Type")),
            yaxis = list(title="Number of Disasters"),
             xaxis = list(title = "Year"))
    
  })
  
}

shinyApp(ui, server)