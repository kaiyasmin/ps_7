#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(knitr)
library(fs)
library(lubridate)
library(formattable)
library(foreign)
library(kableExtra)
library(readr)
library(xml2)
library(stringr)

joined <- read_rds("joined.rds") 




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Relationship between intended party vote and factors related to feminism."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variable",
        label = "Choose an observation",
        choices = c("feminism", "gender", "turnout"),
        selected = "feminism"
      )), 
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) { 
  
  
  output$barPlot <- renderPlot({
    
    joined %>% 
      filter(variable == input$variable) %>%
      select(-variable) %>%
      gather(key = "party", value = "support", -category) %>%
      ggplot(aes(x = category, y = support, col = party)) + geom_point() + 
      xlab("Observation") + ylab("Percentage") + 
    labs(subtitle = "Looking at opinion on feminism, gender and turnout rate.") + 
      theme_minimal() + labs(fill = "") 
    
    
  
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
