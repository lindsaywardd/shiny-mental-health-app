#
# This is SCRAP SCRIPT for our Shiny web application (table focused - Lindsay). You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(shinythemes)

#import data
# move to data folder then add to read csv here
mental_data <- read_csv("mental_health/data/wrangled_dataset.csv")

colnames(mental_data) <- c("Country", "Year", "Schizophrenia", "Bipolar Disorders",
                              "Eating Disorders", "Anxiety Disorders", "Drug Use Disorders", 
                              "Depression", "Alcohol Use Disorders")
# for TAB 3 (TABLE) widgets: 
# for selectizeInput choices for company name, pull directly from data
country_choices <- unique(mental_data$Country)
year_choices <- unique(mental_data$Year)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Mental Health Disorder Prevalance in Different Countries"),
  theme=(shinytheme("journal")),
  # Sidebar with a slider input for number of bins 

  tabPanel(
    title = "Table",
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "yr"
                       , label = "Choose one or more years:"
                       , choices = year_choices
                       , selected = c("2007", "2017")
                       , multiple = TRUE),
        
        selectizeInput(inputId = "cntry"
                       , label = "Choose one or more countries:"
                       , choices = country_choices
                       , selected = "United States"
                       , multiple = TRUE)
      ),
      mainPanel(
        DT::dataTableOutput(outputId = "table")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # TAB 3: TABLE
  data_for_table <- reactive({
    data <- filter(mental_data, Country %in% input$cntry & Year %in% input$yr)
  })
  
  
  output$table <- DT::renderDataTable({data_for_table()})    
  # Q, how to make width = 100% ?
  
}

# Run the application 
shinyApp(ui = ui, server = server)
