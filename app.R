
#    http://shiny.rstudio.com/
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggplot2)
library(ggrepel)
#library(spData) # For getting spatial data
#library(sf) # For preserving spatial data
library(bslib)
library(plotly)

#import data

health <- read.csv("data_Mental/wrangled_dataset.csv")


#import data

mental_data_clean <- read_csv("data_Mental/wrangled_dataset.csv")

disorders <- c("schizophrenia", "bipolar_disorder", "eating_disorders", "anxiety_disorders",
                       "drug_use_disorders", "depression", "alcohol_use_disorders"
                       )


# Define UI for the application that draws a line graph
ui <- fluidPage(

    # Application title
    title = "Mental Health Disorder Prevalance in Different Countries",

tabPanel(
  title = "Bar Chart",
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a Country", choices = unique(mental_data_clean$entity)),
      selectInput("year", "Select a Year", choices = unique(mental_data_clean$year)),
      selectInput("disorders",
                  "Choose a disorder:",
                  disorders, selected = NULL, multiple = FALSE
      )
    ),
    mainPanel(
      plotOutput(outputId = "barplot")
    )
    )
  ),
    

  tabPanel(
  title = "Line Graph",
  # Sidebar with an input for a country
  sidebarLayout(
    sidebarPanel(
      selectInput("entity",
                  "Choose a country:",
                  unique(mental_data_clean$entity)
      ),
      selectInput("disorders",
                  "Choose a disorder:",
                  disorders, selected = NULL, multiple = FALSE
      )
    ),
    
    # Main panel to display the plot
    mainPanel(
      plotlyOutput("mental_health_plot")
    )
  )
)
)


# Define server logic
server <- function(input, output) {
# TAB 1: BAR PLOT
  
    output$barplot <- renderPlot({
      #Filter data based on selected country
      filter(entity == input$country) %>%
        # generate bins based on input$bins from ui.R
      ggplot(data = mental_data_clean, x = ~year, y = ~get(input$disorders)) +
        geom_bar(color = "blue", fill = "lightblue", alpha = 0.7) + 
        labs(x = "Disorder", y= "Percentage in Country",
             title = "Distribution of Mental Health Disorders")
    })

# TAB 2: LINE GRAPH  
      
  # Render the plot
  output$mental_health_plot <- renderPlotly({
    plotly_data <- mental_data_clean %>%
      filter(entity == input$entity) %>% # Filter data based on selected country
      plot_ly(x = ~year, y = ~get(input$disorders), type = "scatter", mode = "lines+markers") %>%
      layout(
        title = "Mental disorder Trends",
        xaxis = list(title = "Years", tickangle = -45),
        yaxis = list(title = "Disorder")
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)