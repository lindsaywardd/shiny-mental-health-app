# Parts:
# Sierra - Bar Chart
# Gretta - Line Graph
# Lindsay - Table

library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggplot2)
library(ggrepel)
library(spData) # For getting spatial data
library(sf)
library(plotly)# For preserving spatial data

#import data

mental_data <- read.csv("data/wrangled_dataset.csv")

colnames(mental_data) <- c("Country", "Year", "Schizophrenia", "Bipolar Disorders",
                           "Eating Disorders", "Anxiety Disorders", "Drug Use Disorders", 
                           "Depression", "Alcohol Use Disorders")

disorders <- c("Schizophrenia", "Bipolar Disorders", "Eating Disorders", "Anxiety Disorders",
               "Drug Use Disorders", "Depression", "Alcohol Use Disorders"
)

health_new <- mental_data %>% 
  pivot_longer(
    cols = !c(Country, Year),
    names_to = "disorders", 
    values_to = "percentages")

# for TAB 3 (TABLE) widgets: 
# for selectizeInput choices for company name, pull directly from data
country_choices <- unique(mental_data$Country)
year_choices <- unique(mental_data$Year)

# Define UI
ui <- navbarPage(


title="Mental Health in Countries Around the World",
theme=(shinytheme("journal")),
#bar chart tab - Sierra
tabPanel(
  title = "Bar Chart",
  sidebarLayout(
    sidebarPanel(
      selectizeInput("country", "Select Countries to Plot", choices = country_choices,
                     multiple = TRUE),
      selectInput("year", "Select a Year", choices = unique(health_new$Year)),
      selectizeInput("disorder", "Select Disorders to Plot", choices = disorders,
                     multiple = TRUE),
    ),
    mainPanel(
      plotOutput(outputId = "barplot")
    )
  )
),


#line graph tab - Gretta

tabPanel(
  title = "Line Graph",
  # Sidebar with an input for a country
  sidebarLayout(
    sidebarPanel(
      selectizeInput("entity",
                  "Choose a country:",
                  choices = country_choices,
                  multiple = TRUE,
                  selected = "Afghanistan"
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
),

#table tab - Lindsay

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
                     , multiple = TRUE),
      
      checkboxGroupInput(inputId = "show_vars", "Disorders to show:",
                         names(mental_data[1:9]), selected = names(mental_data[1:9]))
    ),
    mainPanel(
      DT::dataTableOutput(outputId = "table", width="100%", height="auto")
    )
  )
)
)

# Define server logic
server <- function(input, output) {

  # TAB 1: bar chart
  output$barplot <- renderPlot({
    
    filtered_data <- health_new %>%
      filter(Country %in% input$country, Year == input$year, disorders %in% input$disorder)
    
    # generate bins based on input$bins from ui.R
    ggplot(data = filtered_data, aes(x = disorders, y = percentages, fill = Country)) +
      geom_col(position = "dodge", alpha = 0.7) +
      geom_text(aes(label = sprintf("%.3f", percentages)), vjust = -0.5,
                color = "black", size = 3, position = position_dodge(width = 1)) +
      #coord_flip() +
      labs(x = "Disorder", y= "Percentage in Country",
           title = "Distribution of Mental Health Disorders")
  })
    
    #TAB 2: line graph
    # Render the plot
    output$mental_health_plot <- renderPlotly({
      plotly_data <- mental_data %>%
        filter(Country == input$entity) %>% # Filter data based on selected country
        plot_ly(x = ~Year, y = ~get(input$disorders), type = "scatter", mode = "lines+markers", color = ~Country) %>%
        layout(
          title = "Mental Health Disorder Trends",
          xaxis = list(title = "Years", tickangle = -45),
          yaxis = list(title = "Percentage")
        )
    })

    
    # TAB 3: TABLE
    data_for_table <- reactive({
      data <- filter(mental_data, Country %in% input$cntry & Year %in% input$yr) |>
        select(input$show_vars) 
    })
    
    
    output$table <- DT::renderDataTable({
      data_for_table() 
      })    
    # Q, & add caption/title?
}

# Run the application 
shinyApp(ui = ui, server = server)
