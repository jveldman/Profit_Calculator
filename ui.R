# Import packages
library(shiny)
library(shinydashboard)
library(shinyalert)

# Import functions
source('getcur.R')

# Collect currencies and symbols
currencies <- get_cur()

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    # Set banner with title, menu and label
    dashboardHeader(
        title = 'A product',
        tags$li(
            actionLink(
                'openModal', 
                label = "", 
                icon = icon('info')
            ), 
            class = "dropdown"
        )
    ),
    
    # Define inputs in the menu. 
    dashboardSidebar(
        fluidPage(
            numericInput(
                'treatment', 
                label = 'Treatment Price: ', 
                min = 0, 
                max = 200,
                step = 1, 
                value = 75
            )
        ),
        fluidPage(
            numericInput(
                'staff', 
                label  = 'Staff Costs / Treatment: ', 
                min = 0, 
                max = 200,
                step = 1, 
                value = 20
            )
        ),
        fluidPage(
            numericInput(
                'expected', 
                label = 'Patients per Month: ', 
                min = 0, 
                max = 14, 
                step  = 1, 
                value = 4
            )
        ),
        fluidPage(
            numericInput(
                'tpp', 
                label = 'Treatments per Patient: ', 
                min = 1, 
                max = 50, 
                step = 1, 
                value = 20
            )
        ),
        fluidPage(
            numericInput(
                'machinepay', 
                label = "Machine Finance / Month: ", 
                min = 0, 
                max = 40000, 
                step = 10, 
                value = 800
            )
        ),
        
        fluidPage(
            selectInput(
                "currency",
                label = "Used Currency: ", 
                choices = list(
                    "GBP (£)" = 1,
                    "USD ($)" = 2, 
                    "EUR (€)" = 3, 
                    "CHF (F)" = 4
                ), 
                selected = 1
            )
        ),
        fluidPage(
            sliderInput(
                'range', 
                label = "Number of Years: ",
                min = 1,
                max = 30, 
                value = 5, 
                ticks = FALSE, 
                animate = TRUE
            )
        ), 
        
        # Add link to company
        fluidPage(
            tags$head(tags$style("#shiny-modal img { max-width: 50%; }")),
        HTML('<a href="https://github.com/jveldman"> www.github.com/jveldman/</a>'),
        )
            
    ), 
    
    # Define boxes to display revenues, profits and quantity of treatments.
    dashboardBody(
        fluidRow(
            valueBoxOutput(
                'grpy', 
                width = 3
            ),
            valueBoxOutput(
                'grpm', 
                width = 2
            ),
            valueBoxOutput(
                'ppy',
                width = 3
            ),
            valueBoxOutput(
                'ppm',
                width = 2
            ),
            valueBoxOutput(
                'weekly_treat',
                width = 2
            )
        ), 
        
        # Define plots
        fluidRow(
            box(plotOutput('barchart')),
            box(plotOutput('piechart'))
        )
    )
)
