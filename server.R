## app.R ##

# Import packages
library(shiny)
library(shinydashboard)
library(formattable)

# Import functions
source('barchart.R')
source('piechart.R')
source('calc_rev.R')


server <- function(input, output) {
  # Set comma function
  my_comma <- scales::label_comma(
    accuracy = 1,
    big.mark = ",",
    decimal.mark = "."
  )
  # Set up revenue calculator with inputs
  revenues <- reactive({
        calc_rev(
          as.numeric(input$currency),
          as.numeric(input$treatment), 
          as.numeric(input$staff),
          as.numeric(input$expected),
          as.numeric(input$machinepay),
          as.numeric(input$tpp)
        )
  })
  
  # Set up dependency of currency signs based on input
  signature <- reactive({
    input$currency
    isolate({
        as.numeric(input$currency)
    })
  })
  
  # Box for monthly revenue display
  output$grpm <- renderValueBox({
    revenues <- revenues()
    signa <- signature()
    valueBox(
      paste0(
        revenues$signs[signa],
        ' ', 
        my_comma(revenues$values[1])
        ), 
      "Gross Revenue per Month", 
      color = 'light-blue'
    )
    
  })
  
  # Box for yearly revenue display
  output$grpy <- renderValueBox({
    revenues <- revenues()
    signa <- signature()
    valueBox(
      paste0(
        revenues$signs[signa],
        ' ',
        my_comma(revenues$values[2])
      ), 
      "Gross Revenue per Year", 
      color = 'light-blue'
    )
    
  })
  
  # Box for monthly profit display
  output$ppm <- renderValueBox({
    revenues <- revenues()
    signa <- signature()
    valueBox(
      paste0(
        revenues$signs[signa],
        ' ',
        my_comma(revenues$values[3])
      ), 
      "Profit per Month", 
      color = 'light-blue'
    )
    
  })
  
  # Box for yearly profit display
  output$ppy <- renderValueBox({
    revenues <- revenues()
    signa <- signature()
    valueBox(
      paste0(
        revenues$signs[signa],
        ' ',
        my_comma(revenues$values[4])
      ), 
      "Profit per Year", 
      color = 'light-blue'
    )
  })
  
  # Box for monthly treatment count display
  output$weekly_treat <- renderValueBox({
    valueBox(
      (input$expected * input$tpp),
      "Treatments per Month", 
      color = 'light-blue'
    )
  })
  
  # Barchart with accumulated profits per year
  output$barchart <- renderPlot({
   plotter(
     signature(),
     input$treatment,
     input$staff,
     input$expected,
     input$machinepay,
     input$tpp,
     input$range
    )
  })
  
  # Piechart with expenses in % of revenue
  output$piechart <- renderPlot({
    piechart(
      signature(),
      input$treatment,
      input$staff,
      input$expected,
      input$machinepay,
      input$tpp
    )
  })
  
  # Display popup of director when starting dashboard. 
  observeEvent('', {
    showModal(
      modalDialog(
        includeHTML("www/popup.html"), 
        easyClose = TRUE, 
        footer = modalButton("OK"), 
        size = "s"
      )
    )
  })
  
  # Show director popup when clicking info button
  observeEvent(input$openModal, {
    showModal(
      modalDialog(
        includeHTML("www/popup.html"), 
        easyClose = TRUE, 
        footer= modalButton("OK"), 
        size = "s"
      )
    )
  })
  
}