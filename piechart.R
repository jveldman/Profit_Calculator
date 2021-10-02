#'  Build Donut Chart
#' 
#' Function to create a Donut chart to see how your monthly revenue is built up.
#' 
#' @param currency The currency in which you want to calculate the profit
#' @param treatment The price per treatment 
#' @param staff The staff costs per treatment
#' @param expected The expected amount of patients per month
#' @param machinepay In case of monthly payment for machine, then the machine costs
#' @param tpp The amount of treatments a patient gets, on average
#' 
#' @return A ggplot2 object containing a Donut chart displaying how the monthly revenue is divided


# Import packages
library(scales)
library(ggplot2)
library(ggforce)
library(dplyr)

# Source revenue calculator
#source('calc_rev.R')

# Set theme of Donut chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(
      size=24,
      face="bold"
    ), 
    legend.position = 'none', 
  )


piechart <- function(currency, treatment, staff, expected, machinepay, tpp){
  my_comma <- scales::label_comma(
    accuracy = 1, 
    big.mark = ",",
    decimal.mark = "."
  )
  # calculate revenue
  calculat <- calc_rev(
    currency, 
    treatment, 
    staff, 
    expected, 
    machinepay,
    tpp
    )
  
  # Obtain currency symbols
  
  # Create dataframe from previous values
  calculat$signs[currency]
  df <- data.frame(
    group = c(
      'Machine',
      'Staff',
      'Profit'
    ),
    value = c(
      machinepay, 
      staff*tpp*expected, 
      (treatment*tpp*expected)-machinepay-(staff*tpp*expected)
    )
  )
  # Calculate monthly income defining that a patient need, on average, 15 treatments. 
  income = treatment*15*expected
  # Calculate percentages of values defined in dataframe
  df$fraction <- round(df$value / sum(df$value)*100,0)
  
  # If profit < 0, warn for negative outcome
  if(df$value[3] < 0){
    q <- ggplot() + 
      labs(
      title = "\n \n \n
      In this scenario you will make a loss, \n
      please check your settings. \n
      A clinic that will treat less than \n
      one patient per month should not  \n
      invest in this specific machine.") +
      theme_void() +
      theme(
        legend.position = "none",
        plot.title=element_text(
          size=20, 
          face="bold",
          hjust = 0.5
        )
      )
    return(q)
  }
  # otherwise, plot donut chart 
  else{
    # Define min and max of axis
    df$ymax <- cumsum(df$fraction)
    df$ymin <- c(
      0,
      head(
        df$ymax, 
        n = -1
      )
    )
    # Indicate where labels will be placed
    df$labelPosition <- (df$ymax + df$ymin) / 2
    df$label <- paste0(
      df$group, 
      "\n", 
      df$fraction,
      " %"
    )
    # Define ggplot
    q <- ggplot(
      df,
      aes(
        ymax=ymax, 
        ymin=ymin,
        xmax=4, 
        xmin=3, 
        fill=group
      )
    ) +
      geom_rect() +
      # Add indicators of donut chart chunks
      geom_text(
        x=4.87,
        aes(
          y=labelPosition, 
          label=label,
          color=group
        ),
        size=6
      ) + 
      # # Define colours of donut chart pieces
      scale_fill_manual(
        values = c(
          'gray',
          '#009CD9',
          '#1E295D',
          'black'
        )
      ) +
      scale_color_manual(
        values = c(
          'gray',
          '#009CD9',
          '#1E295D',
          'black'
        )
      ) +
      # Make donut chart circular
      coord_polar(theta="y") +
      xlim(c(1, 4.5)) +
      # Remove any theme settings 
      theme_void() +
      # Add customised theme settings. 
      theme(
        legend.position = "none", 
        plot.title=element_text(
          size=20,
          face="bold",
          hjust = 0.5
        ), 
        plot.subtitle = element_text(
          size=16,
          face="bold", 
          hjust = 0.5
        )
      ) + 
      # Add labels
      labs(
        title = 'Monthly Expenses in % of Revenue', 
        subtitle = paste0(
          'Total Revenue: ',
          as.character(calculat$signs[currency]),
           " ",
          my_comma(calculat$values[1])
        )
      )
    return(q)
  }
}

