#' Build profit barchart
#' 
#' Function that plots a barchart of the revenues over multiple years. It takes possible machine payments into consideration. 
#' 
#' @param currency The currency in which you want to calculate the profit
#' @param treatment The price per treatment 
#' @param staff The staff costs per treatment
#' @param expected The expected amount of patients per month
#' @param machinepay In case of monthly payment for machine, then the machine costs
#' @param tpp The amount of treatments a patient gets, on average
#' @param max The maximum amount of years to display
#' 
#' @return A ggplot2 object with a barchart of the cumulated profits over the years. 
#' 
# activate packages
library(rlist)
library(ggplot2)
library(FinancialMath)
library(scales)

# Source other documents
#source('calc_rev.R')
#source('getcur.R')



plotter <- function(currency, treatment, staff, expected, machinepay, tpp,max){
  # Define comma function
  my_comma <- scales::label_comma(
    accuracy = 1,
    big.mark = ",",
    decimal.mark = "."
  )
  # Obtain current currency exchanges
  currencies <- get_cur()
  calculat <- calc_rev(
    currency, 
    treatment, 
    staff, 
    expected, 
    machinepay,
    tpp
  )
  # If negative profit, give warning about purchase
  if(machinepay == 0){
    if(calculat$values[3] < 0){
      pic <- ggplot() + 
        labs(title = "\n \n \n
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
      return(pic)
    }
    # Calculate the monthly and yearly revenue and profit without machine payment
    calculat <- calc_rev(
      currency, 
      treatment, 
      staff, 
      expected,
      FALSE, 
      tpp
    )
    values <- list()
    
    # Calculate value of machine in selected currency
    machine <- -(currencies$Last[currency] * 50000)
    # Calculate profits for selected years
    for(item in 1:max){
      profits <- machine + (item * calculat$values[4])
      values <- list.append(values, profits)
    }
    # Create dataframe of calcutated profits
    result = data.frame(
      'Year' = seq(1,max,1),
      'Values' = as.numeric(unlist(values))
    )
    # Create cumulative summation
    result <- result %>%
      mutate(csum = cumsum(Values))
    # Create barchart
    pic <- ggplot(
      data = result, 
      aes(
        x = Year,
        y = Values
      )
    ) + 
      # Set bar color
      geom_bar(
        stat = 'identity',
        fill = '#009CD9'
      ) + 
      # Define labs
      labs(
        title = 'Accumulated Profit', 
        subtitle = paste0(
          'Total after ', 
          max, 
          ' years: ',
          as.character(calculat$signs[currency]),
          " ",
          my_comma(round((calculat$values[4] * max) + machine, 0))
        ),
        y = 'Value machine in £', 
        caption = 'Costs of machine payments included in calculation'
      ) + 
      # Clear theme and set customized theme
      theme_minimal() +
      theme(
        legend.position = "none", 
        plot.title=element_text(
          size=18, 
          face="bold",
          hjust = 0.5
        ), 
        plot.subtitle = element_text(
          size=14, 
          face="bold", 
          hjust = 0.5
        ), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_text(
          size = 12,
          face = 'bold', 
          angle = 0
        ), 
        axis.text.x = element_text(
          size = 12, 
          face = 'bold'
        ),
        axis.title.x = element_text(
          size = 12,
          face = 'bold'
        ),
        plot.caption = element_text(
          size = 8 ,
          face = 'italic'
        )
      ) + 
      scale_y_continuous(
        labels = dollar_format(
          prefix = as.character(calculat$signs[currency]),
          suffix = ""
        )
      ) 
    return(pic)
  }
  # If negative profit, give warning about purchase
  else if(calculat$values[3] < 0){
    pic <- ggplot() + 
      labs(
        title = "\n \n \n
        In this scenario you will make a loss, \n
        please check your settings. \n
        A clinic that will treat less than \n
        one patient per month should not  \n
        invest in this specific machine."
      ) +
      theme_void() +
      theme(
        legend.position = "none", 
        plot.title=element_text(
          size=20,
          face="bold",
          hjust = 0.5
        )
      )
    return(pic)
  }
  else{
    # Calculate the monthy and yearly revenue and profit
    calculat_nopay <- calc_rev(
      currency, 
      treatment,
      staff,
      expected,
      FALSE,
      tpp
    )
    # Calculate profit with machine pay
    calculat_pay <- calc_rev(
      currency, 
      treatment,
      staff,
      expected, 
      machinepay,
      tpp
    )
    values <- list()
    # Calculate machine price
    machine <-  currencies$Last[currency] * 50000
    # find how long before machine is paid back
    levels <- annuity.level(
      pv = machine, 
      n = NA, 
      pmt = machinepay, 
      i = 0.0785, 
      ic = 12, 
      pf = 12
    )
    values = list()
    for(i in 1:max){
      if(i <= round(levels[6]/12)){
        value = calculat_pay$values[4]
        values = list.append(values, value)
      }
      else{
        value = calculat_nopay$values[4]
        values = list.append(values, value)
      }
    }
    # Create dataframe of outcomes
    result = data.frame(
      'Year' = seq(1,max,1),
      'Values' = as.numeric(unlist(values))
    )
    result <- result %>%
      mutate(csum = cumsum(Values))
    # Create barchart
    pic <- ggplot(
      data = result,
      aes(x = Year,
          y = csum
        )
    ) + 
      geom_bar(
        stat = 'identity',
        fill = '#009CD9'
      ) + 
      labs(
        title = 'Accumulated Profit', 
        subtitle = paste0(
          'Total after ', 
          max, 
          ' years: ',
          as.character(calculat$signs[currency]),
          " ",
          my_comma(round(result$csum[nrow(result)], 0))
        ),
        y = 'Value machine in £', 
        caption = 'Costs of machine payments included in calculation'
      ) + 
      theme_minimal() +
      theme(
        legend.position = "none", 
        plot.title=element_text(
          size=18, 
          face="bold",
          hjust = 0.5
        ), 
        plot.subtitle = element_text(
          size=14, 
          face="bold", 
          hjust = 0.5
        ), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_text(
          size = 12, 
          face = 'bold', 
          angle = 0
        ), 
        axis.text.x = element_text(
          size = 12, 
          face = 'bold'
        ),
        axis.title.x = element_text(
          size = 12, 
          face = 'bold'
        ),
        plot.caption = element_text(
          size = 8 ,
          face = 'italic'
        )
      ) + 
      scale_y_continuous(
        labels = dollar_format(
          prefix = as.character(calculat$signs[currency]), 
          suffix = ""
        )
      ) 
    return(pic)
  }
}
