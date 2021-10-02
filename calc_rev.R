#'  Calculate the revenue and profits per month and year
#' 
#' Function to calculate the revenue and profit per month and per year. Used in the dashboard for display and in other functions. 
#' 
#' @param currency The currency in which you want to calculate the profit
#' @param treatment The price per treatment 
#' @param staff The staff costs per treatment
#' @param expected The expected amount of patients per month
#' @param machinepay In case of monthly payment for machine, then the machine costs
#' @param tpp The amount of treatments a patient gets, on average
#' 
#' @example calc_rev(1, 75, 20, 1, FALSE)
#' 
#' @return a 4x3 dataframe containing profit and revenue per month and per year. 


# calcs
#source('getcur.R')

calc_rev <- function(currency, treatment, staff, expected, machinepay, tpp){
  currencies <- get_cur()
# Revenue calculations
  rev <- treatment*expected*tpp 
  rev_y <- rev * 12

# Profit calculations
  profit <- (rev - (staff*expected*tpp) - ifelse(machinepay == FALSE,0,machinepay))
  profit_y <- profit * 12

# Display in dataframe 
  result <- data.frame(
    "vars" = c(
      'revenue',
      'revenue_year',  
      'profit', 
      'profit_year'
    ),
    'signs' = c(
      enc2utf8("\u00A3"),
      '$',
      enc2utf8('\u20AC'), 
      'F'
    ),
    "values" = c(
      rev, rev_y, 
      profit,
      profit_y
    )
  )
# Round values 
  result$values <- round(result$values,0)
  return(result)
}
#