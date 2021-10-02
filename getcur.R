#' Currency loader
#' 
#' Loads currency exchange rates for GBP to USD, EUR and CHF. 
#' 
#' @return Dataframe with currency exchange rates including dates, high low, etc. 

library(quantmod)

get_cur <- function(){
  from <- c('GBP', 'GBP', 'GBP', 'GBP')
  to <- c('GBP', 'USD', "EUR", 'CHF')
  a <- getQuote(paste0(from, to, '=X'))
  return(a)
}

