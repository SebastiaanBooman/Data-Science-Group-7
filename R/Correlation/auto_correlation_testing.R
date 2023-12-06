#' creates a time series object from a given column
#'
#' @param column the data that needs to be put into a time series
#'
#' @returns time series object of the length of the data provided
timeseries_from_column <- function(column){
  return(ts(data = seq(from = 1, to = length(column), by = 1)))
}

#' creates a fortified model given a dependant and independant variable
#'
#'  @param dep_var the dependant variable (y)
#'  @param indep_var the independant variable (x)
#'  
#'  @returns fortified model of the given variables
create_model <- function(dep_var, indep_var){
  return(fortify(lm(dep_var ~ indep_var)))
}

#' performs auto correlation testing over the residuals for 2 variables
#' 
#' @param column the column to be used for auto correlation testing
#' 
#' @returns a dataframe containing the lag values and the correlation values
auto_correlation_dataframe <- function(column){
  model <- create_model(column, timeseries_from_column(column))
  bacf <- acf(model$.resid, type = "correlation", plot = FALSE)
  bacfdf <- with(bacf, data.frame(lag, acf))
  return(bacfdf)
}

#' plots the auto correlation results in a ggplot
#' to get the dataframe needed for this function, call the function 'auto_correlation_dataframe(var1, var2)'
#'
#' @param auto_corr_dataframe the dataframe that gets returned from the 'auto_correlation_dataframe' function
#'
#' @returns a ggplot object containing the plot for the correlation test
auto_correlation_plot <- function(auto_corr_dataframe){
  corr_plot <- ggplot(data = auto_corr_dataframe, mapping = aes(x = lag, y = acf, )) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5)) +
    scale_y_continuous(breaks = seq(from = -1, to = 1.0, by = 0.1))
  return(corr_plot)
}