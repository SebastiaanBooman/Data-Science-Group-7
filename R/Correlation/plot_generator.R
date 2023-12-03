pacman::p_load(ggplot2)

#' Generate a ggplot2:ggplot based on a dataframe with various optional decorations
#' 
#' @param dataframe dataframe, dataframe should contain two or one axis depending on plot
#' @param title character = "", optional plot title
#' @param substitle character = "", optional subtitle
#' @param xlab character = "x", optional x label for x-axis 
#' @param ylab character = "y", optional y label for y-axis
#' @param geom_hist logical = FALSE, creates a plot using ggplot2 `geom_histogram()`, uses first column of `dataframe()` for data
#' @param geom_box logical = FALSE, creates a plot using ggplot2 `geom_box()`, uses first column of `dataframe()` for data
#' @param geom_point logical = FALSE, adds scatter points to plot using ggplot2 `geom_point()` 
#' @param zero_line logical = FALSE, adds a horizontal line on y=0 using ggplot2 `geom_hline()` 
#' @param geom_abline logical = FALSE, adds a linear smooth line using ggplot2 `geom_smooth()`
#' @param smooth logical = FALSE, adds regular smooth line using ggplot2 `geom_smooth()` with "Loess"
#' @param theme ggplot2::theme, the theme to use for the plot
#' @returns ggplot2:ggplot, the plot created using given parameters
generate_plot <- function(dataframe, theme, title = "", subtitle = "",
                          xlab = "x", ylab = "y",
                          geom_hist = FALSE, geom_box = FALSE,
                          geom_point = FALSE, zero_line = FALSE, abline = FALSE,
                          smooth = FALSE) {
  #' Plot the desired correlation test
  
  #create base plot based on plot type (geom_hist, geom_box or generic)
  if (geom_hist) {
    plot <- ggplot(dataframe, aes(dataframe[, 1])) +
      geom_histogram(color = "black", fill = "white")
  }
  
  else if (geom_box){
    plot <- ggplot(dataframe, aes(y = dataframe[,1])) +
      geom_boxplot() +
      scale_x_discrete()
  }
  
  else{
    plot <- ggplot(dataframe, aes(dataframe[, 1], dataframe[, 2])) 
  }
  
  ## add peripherals to plot
  plot <- plot +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab
    ) +
    theme
  
  ## optional peripherals
  if (geom_point)
    plot <- plot + geom_point(color = "white", size = 1)
  
  if (abline)
    plot <- plot + geom_smooth(method = "lm", se = FALSE) #geom_abline()
  
  if (smooth)
    plot <- plot + geom_smooth(se = FALSE)
  
  
  if (zero_line)
    plot <- plot + geom_hline(yintercept = 0, color = "white")
  
  return(plot)
}

#TODO: Figure out what type this function actually returns
#' Combine two or more plots in one
#' 
#' @param plot_list arraylike, list containing different ggplot2 instances (i.e list of ggplot elements). `length` should be >= 2
#' @param title character, plot title
#' @param theme ggplot2::theme, the theme to use for the plot
#' @param substitle character = "", optional subtitle
#' @param tag_lvl logical = TRUE, whether to add references to each subplot using the alphabet (e.g "A","B", "C" ... etc as annotations for each plot)
#' 
#' @returns ggplot2:ggplot | None, the plot created after merging or none if `plot_list` does not contain enough plots to merge
combine_plots <- function(plot_list, title, theme, sub_title = "", tag_lvl = TRUE) {
  #' Combines multiple plots from plot_list into one and returns as plot
  pacman::p_load(patchwork)
  
  if (length(plot_list) < 2) {
    warning(paste("not enough plots supplied to combine_plots() received", length(plot_list), "expected >=2"))
    return()
  }
  
  if(tag_lvl)
    result <- patchwork::wrap_plots(plot_list) +
      patchwork::plot_annotation(
        title = title,
        subtitle = sub_title,
        tag_levels = "A",
        theme = theme
      )
  else
    result <- patchwork::wrap_plots(plot_list) +
      patchwork::plot_annotation(
        title = title,
        subtitle = sub_title,
        theme = theme
      )
  return(result)
}

## Default implementations for plots
gen_regression_plot <- function(f_lin_model, x_lab, y_lab, theme, s, r_squared){
  return(
    generate_plot(
      dataframe = subset(f_lin_model, select = c(y, x)),
      title = "Regression Plot",
      subtitle = paste(
        # FIXME: ew, inefficient, but who cares
        "S = ", round(s, 2), "  ",
        "R-Sq = ", round(r_squared, 2), "  ",
        sep = ""
      ),
      xlab = x_lab,
      ylab = y_lab,
      geom_point = TRUE,
      abline     = TRUE,
      theme=theme
    )
  )
}
  
gen_resid_fitted_plot <- function(f_lin_model, theme){
  return(
    generate_plot(
      dataframe = subset(f_lin_model, select = c(.fitted, .resid)),
      title = "Residuals vs. Fits",
      xlab = "Fitted Values",
      ylab = "Residuals",
      geom_point = TRUE,
      zero_line = TRUE,
      smooth = TRUE,
      theme=theme
    )
  )
}

gen_scale_loc_plot <- function(f_lin_model, theme){
  return(
    generate_plot(
      dataframe = data.frame(f_lin_model$.fitted, sqrt(abs(f_lin_model$.stdresid))),
      title = "Scale-Location",
      xlab = "Fitted Values",
      ylab = expression(sqrt("Standardized Residuals")),
      geom_point = TRUE,
      smooth = TRUE,
      theme=theme
    )
  )
}

gen_histogram_plot <- function(f_lin_model, theme){
  return(
    #TODO: would be nice to add simulation of normal distr and actual (see https://rpubs.com/dvdunne/adding_legend)
    generate_plot(
      dataframe = data.frame(f_lin_model$.stdresid, f_lin_model$.stdresid),
      title = "std residuals distribution histogram",
      xlab = "Deviation",
      ylab = "Density",
      geom_hist = TRUE,
      theme=theme
    )
  )
}

gen_box_plot <- function(f_lin_model, theme, title="std residuals distribution boxplot", ylab=expression(sqrt("Standardized residual spread"))){
  return(
    generate_plot(
      dataframe = data.frame(f_lin_model$.stdresid, f_lin_model$.stdresid),
      title = title,
      xlab = "",
      ylab = ylab,
      geom_box = TRUE,
      theme=theme
    )
  )
}

gen_qq_plot <- function(f_lin_model, theme, sw_test, sw_p){
  return(
    generate_plot(
      dataframe = data.frame(qqnorm(f_lin_model$.stdresid, plot.it = FALSE)[[1]],
                             f_lin_model$.stdresid),
      title = paste("Normal Q-Q Plot"),
      subtitle = paste(
        "Shapiro-Wilk: ",
        "W = ", round(sw_test, 2), "  ",
        "p = ", round(sw_p, 2), "  ",
        sep = ""
      ),
      xlab = "Theoretical Quantiles",
      ylab = "Standardized Residuals",
      geom_point = TRUE,
      abline = TRUE,
      theme=theme
    )
  )
}