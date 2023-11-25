source("common.R")
source("./Correlation/statistical_tests.R")
pacman::p_load(ggplot2)

presentation_theme <- theme(
  line = element_line(color = "white", linewidth = .5, linetype = 1,
                      lineend = "butt"),
  rect = element_rect(fill = NA, color = NA, linewidth = .5, linetype = 1),
  text = element_text(color = "lightgray"),

  axis.text.x = element_text(color = "white"),
  axis.text.y = element_text(color = "white"),

  axis.ticks = element_line(color = "white", linewidth = .5),

  axis.line   = element_line(color = "white", linewidth = .5,
                             lineend = "square"),
  axis.line.x = NULL,
  axis.line.y = NULL,

  panel.background = element_blank(),
  panel.border     = element_blank(),
  panel.grid       = element_blank(),

  plot.background = element_blank()
)

doc_theme <- theme(
  line = element_line(color = "white", linewidth = .5, linetype = 1,
                      lineend = "butt"),
  rect = element_rect(fill = "black", color = NA, linewidth = .5, linetype = 1),
  text = element_text(color = "lightgray"),
  
  axis.text.x = element_text(color = "white"),
  axis.text.y = element_text(color = "white"),
  
  axis.ticks = element_line(color = "white", linewidth = .5),
  
  axis.line   = element_line(color = "white", linewidth = .5,
                             lineend = "square"),
  axis.line.x = NULL,
  axis.line.y = NULL,
  
  plot.title = element_text(size=22),
  
  panel.border     = element_blank(),
  panel.grid       = element_blank(),
  
  #plot.background = element_blank()
)


#Combines multiple plots from plot_list into one and returns as plot
combine_plots <- function(theme, plot_list, title, tag_lvl = TRUE) {
  pacman::p_load(patchwork)
  
  if (length(plot_list) < 2) {
    warning(paste("not enough plots supplied to combine_plots() received", length(plot_list), "expected >=2"))
    return()
  }
  title <- paste("\"", title,"\"", sep = "")
  if(tag_lvl)
    result <- patchwork::wrap_plots(plot_list) +
      patchwork::plot_annotation(
        title = paste("Scope:", title, "(Combination of plots)"),
        tag_levels = "A",
        theme = theme
      )
  else
    result <- patchwork::wrap_plots(plot_list) +
      patchwork::plot_annotation(
        title = paste(title, ": Combination Plots"),
        theme = theme
      )
  return(result)
}

## Plot the desired correlation test
plot_cor_test <- function(dataframe, title = "", subtitle = "",
                          xlab = "x", ylab = "y",
                          geom_hist = FALSE,
                          geom_point = FALSE, zero_line = FALSE, abline = FALSE,
                          smooth = FALSE, stat_smooth = FALSE) {
  if (geom_hist) {
    plot <- ggplot(dataframe, aes(dataframe[, 1])) +
      geom_histogram(color = "black", fill = "white") +
      labs(
        title = title,
        subtitle = subtitle,
        x = xlab,
        y = ylab
      ) +
      presentation_theme

    return(plot)
  }

  plot <- ggplot(dataframe, aes(dataframe[, 1], dataframe[, 2])) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab
    ) +
    presentation_theme

  if (geom_point)
    plot <- plot + geom_point(color = "white", size = 1)

  if (abline)
    plot <- plot + geom_smooth(method = "lm", se = FALSE) #geom_abline()

  if (smooth)
    plot <- plot + geom_smooth(se = FALSE)

  # TODO: Want to change these through optional func parameters?
  #       -> dict with values, unpacking ideal
  if (stat_smooth)
    plot <- plot +
      stat_smooth(method = "loess", span = 0.1, colour = I("red"), se = FALSE)

  if (zero_line)
    plot <- plot + geom_hline(yintercept = 0, color = "white")

  return(plot)
}

## Generate correlation test plots and save them to a csv file
save_correlation_plots <- function(data_scope_str, lin_model, dependent_name, independent_name,
                                   output_dir) {
  title <- paste(dependent_name, "~", independent_name)

  ## Linearity plots
  regres_p <- plot_cor_test(
    dataframe = subset(lin_model, select = c(response, terms)),
    title = "Regression Plot",
    subtitle = paste(
      # FIXME: ew, inefficient, but who cares
      "S = ", round(sqrt(diag(vcov(lm(lin_model$response ~ lin_model$terms)))),
                    2), "  ",
      "R-Sq = ", round(cor(lin_model$response, lin_model$terms) ^ 2, 2), "  ",
      sep = ""
    ),
    xlab = independent_name,
    ylab = dependent_name,
    geom_point = TRUE,
    abline     = TRUE
  )
  resid_fitted_p <- plot_cor_test(
    dataframe = subset(lin_model, select = c(.fitted, .resid)),
    title = "Residuals vs. Fits",
    xlab = "Fitted Values",
    ylab = "Residuals",
    geom_point = TRUE,
    zero_line = TRUE
    #smooth = TRUE
  )

  ## Homoscedasticity plot (+ resid_fitted_p)
  scale_loc_p <- plot_cor_test(
    dataframe = data.frame(lin_model$.fitted, sqrt(abs(lin_model$.stdresid))),
    title = "Scale-Location",
    xlab = "Fitted Values",
    ylab = expression(sqrt("Standardized Residuals")),
    geom_point = TRUE,
    smooth = TRUE
  )

  ## Histogram
  hist_p <- plot_cor_test(
    dataframe = data.frame(lin_model$.stdresid, lin_model$.stdresid),
    title = paste("Histogram"),
    xlab = independent_name,
    ylab = "Density",
    geom_hist = TRUE
  )

  ## Normality plots
  # FIXME: ew, inefficient, but who cares
  sw_test <- shapiro.test(lin_model$.resid) #TODO: MOVE THIS TO PARAMETER!
  qq_p <- plot_cor_test(
    dataframe = data.frame(qqnorm(lin_model$.stdresid, plot.it = FALSE)[[1]],
                           lin_model$.stdresid),
    title = paste("Normal Q-Q Plot"),
    subtitle = paste(
      "Shapiro-Wilk: ",
      "W = ", round(sw_test$statistic, 2), "  ",
      "p = ", round(sw_test$p.value, 2), "  ",
      sep = ""
    ),
    xlab = "Theoretical Quantiles",
    ylab = "Standardized Residuals",
    geom_point = TRUE,
    abline = TRUE
  )

  ## Save all plots to disk
  combined_plots <- combine_plots(doc_theme, list(regres_p, resid_fitted_p, qq_p, scale_loc_p), data_scope_str)
  save_plot(
    path = paste(output_dir, "uberplot.png", sep = "/", collapse = "/"),
    combined_plots
  )
  save_plot(
    path = paste(output_dir, "histogram.png", sep = "/", collapse = "/"),
    hist_p
  )
  #TODO: Fix these commented commands (currently using deprecated `save_plots` function, should use save_plot)
  #save_plots(
  #  path = paste(output_dir, "linearity.png", sep = "/", collapse = "/"),
  #  title = paste("Linearity:", title),
  #  regres_p, resid_fitted_p
  #)
  #save_plots(
  #  path = paste(output_dir, "homoscedasticity.png", sep = "/", collapse = "/"),
  #  title = paste("Homoscedasticity:", title),
  #  resid_fitted_p, scale_loc_p
  #)
  #save_plots(
  #  path = paste(output_dir, "normality.png", sep = "/", collapse = "/"),
  #  title = paste("Quantile-Quantile plot:", title),
  #  qq_p
  #)
}