source("common.R")
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

## Save multiple plots
# TODO: perhaps move to common.R
save_plots <- function(path, title = "", ...) {
  pacman::p_load(patchwork)
  
  plots <- list(...)
  
  if (length(plots) < 1) {
    warning("no plots supplied to save_plots()")
    return()
  }
  
  if (length(plots) > 1) {
    result <- patchwork::wrap_plots(plots) +
      patchwork::plot_annotation(
        #title = title,
        #tag_levels = "A",
        #theme = plot_theme
        theme = presentation_theme
      )
    # TODO: calculate fitting width and height values automatically
    ggsave(path, result, width = 12, height = 6)
  } else {
    result <- plots[[1]] + presentation_theme #+ ggtitle(title)
    ggsave(path, result, width = 8, height = 6)
  }
}

## Plot the desired correlation test
plot_cor_test <- function(dataframe, title = "", subtitle = "",
                          xlab = "x", ylab = "y",
                          geom_point = FALSE, zero_line = FALSE, abline = FALSE,
                          smooth = FALSE, stat_smooth = FALSE) {
  plot <- ggplot(dataframe, aes(dataframe[, 1], dataframe[, 2])) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab
    ) +
    presentation_theme
  
  if (geom_point)
    plot <- plot + geom_point(color = "white")
  
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
save_correlation_plots <- function(lin_model, dependent_name, independent_name,
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
    ylab = "sqrt Standardized Residuals",
    geom_point = TRUE,
    smooth = TRUE
  )
  
  ## Normality plots
  qq_p <- plot_cor_test(
    dataframe = data.frame(qqnorm(lin_model$.stdresid, plot.it = FALSE)[[1]],
                           lin_model$.stdresid),
    #title = paste("Q-Q Plot:", title),
    xlab = "Theoretical Quantiles",
    ylab = "Standardized Residuals",
    geom_point = TRUE,
    abline = TRUE
  )
  
  ## Save all plots to disk
  save_plots(
    path = paste(output_dir, "linearity.png", sep = "/", collapse = "/"),
    title = paste("Linearity:", title),
    regres_p, resid_fitted_p
  )
  save_plots(
    path = paste(output_dir, "homoscedasticity.png", sep = "/", collapse = "/"),
    title = paste("Homoscedasticity:", title),
    resid_fitted_p, scale_loc_p
  )
  save_plots(
    path = paste(output_dir, "normality.png", sep = "/", collapse = "/"),
    title = paste("Quantile-Quantile plot:", title),
    qq_p
  )
}


mean_of_residuals_test <- function(rdl_vector, res_thresh) {
  res_mean <- mean(rdl_vector)
  
  return(data.frame(test = c("Mean of residuals close to 0"),
                    result = c(as.character(res_mean)),
                    pass = c(as.character(res_mean <= res_thresh))
                    )
         )
    
}

hypothesis_test <- function(responses, terms, method = "pearson",
                            significance_level = 0.05) {
  confidence_level <- 1 - significance_level
  strong_threshold <- 0.75 #TODO: Figure out threshold
  
  ## Mean of residuals check
  c_test <- cor.test(responses, terms, method = method,
                   conf.level = confidence_level)
  
  #Output 1
  conf_interval <- c(paste(confidence_level * 100, "% confidence interval", sep = ""),
    paste(c_test$conf.int[1], c_test$conf.int[2], sep = " - "), 
    "NA")
  
  #Output 2
  if (c_test$p.value < 0.001) evidence_against_h0 <- "Strong evidence"
  else if (c_test$p.value < 0.05) evidence_against_h0 <- "Moderate evidence"
  else if (c_test$p.value < 0.1) evidence_against_h0 <- "Weak evidence"
  else if (c_test$p.value >= 0.1) evidence_against_h0 <- "Insufficient evidence"
  p_value <- c(
    "Reject H₀ with p-value", 
    as.character(c_test$p.value),
    evidence_against_h0)
  #Output 3
  test_estimate <- c(paste(method, " correlation coefficient >= (-)", strong_threshold, sep = ""),
    as.character(c_test$estimate),
    as.character(c_test$estimate >= strong_threshold || c_test$estimate <= -strong_threshold))
  
  return(data.frame(
    test = c(
      conf_interval[1],
      p_value[1],
      test_estimate[1]
    ),
    result = c(
      conf_interval[2],
      p_value[2],
      test_estimate[2]
    ),
    
    pass = c(
      conf_interval[3],
      p_value[3],
      test_estimate[3]
    )
  ))
}

## Aggregate correlation test results and save to a csv file
save_correlation_stats <- function(lin_model, output_dir) {
  mean_residual_thresh <- 0.005 #TODO: Why this threshold?
  
  mean_of_resid_test <- mean_of_residuals_test(lin_model$.resid, mean_residual_thresh)
  hypo_test <- hypothesis_test(lin_model$response, lin_model$terms)
  
  results <- bind_rows(mean_of_resid_test, hypo_test)
  
  # TODO: Independence X var and residuals check
  
  ## Write the results to a CSV file
  filename <- paste(output_dir, "stat_tests.csv", sep = "/", collapse = "/")
  write.csv(results, filename, row.names = FALSE)
  
  # TODO: add summary output to (a) csv file
  #summary <- summary(lin_model)
}


## Convenience function to set up a basic linear model and write the results
## and plots to disk
correlate <- function(data, response, terms, dependent_name, independent_name,
                      subdir = "", save_results = TRUE) {
  pacman::p_load(ggfortify)
  
  print(paste("Correlating", subdir, independent_name))
  
  output_dir <- paste("./Correlation/Output", subdir, independent_name,
                      sep = "/", collapse = "/")
  
  ## Normalize relevant data using scale() so that mean = 0 and stddev = 1
  normalized_data <- data %>%
    select(c(response, terms)) %>%
    drop_na %>%
    filter(.[[terms]] != 1) %>%
    scale %>%
    data.frame
  
  if (nrow(normalized_data) == 0) {
    warning(paste(subdir, independent_name, "contains only NA values"),
            call. = FALSE)
    print(data %>% select(c(response, terms)))
    return(NA)
  }
  
  ## Prepare the linear model and convert it to a ggplot2 compatible format
  lin_model <- fortify(lm(normalized_data[[response]]
                          ~ normalized_data[[terms]], na.action = na.omit))
  
  ## Standardize column names
  colnames(lin_model)[1] <- "response"
  colnames(lin_model)[2] <- "terms"
  
  if (save_results) {
    ## Create output directories if they did not exist yet
    if (!dir.exists(output_dir))
      dir.create(output_dir, recursive = TRUE)
    
    save_correlation_plots(lin_model, dependent_name, independent_name,
                           output_dir)
    save_correlation_stats(lin_model, output_dir)
  }
  
  # FIXME: ugh
  return(round(cor(lin_model$response, lin_model$terms) ^ 2, 2))
}
