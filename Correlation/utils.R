source("../common.R", chdir = TRUE)

## Save multiple plots
# TODO: perhaps move to common.R
save_plots <- function(filename, title = "", ...) {
  pacman::p_load(patchwork)

  plots <- list(...)

  if (length(plots) < 1) {
    warning("no plots supplied to save_plots()")
    return()
  }

  plot_theme <- theme(plot.title = element_text(face = "bold"))

  if (length(plots) > 1) {
    result <- patchwork::wrap_plots(plots) +
      patchwork::plot_annotation(
        title = title,
        tag_levels = "A",
        theme = plot_theme
      )
  } else {
    result <- plots[[1]] + ggtitle(title) + plot_theme
  }

  # TODO: calculate fitting width and height values automatically
  ggsave(filename, result, width = 16, height = 8)
}

## Plot the desired correlation test
plot_cor_test <- function(dataframe, xlab, ylab, title = "", geom_point = FALSE,
                          zero_line = FALSE, abline = FALSE, smooth = FALSE,
                          stat_smooth = FALSE) {
  pacman::p_load(ggplot2)

  plot <- ggplot(dataframe, aes(dataframe[, 1], dataframe[, 2])) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(title) +
    theme_bw()

  if (geom_point)
    plot <- plot + geom_point()

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
    plot <- plot + geom_hline(yintercept = 0)

  return(plot)
}

## Generate correlation test plots and save them to a csv file
save_correlation_plots <- function(lin_model, response, terms,
                                   output_dir) {
  title <- paste(response, "~", terms)

  ## Linearity plots
  regres_p <- plot_cor_test(
    # TODO: Subset?
    dataframe = data.frame(lin_model$response, lin_model$terms),
    xlab = terms,
    ylab = response,
    title = paste(response, "vs", terms),
    geom_point = TRUE,
    abline =  TRUE
  )
  resid_fitted_p <- plot_cor_test(
    # TODO: Subset?
    dataframe = data.frame(lin_model$.fitted, lin_model$.resid),
    xlab = "Fitted values",
    ylab = "Residuals",
    title = "Residuals vs fitted values",
    geom_point = TRUE,
    zero_line  = TRUE,
    smooth = TRUE
  )

  ## Homoscedasticity plot (+ resid_fitted_p)
  scale_loc_p <- plot_cor_test(
    dataframe = data.frame(lin_model$.fitted, sqrt(abs(lin_model$.stdresid))),
    xlab = "Fitted values",
    ylab = "sqrt standardized residuals",
    title = "Scale location",
    geom_point = TRUE,
    smooth = TRUE
  )

  ## Normality plots
  qq_p <- plot_cor_test(
    dataframe = data.frame(qqnorm(lin_model$.stdresid, plot.it = FALSE)[[1]],
                           lin_model$.stdresid),
    xlab = "Theoretical quantiles",
    ylab = "Standardized residuals",
    #title = paste("Q-Q Plot:", title),
    geom_point = TRUE,
    abline = TRUE
  )

  ## Save all plots to disk
  save_plots(
    paste(output_dir, "linearity.png", sep = "/", collapse = "/"),
    paste("Linearity:", title),
    regres_p,
    resid_fitted_p
  )
  save_plots(
    paste(output_dir, "homoscedasticity.png", sep = "/", collapse = "/"),
    paste("Homoscedasticity:", title),
    resid_fitted_p,
    scale_loc_p
  )
  save_plots(
    paste(output_dir, "normality.png", sep = "/", collapse = "/"),
    paste("Q-Q Plot:", title),
    qq_p
  )
}


mean_of_residuals_test <- function(rdl_vector, res_thresh) {
  res_mean <- mean(rdl_vector)

  return(list(
    c("Mean of residuals close to 0", res_mean, res_mean <= res_thresh)
  ))
}

hypothesis_test <- function(responses, terms, method = "pearson",
                            significance_level = 0.05) {
  confidence_level <- 1 - significance_level
  strong_threshold <- 0.75

  ## Mean of residuals check
  test <- cor.test(responses, terms, method = method,
                   conf.level = confidence_level)

  return(list(
    c(paste(confidence_level * 100, "% confidence interval", sep = ""),
      paste(test$conf.int[1], test$conf.int[2], sep = " - "), "NA"),
    c("Reject H₀ with p-value", test$p.value,
      test$p.value <= significance_level),
    c(paste("Strong", method, "correlation coefficient"), test$estimate,
      test$estimate >= strong_threshold || test$estimate <= -strong_threshold)
  ))
}

## Aggregate correlation test results and save to a csv file
save_correlation_stats <- function(lin_model, output_dir) {
  mean_residual_thresh <- 0.005

  rows <- c(
    mean_of_residuals_test(lin_model$.resid, mean_residual_thresh),
    hypothesis_test(lin_model$response, lin_model$terms)
  )

  results <- data.frame(test = character(0), result = character(0),
                        pass = character(0))
  colnames(results) <-  c("test", "result", "pass")
  results <- bind_rows(lapply(rows, setNames, names(results)))

  # TODO: Independence X var and residuals check

  ## Write the results to a CSV file
  filename <- paste(output_dir, "stat_tests.csv", sep = "/", collapse = "/")
  write.csv(results, filename, row.names = FALSE)

  # TODO: add summary output to (a) csv file
  #summary <- summary(lin_model)
}


## Convenience function to set up a basic linear model and write the results
## and plots to disk
save_correlation_results <- function(data, response, terms, title) {
  pacman::p_load(ggfortify)

  output_root <- "./Correlation/Output"
  output_dir <- paste(output_root, title, sep = "/", collapse = "/")

  ## Prepare the linear model and convert it to a ggplot2 compatible format
  lin_model <- fortify(lm(data[[response]] ~ data[[terms]]))

  ## Standardize column names
  colnames(lin_model)[1] <- "response"
  colnames(lin_model)[2] <- "terms"

  ## Create output directories if they did not exist yet
  if (!dir.exists(output_root))
    dir.create(output_root)
  if (!dir.exists(output_dir))
    dir.create(output_dir)

  save_correlation_plots(lin_model, response, terms, output_dir)
  save_correlation_stats(lin_model, output_dir)
}
