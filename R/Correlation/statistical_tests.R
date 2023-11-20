mean_of_residuals_test <- function(rdl_vector, res_thresh) {
  res_mean <- mean(rdl_vector)

  return(data.frame(
    test = c("Mean of residuals close to 0"),
    result = c(as.character(res_mean)),
    pass = c(as.character(res_mean <= res_thresh))
  )
  )
}

hypothesis_test <- function(responses, terms, method = "pearson",
                            significance_level = 0.05) {
  confidence_level <- 1 - significance_level
  strong_threshold <- 0.75 # TODO: Figure out threshold

  ## Mean of residuals check
  c_test <- cor.test(responses, terms, method = method,
                     conf.level = confidence_level)

  ## Output 1
  conf_interval <- c(paste(confidence_level * 100, "% confidence interval",
                           sep = ""),
                     paste(c_test$conf.int[1], c_test$conf.int[2],
                           sep = " - "),
                     "NA")

  ## Output 2
  if (c_test$p.value < 0.001) evidence_against_h0 <- "Strong evidence"
  else if (c_test$p.value < 0.05) evidence_against_h0 <- "Moderate evidence"
  else if (c_test$p.value < 0.1) evidence_against_h0 <- "Weak evidence"
  else if (c_test$p.value >= 0.1) evidence_against_h0 <- "Insufficient evidence"

  p_value <- c(
    "Reject H₀ with p-value",
    as.character(c_test$p.value),
    evidence_against_h0
  )

  ## Output 3
  test_estimate <- c(
    paste(method, " correlation coefficient >= (-)", strong_threshold,
          sep = ""),
    as.character(c_test$estimate),
    as.character(c_test$estimate >= strong_threshold
                 || c_test$estimate <= -strong_threshold)
  )

  return(data.frame(
    test   = c(conf_interval[1], p_value[1], test_estimate[1]),
    result = c(conf_interval[2], p_value[2], test_estimate[2]),
    pass   = c(conf_interval[3], p_value[3], test_estimate[3])
  ))
}

shapiro_wilk_test <- function(data_range, label) {
  ## p-value > 0.05? Assume normality
  sw_test <- shapiro.test(data_range)
  assume_linearity <- sw_test$p.value > 0.05

  ## Output 1
  sw_w <- c(paste("Shapiro Wilk: statistic for", label),
            as.character(sw_test$statistic), "NA")

  ## Output 2
  sw_p <- c(paste("Shapiro Wilk: P-value for", label, "indicates linearity"),
            as.character(sw_test$p.value),
            assume_linearity)

  return(data.frame(
    test   = c(sw_w[1], sw_p[1]),
    result = c(sw_w[2], sw_p[2]),
    pass   = c(sw_w[3], sw_p[3])
  ))
}

## Aggregate correlation test results and save to a csv file
save_correlation_stats <- function(lin_model, output_dir) {
  mean_residual_thresh <- 0.005 # TODO: Why this threshold?

  mean_of_resid_test <- mean_of_residuals_test(lin_model$.resid,
                                               mean_residual_thresh)
  hypo_test <- hypothesis_test(lin_model$response, lin_model$terms)
  shapiro_wilk_test <- shapiro_wilk_test(lin_model$.resid, "residuals")
  # TODO: Should we shapiro test even more?

  results <- bind_rows(mean_of_resid_test, hypo_test, shapiro_wilk_test)

  # TODO: Independence X var and residuals check

  ## Write the results to a CSV file
  filename <- paste(output_dir, "stat_tests.csv", sep = "/", collapse = "/")
  write.csv(results, filename, row.names = FALSE)

  # TODO: add summary output to (a) csv file
  #summary <- summary(lin_model)
}
