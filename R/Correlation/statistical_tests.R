mean_of_residuals_test <- function(rdl_vector, res_thresh) {
  res_mean <- mean(rdl_vector)

  return(data.frame(
    test = c("Mean of residuals close to 0"),
    result = c(as.character(res_mean)),
    pass = c(as.character(res_mean <= res_thresh))
  )
  )
}

outlier_test <- function(var_name, data_range){
  #' Checks if there are outliers present in numerical `data_range` using IQR criterion
  sorted_range <- sort(data_range)
  range_len <- length(sorted_range)
  quantiles <- quantile(sorted_range)
  q1 <- quantiles[2]
  q3 <- quantiles[4]
  
  #Since range is sorted only have to check values before Q1 and after Q3 for potential outliers
  #Depending if dataset is even or odd, different q1 and q3 values get chosen
  if (range_len %% 2 == 0){
    before_range <- 1: ceiling(range_len * 0.25)
    index_after_q3 <- ceiling(range_len * 0.75)
    after_range <- index_after_q3: (index_after_q3 + (range_len - index_after_q3) )
  }
  else{
    before_range <- 1: ((range_len * 0.25) %/% 1)
    index_after_q3 <- ceiling(range_len * 0.75) + 1
    after_range <- index_after_q3: (index_after_q3 + (range_len - index_after_q3) )
  }
  low_out_amt <- 0
  no_low_out <- "TRUE"
  for(d in sorted_range[before_range])
    if (d < q1 * -1.5) low_out_amt <- low_out_amt+1
  if (low_out_amt > 0) no_low_out <- "FALSE"
  
  high_out_amt <- 0
  no_high_out <- "TRUE"
  for(d in sorted_range[after_range])
    if (d > q3 * 1.5) high_out_amt <- high_out_amt+1
  if (high_out_amt > 0) no_high_out <- "FALSE"
  
  #Output 1
  low_out <- c(paste("Outliers (-) IQR amt for", var_name),
               low_out_amt,
               no_low_out
  )
  
  #Output 2
  high_out <- c(paste("Outliers (+) IQR amt for", var_name),
                high_out_amt,
                no_high_out
  )
  return(data.frame(
    test   = c(low_out[1], high_out[1]),
    result = c(low_out[2], high_out[2]),
    pass   = c(low_out[3], high_out[3])
  ))
}

hypothesis_test <- function(y, x, method = "pearson",
                            significance_level = 0.05) {
  confidence_level <- 1 - significance_level
  strong_threshold <- 0.75 # TODO: Figure out threshold

  ## Mean of residuals check
  c_test <- cor.test(y, x, method = method,
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
save_correlation_stats <- function(f_lin_model, output_dir) {
  mean_residual_thresh <- 0.005 # TODO: Why this threshold?

  mean_of_resid_test <- mean_of_residuals_test(f_lin_model$.resid,
                                               mean_residual_thresh)
  hypo_test <- hypothesis_test(f_lin_model$y, f_lin_model$x)
  shapiro_wilk_test <- shapiro_wilk_test(f_lin_model$.resid, "residuals")
  # TODO: Should we shapiro test even more?

  #TODO: want to send the response and terms var names through func or is ok?
  data_out_X_test <- outlier_test("x", f_lin_model$x)
  data_out_y_test <- outlier_test("y", f_lin_model$y)
  
  results <- bind_rows(mean_of_resid_test, hypo_test, 
                       shapiro_wilk_test, data_out_X_test , data_out_y_test)

  # TODO: Independence X var and residuals check

  ## Write the results to a CSV file
  filename <- paste(output_dir, "stat_tests.csv", sep = "/", collapse = "/")
  write.csv(results, filename, row.names = FALSE)

  # TODO: add summary output to (a) csv file
  #summary <- summary(lin_model)
}
