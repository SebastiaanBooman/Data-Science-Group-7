#' File contains functions for calculating various statistics about linear correlation
#' Each function returns a dataframe containing three character columns: "test", "result" and "pass"
#' Within test a small description of the respective test is defined
#' result contains the (numerical) result of the test
#' pass contains a logical or NA whether the test has been passed or not
#' NOTE: within the dataframe all values are cast to character

#TODO: Why use threshold of 0.005? (arbitrarily chosen)
#' TODO: Docstring
mean_of_residuals_test <- function(rdl_vector, res_thresh=0.005) {
  res_mean <- mean(rdl_vector)
  
  return(data.frame(
    test = c("Mean of residuals close to 0"),
    result = c(as.character(res_mean)),
    pass = c(as.character(res_mean <= res_thresh))
  )
  )
}

#' TODO: Docstring
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

#' TODO: Docstring
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

#' TODO: Docstring
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

#TODO: Want to extract more values?
#' Extracts the R squared, adjusted R squared and standard deviation (sigma) from given linear model summary
dataframe_from_lm_sum <- function(lm_sum){
  ## Output R-squared
  r_squared <- c("R-squared",
                 as.character(lm_sum$r.squared),
                 "NA")
  
  ## Output adjusted R-squared
  adj_r_squared <- c("Adjusted R-squared",
                     as.character(lm_sum$adj.r.squared),
                     "NA")
  
  ## Output standard deviation
  sigma <- c("Standard deviation",
             as.character(lm_sum$sigma),
             "NA")
  
  return(data.frame(
    test   = c(r_squared[1], adj_r_squared[1], sigma[1]),
    result = c(r_squared[2], adj_r_squared[2], sigma[2]),
    pass   = c(r_squared[3], adj_r_squared[3], sigma[3])
  ))
}

#TODO: Figure out what this test entails
s_test <- function(f_lin_model){
  s <- diag(vcov(lm(f_lin_model$y ~ f_lin_model$x)))
  ## Output s
  out_s <- c("s", as.character(s))
  
  return(data.frame(
    test   = c(out_s[1]),
    result = c(out_s[2]),
    pass   = c("NA")
  ))
}