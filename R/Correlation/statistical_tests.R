source("./Correlation/stat_generator.R")

#' Aggregate correlation test results and if given an output_dir save to a CSV
#' 
#' @param lin_model list, linear model using one x variable and one y variable #TODO: Find out what type lin_model actually is
#' @param output_dir  character, the absolute output path to save CSV file. If CSV should not be saved make empty
#' @returns dataframe, dataframe with 3 character columns: "Test", "Result" and "Pass"
gen_corr_stats <- function(lin_model, output_dir) {
  df_lm_sum <- dataframe_from_lm_sum(summary(lin_model))
  
  f_lin_model <- fortify(lin_model)
  colnames(f_lin_model)[1] <- "y"
  colnames(f_lin_model)[2] <- "x"
  
  
  mean_of_resid_test <- mean_of_residuals_test(f_lin_model$.resid)
  hypo_test <- hypothesis_test(f_lin_model$y, f_lin_model$x)
  shapiro_wilk_test <- shapiro_wilk_test(f_lin_model$.resid, "residuals")
  # TODO: Should we shapiro test even more?

  #TODO: want to send the response and terms var names through func or is ok?
  data_out_X_test <- outlier_test("x", f_lin_model$x)
  data_out_y_test <- outlier_test("y", f_lin_model$y)
  
  standard_error_test <- standard_error_test(f_lin_model)
  
  results <- bind_rows(df_lm_sum, mean_of_resid_test, hypo_test, 
                       shapiro_wilk_test, data_out_X_test , data_out_y_test, standard_error_test)

  
  # TODO: Independence X var and residuals check

  #TODO: maybe want to save some output in different files (e.g: lm info vs assumption checks)
  if(nchar(output_dir) > 0){
    ## Write the results to a CSV file
    filename <- paste(output_dir, "stat_tests.csv", sep = "/", collapse = "/")
    write.csv(results, filename, row.names = FALSE)
  }
  return (results)
}


get_stat_result_by_testname <- function(stat_result, testname, round=FALSE){
  extracted_result <- stat_result %>%
    filter(test == testname) %>%
    select(result) %>%
    as.numeric() 
  if (round)
    extracted_result <- extracted_result %>%
      round(2)
  return(extracted_result)
}














