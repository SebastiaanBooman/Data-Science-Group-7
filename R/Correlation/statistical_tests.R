source("./Correlation/stat_generator.r")
#' Aggregate correlation test results and if specified save to a CSV
#' 
#' @param lin_model TODO
#' @param output_dir  TODO
#' @param save_res TODO
#' @returns dataframe, dataframe with 3 character columns: "Test", "Result" and "Pass"
gen_corr_stats <- function(lin_model, output_dir, save_res) {
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
  
  s <- s_test(f_lin_model)
  
  results <- bind_rows(df_lm_sum, mean_of_resid_test, hypo_test, 
                       shapiro_wilk_test, data_out_X_test , data_out_y_test, s)

  
  # TODO: Independence X var and residuals check

  #TODO: maybe want to save some output in different files (e.g: lm info vs assumption checks)
  if(save_res){
    ## Write the results to a CSV file
    filename <- paste(output_dir, "stat_tests.csv", sep = "/", collapse = "/")
    write.csv(results, filename, row.names = FALSE)
  }


  # TODO: add summary output to (a) csv file
  #summary <- summary(lin_model)
  
  return (results)
}
