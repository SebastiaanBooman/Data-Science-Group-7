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
    print(d)
    if (d > q3 * 1.5) high_out_amt <- high_out_amt+1
  if (high_out_amt > 0) no_high_out <- "FALSE"
  #Output 1
  low_out <- c(paste("Outlier test", var_name, ": IQR crit LOW amt"),
               low_out_amt,
               no_low_out
  )
  tail(sorted_range, n=1)
  subset <- sorted_range[12:18]
  for(d in sorted_range[0:18])
    print(d)
  #Output 2
  high_out <- c(paste("Outlier test", var_name, ": IQR crit HIGH amt"),
                low_out_amt,
                no_low_out
  )
  return(data.frame(
    test   = c(low_out[1], high_out[1]),
    result = c(low_out[2], high_out[2]),
    pass   = c(low_out[3], high_out[3])
  ))
}


data_range <- c(-45,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17, 400)
data_range_2 <- c(-45,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17, 400)

lin_m <- lm(data_range ~ data_range_2) 

length(data_range)
test <- outlier_test(data_range)










