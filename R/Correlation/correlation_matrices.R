source("Correlation/correlation_pipeline.R")
pacman::p_load(rnaturalearth, stringr, sf, reshape)

#' Returns a character vector containing column names based on index values
#'
#' Expects `col_names` where each name is sequentially repeated once (e.g: c("col1", "col1", "col2", "col2"...))
#' and aims to specify the discrepancy between these two columns using `even_char` and `odd_char`
#'
#' @param col_names character vector, vector containing the column names
#' @param even_char character, name to append to `col_names` at even indexes
#' @param odd_char character, name to append to `col_names` at odd indexes
#' @returns character vector
append_char_even_odd <- function(col_names, even_char, odd_char){
  return_vec <- c()
  for (i in 1:length(col_names)){
    return_vec <- append(return_vec, paste(col_names[i], even_char))
    return_vec <- append(return_vec, paste(col_names[i], odd_char))
  }
  return(return_vec)
}

#' Generate a color based on `p_val`
#' 
#' @param `p_val` float
#'@returns character, STRONG -> "GREEN", MODERATE -> "YELLOW", WEAK -> "ORANGE", or INSUFFICIENT -> "RED"
get_color_based_on_p_val <- function(p_val){

  if (p_val < 0.001) return("green")
  if (p_val< 0.05) return("yellow")
  if (p_val < 0.1) return("orange")
  return("red")
}

#' Transforms given `cor_res` into a format ready to plot matrices from
#' 
#' Transforms cor_res to long format, renames the columns to specify "P-Value" vs "Correlation" (R2)
#' TODO: Need more comments
#'
#' @param cor_res list, a nested list containing correlation results (P-values and R2)
#' @returns dataframe, dataframe ready for matrix plotting
transform_cor_res_for_cor_matrices <- function(cor_res){
  #TODO: Some comments maybe
  m <- matrix(unlist(cor_res), ncol = length(cor_res[[1]]) * 2, byrow = TRUE)
  colnames(m) <- append_char_even_odd(names(cor_res[[1]]), "P-value", "Correlation")
  rownames(m) <- names(cor_res)
  
  ## Transform the matrix into a "long format"
  df <- melt(m)
  colnames(df) <- c("y", "x", "value")
  #transform the df (again)
  df <- df %>%
    mutate(type = word(x,-1)) %>% 
    mutate(x = word(x,1))
  
  unique_combos <- unique(df[c("x","y")])
  
  df_corr <- data.frame("p-value"  = df[df$type == "P-value", "value"],
                        "correlation" = df[df$type == "Correlation", "value"]
  )
  
  final_df <- cbind(unique_combos, df_corr) %>%
    mutate(conf = mapply(get_color_based_on_p_val,p.value))
  
  return (final_df)
}

#' Generates two correlation matrices
#'
#' @returns void
generate_cor_matrices <- function(){
  
  #TODO: figure out how to get y_render_name here
  y_var_name= "rkna"
  cor_res <- generate_cor_res(y_var_name)
  trans_res <- transform_cor_res_for_cor_matrices(cor_res)
  
  cor_matrix <- ggplot(trans_res, aes(x = , x, y = y, fill = correlation)) +
    labs(
      #title = "Correlation matrix - GDP vs. Variable",
      x = "Variable",
      #TODO: would be cleaner to use the `indep_name`
      y = y_var_name
    ) +
    geom_tile(show.legend = FALSE) +
    geom_text(aes(x, y, label = correlation),
              color = ifelse(trans_res$correlation > 0.5, "white", "black"), size = 5) +
    scale_fill_distiller(palette = 2, direction = 1) +
    #presentation_theme +
    theme(axis.text.x = element_text(angle = 335, hjust = 0,
                                     margin = margin(3, 0, 0, 0)))
  #cor_matrix
  #save_plots("Correlation/Output/cor_matrix.png", "", cor_matrix)
  
  ## Matrix including P-values
  #TODO: Does not work as desired yet, colors should be discrete instead of continous scale (including legend)
  cor_matrix_conf  <- ggplot(trans_res, aes(x = , x, y = y, fill = p.value)) +
    labs(
      #title = "Correlation matrix - GDP vs. Variable",
      x = "Variable",
      #TODO: would be cleaner to use the `indep_name`
      y = y_var_name
    ) +
    geom_tile(show.legend = TRUE) +
    geom_text(aes(x, y, label = round(correlation, 3)),
              # color = ifelse(trans_res$p.value < 0.0000000000000000005, "white", "black"),
              color = "black",
              size = 5) +
    scale_fill_distiller(palette = 2, direction = 1) +
    #scale_color_identity() +
    #scale_color_manual(values=trans_res$conf) +
    #presentation_theme +
    theme(axis.text.x = element_text(angle = 335, hjust = 0,
                                     margin = margin(3, 0, 0, 0)))
  #cor_matrix_conf
  #TODO: Save (not here but after this code gets ran)
  #save_plots("Correlation/Output/cor_matrix_conf.png", "", cor_matrix_conf)
  
  #TODO: return both plots
  return (cor_matrix)
}

plot <- generate_cor_matrices()
plot