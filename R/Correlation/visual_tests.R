source("common.R")
source("./Correlation/plot_generator.R")
source("./Correlation/plot_themes.R")
source("./Correlation/auto_correlation_testing.R")

#' Generate linear correlation test (assumptions) plots and save them to a disk
#' 
#' @param data_scope_str character, metadata about scope of `data`
#' @param lin_model list, linear model to generate plots from
#' @param y_render_name character, name to use when referring to the dependent data
#' @param x_render_name character, name to use when referring to the independent data
#' @param output_dir logical = TRUE, whether to add references to each subplot using the alphabet (e.g "A","B", "C" ... etc as annotations for each plot)
#' @param sw_stat TODO
#' @param sw_p TODO
#' @param s TODO
#' @param r_squared TODO
#' @param ac_df the dataframe to be used for performing the auto correlation calculations
#' @returns ggplot2:ggplot | None, the plot created after merging or none if `plot_list` does not contain enough plots to merge
save_correlation_plots <- function(data_scope_str, lin_model, y_render_name, x_render_name,
                                   output_dir, sw_stat, sw_p, s, r_squared, ac_df) {
  
  f_lin_model <- fortify(lin_model)
  colnames(f_lin_model)[1] <- "y"
  colnames(f_lin_model)[2] <- "x"
  
  ## Define themes used
  presentation_theme <- get_presentation_theme()
  doc_theme <- get_doc_theme()
  
  title <- paste(y_render_name, "~", x_render_name)

  ## Generate plots
  ## Linearity plots
  regres_p <- gen_regression_plot(f_lin_model, x_render_name, y_render_name, presentation_theme, s, r_squared)
  resid_fitted_p <- gen_resid_fitted_plot(f_lin_model, presentation_theme)

  ## Homoscedasticity plot (+ resid_fitted_p)
  scale_loc_p <- gen_scale_loc_plot(f_lin_model, presentation_theme)
  
  ## Normality plots
  hist_p <- gen_histogram_plot(f_lin_model, presentation_theme)
  box_p <- gen_box_plot(f_lin_model, presentation_theme)
  box_p_X <- gen_box_plot(f_lin_model, presentation_theme, paste(x_render_name, "boxplot"), x_render_name)
  box_p_y <- gen_box_plot(f_lin_model, presentation_theme, paste(y_render_name, "boxplot"), y_render_name)
  #FIXME: ew, inefficient, but who cares
  sw_test <- shapiro.test(f_lin_model$.resid)
  qq_p <- gen_qq_plot(f_lin_model, presentation_theme, sw_stat, sw_p)
  
  ##auto correlation plots
  ac_plot <- auto_correlation_plot(ac_df)
  
  ## Merge plots together
  scope_str_quotes <- paste("\"", data_scope_str, "\"", sep = "")
  combined_plots <- combine_plots(list(regres_p, resid_fitted_p, qq_p, scale_loc_p),
                                  title=paste("Scope:", scope_str_quotes, "(Combination of plots)"),
                                  theme = doc_theme)
  
  normality_plots <- combine_plots(list(qq_p, hist_p, box_p),
                                   title=paste("Scope:", scope_str_quotes, "(Normality plots)"),
                                   sub_title = paste("Linear model:",y_render_name, "~", x_render_name),
                                   theme = doc_theme)
  
  linearity_plots <- combine_plots(list(regres_p, resid_fitted_p),
                                   title=paste("Scope:", scope_str_quotes, "(Linearity plots)"),
                                   sub_title = paste("Linear model:",y_render_name, "~", x_render_name),
                                   theme = doc_theme)
  
  homoscedasticity_plots <- combine_plots(list(resid_fitted_p, scale_loc_p),
                                          title=paste("Scope:", scope_str_quotes, "(Homoscedasticity plots)"),
                                          sub_title = paste("Linear model:",y_render_name, "~", x_render_name),
                                          theme = doc_theme)
  
  X_y_box_plots <- combine_plots(list(box_p_X, box_p_y),
                                  title=paste("Scope:", scope_str_quotes, "(Box plots)"),
                                  theme = doc_theme)
  
  ## Save plots to disk
  save_plot(
    path = paste(output_dir, "uberplot.png", sep = "/", collapse = "/"),
    combined_plots
  )
  
  save_plot(
    path = paste(output_dir, "normality_plots.png", sep = "/", collapse = "/"),
    normality_plots,
    w = 13
  )
  
  save_plot(
    path = paste(output_dir, "linearity_plots.png", sep = "/", collapse = "/"),
    linearity_plots
  )
  
  save_plot(
    path = paste(output_dir, "homoscedasticity_plots.png", sep = "/", collapse = "/"),
    homoscedasticity_plots
  )
  
  save_plot(
    path = paste(output_dir, "data_box_plots.png", sep = "/", collapse = "/"),
    X_y_box_plots
  )
  
  save_plot(
    path = paste(output_dir, "auto_correlation_plots.png", sep = "/", collapse = "/"),
    ac_plot
  )
  
  # save_plot(
  #   path = paste(output_dir, "histogram.png", sep = "/", collapse = "/"),
  #   hist_p
  # )
  # save_plot(
  #   path = paste(output_dir, "boxplot.png", sep = "/", collapse = "/"),
  #   box_p
  # )
  # save_plot(
  #  path = paste(output_dir, "qq.png", sep = "/", collapse = "/"),
  #  title = paste("Quantile-Quantile plot:", title),
  #  qq_p
  # )
}