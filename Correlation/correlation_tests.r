source("./Correlation/utils.r", chdir = TRUE)
pacman::p_load(dplyr, stringr, ggplot2, rnaturalearth, rnaturalearthdata, broom, ggfortify, patchwork)#TODO: Sort out imports

START_YEAR <- 1990
END_YEAR <- 2019
OUTPUT_DEST <- "./Correlation/Output"
TITLE <- "example"
OUT_DIR <- paste(OUTPUT_DEST, TITLE, sep = "/", collapse = "/")

## Import and clean PWT
pwt <- pwt() %>%
  filter(year >= START_YEAR & year <= END_YEAR) %>%
  filter(!is.na(rgdpna) & !is.na(pop) & !is.na(avh) & !is.na(emp)) %>%
  mutate(rgdpna = (rgdpna / pop)) %>%
  mutate(avh_by_pop = (avh * emp) / pop)

nw <- ne_countries(scale = "medium", returnclass = "sf") %>%
  rename("countrycode" = "iso_a3") %>%
  filter(admin != "Antarctica") %>%
  mutate(economy = str_replace(economy, "^.{0,3}", "")) %>%
  select(c("countrycode", "economy"))

joined_data <- merge(nw, pwt)

#function for plotting whatever cor test desired
plot_cor_test <- function(dataframe, title, xlab, ylab, geom_point=FALSE, zero_line = FALSE, abline=FALSE, smooth=FALSE, stat_smooth = FALSE){
  plot <-  ggplot(dataframe, aes(dataframe[,1], dataframe[,2])) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(title) +
    theme_bw()
  if (geom_point) plot <- plot +
      geom_point()
  if (abline) plot <- plot +
      #geom_abline()
      geom_smooth(method = "lm", se = FALSE)
  if (smooth) plot <- plot +
      geom_smooth(se = FALSE)
  if (stat_smooth) plot <- plot +
      stat_smooth(method = "loess", span = 0.1, colour = I("red"), se = FALSE) #TODO: Want to change these through optional func parameters? -> dict with values, unpacking ideal
  if (zero_line ) plot <- plot +
      geom_hline(yintercept=0)
  return(plot)
}

mean_of_residuals_check <- function(rdl_vector, res_thresh){
  passed_test <- FALSE
  res_mean <- mean(rdl_vector)
  if (res_mean <= res_thresh) passed_test <- TRUE
  return(c("Mean of residuals close to 0", res_mean, passed_test))
}

save_two_plots <- function(p1, p2, main, filename){
  plots <- p1 + p2
  plots <- plots +
    plot_annotation(
      title = main,
      tag_levels = "A",
      theme = theme(plot.title = element_text(face = 'bold'))
    )
  ggsave(filename, plots, width=2000, height=800, units="px")  
}

visual_corr_check <- function(fortified_model, x_lab, y_lab, title, output_dir){
  #Linearity plots
  regres_p <- plot_cor_test(
    dataframe = data.frame(fortified_model$x_values, fortified_model$y_values), #TODO: Subset?
    title = paste(x_lab, "vs", y_lab),
    xlab = x_lab,
    ylab = y_lab,
    geom_point = TRUE,
    abline =  TRUE
  )
  
  resid_fitted_p <- plot_cor_test(
    dataframe = data.frame(fortified_model$.fitted, fortified_model$.resid), #TODO: Subset?
    title = "Residuals vs fitted values",
    xlab = "Fitted values",
    ylab = "Residuals",
    geom_point=TRUE,
    zero_line  = TRUE,
    smooth=TRUE
  )
  save_two_plots(
    p1 = regres_p, 
    p2 = resid_fitted_p, 
    main = paste("Linearity check:", title), 
    filename = paste(output_dir, "linearity_check.png", sep = "/", collapse = "/")
    )
  
  #Homoscedasticity plot (+ resid_fitted_p)
  scale_loc_p <- plot_cor_test(
    dataframe = data.frame(fortified_model$.fitted, sqrt(abs(fortified_model$.stdresid))),
    title = "Scale location",
    xlab = "Fitted values",
    ylab = "sqrt standardized residuals",
    geom_point = TRUE,
    smooth=TRUE
    )
  save_two_plots(
    p1 = resid_fitted_p, 
    p2 = scale_loc_p, 
    main = paste("Homoscedasticity check: ", title), 
    filename = paste(output_dir, "homoscedasticity_check.png", sep="/", collapse = "/")
  )
  
  #Normality plots
  qq_p <- plot_cor_test(
    dataframe = data.frame(qqnorm(fortified_model$.stdresid)[[1]], fortified_model$.stdresid), 
    title = paste("Q-Q Plot:", title),
    xlab = "Theoretical quantiles",
    ylab = "Standardized residuals",
    geom_point=TRUE,
    abline=TRUE
    )
  ggsave(paste(output_dir, "normality_check.png", sep="/",collapse = "/"), qq_p, width=1000, height=800, units="px") #TODO: bad design to save like this (no 3NF)
}

correlation_hypothesis_test <- function(x_values, y_values, method="pearson", conf_level = 0.95){
  cor_df <- data.frame(test=character(0), result= numeric(0), pass=logical(0))
  significance_value = 1 - conf_level
  #Mean of residuals check
  cor_test <- cor.test(x_values, y_values, method=method, conf.level=conf_level)
  p_val <- cor_test$p.value
  estimate <- cor_test$estimate
  cor_df[nrow(cor_df)+1,] <- c(paste("Hypothesis:", method, "estimated"), estimate, "NA")
  cor_df[nrow(cor_df)+1,] <- c("Hypothesis: significance value", significance_value, "NA")
  if (p_val < significance_value)
    cor_df[nrow(cor_df)+1,] <- c("Hypothesis: reject H1 through P-value", p_val, FALSE)
  else
    cor_df[nrow(cor_df)+1,] <- c("Hypothesis: reject H1 through P-value", p_val, TRUE)
  if (estimate >= 0.5 || estimate <= -0.5)
    cor_df[nrow(cor_df)+1,] <- c("Hypothesis: strong correlation", p_val, TRUE)
  else
    cor_df[nrow(cor_df)+1,] <- c("Hypothesis: strong correlation", p_val, FALSE)
  cor_df[nrow(cor_df)+1,] <- c("Hypothesis: confidence interval", paste(cor_test$conf.int[1],cor_test$conf.int[2]), "NA")
  return(cor_df)
}

stat_corr_check <- function(fort_lin_model, mean_residual_thresh = 0.005){
  stat_df <- data.frame(test=character(0), result= numeric(0), pass=logical(0))
  #Mean of residuals check
  stat_df[nrow(stat_df) + 1, ]  <- mean_of_residuals_check(fort_lin_model$.resid, mean_residual_thresh)
  #Hypothesis testing
  stat_df <- rbind(stat_df, correlation_hypothesis_test(
    fort_lin_model$x_values,
    fort_lin_model$y_values
    ) 
  )
  x <- c("test", "result", "pass")
  colnames(stat_df) <- x
  
  #TODO: Independence X var and residuals check
  #Save test csv results
  filename <- paste(OUT_DIR, "stat_tests.csv", sep = "/", collapse = "/")
  write.csv(stat_df, filename, row.names=FALSE)
  
  #TODO: add summary output to (a) csv file
  #summary <- summary(lin_model)
}

lin_model <- lm(joined_data$rgdpna ~ joined_data$avh_by_pop)
lin_model_df <- fortify(lin_model)
#standardize x and y column names
colnames(lin_model_df)[1] = "x_values"
colnames(lin_model_df)[2] = "y_values"

#Generate output dir if necessary
if (!dir.exists(OUTPUT_DEST)) {dir.create(OUTPUT_DEST)}
if (!dir.exists(OUT_DIR)) {dir.create(OUT_DIR)}
visual_corr_check(
  fortified_model = lin_model_df,
  x_lab = "avh by pop",
  y_lab = "rgdpna",
  title = "RGDPNA ~ AVH by pop",
  output_dir = OUT_DIR
  )
stat_corr_check(lin_model_df, TITLE)