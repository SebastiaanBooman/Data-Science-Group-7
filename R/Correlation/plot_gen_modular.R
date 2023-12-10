## Base Plot Builder class

library(ggplot2)
setOldClass(c("gg", "ggplot"))


setRefClass( 
  Class="BasePlotBuilder", 
  fields=list( 
    plot="ggplot" 
  ),
  methods = list(
    get_plot = function(){
      return (.self$plot)
    },
    instantiate_plot = function(dataframe){
      stop("No default implementation for BasePlotBuilder.instantiate_plot()")
    }
    add_peripherals_plot = function(){
      stop("No default implementation for BasePlotBuilder.add_peripherals_plot()")
    }
  ), 
  contains=c("VIRTUAL", "ggplot") 
)


setRefClass( 
  Class="OneDimPlotBuilder", 
  methods = list(
    instantiate_plot = function(dataframe){
      #TODO: Want to receive an arraylike object instead of dataframe
      .self$plot <- ggplot(dataframe, aes(dataframe[, 1]))
      }
  ),
  contains=c("BasePlotBuilder")  
)

setRefClass( 
  Class="TwoDimPlotBuilder", 
  methods = list(
    instantiate_plot = function(dataframe){
      .self$plot <- ggplot(dataframe, aes(dataframe[, 1], dataframe[, 2])) 
    }
  ),
  contains=c("BasePlotBuilder")  
)


## child 1
## Scale Location Plot Builder
setRefClass( 
  Class="ScaleLocationPlotBuilder", 
  add_peripherals_plot = function(){
    .self$plot <- .self$plot +
      labs(
        title = title,
        subtitle = subtitle,
        x = xlab,
        y = ylab
      ) +
      theme
  }
  contains=c("TwoDimPlotBuilder")
)

scale_loc_p <- plot_cor_test(
  dataframe = data.frame(f_lin_model$.fitted, sqrt(abs(f_lin_model$.stdresid))),
  title = "Scale-Location",
  xlab = "Fitted Values",
  ylab = expression(sqrt("Standardized Residuals")),
  geom_point = TRUE,
  smooth = TRUE,
  theme=presentation_theme
)



df <- data.frame(c(1,2,3,4,5,6), c(1,2,3,4,5,6))

a = new("ScaleLocationPlotBuilder")

a$instantiate_plot(df)
plot <- a$get_plot()
plot(plot)

## Plot Director
setRefClass( 
  Class="PlotDirector", 
  methods = list(
    construct_plot = function(plot_builder, dataframe){
      plot_builder$instantiate_plot(dataframe)
      plot_builder$add_peripherals_plot()
      return(plot_builder$get_plot())
    }
  )
  #contains=c("BasePlotBuilder")  
)
