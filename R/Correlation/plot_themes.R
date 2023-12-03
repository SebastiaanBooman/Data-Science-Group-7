pacman::p_load(ggplot2)

#' Creates a ggplot `theme`
#' 
#' @returns ggplot2:theme, the created theme
get_presentation_theme <- function(){
  return(
    theme(
      line = element_line(color = "white", linewidth = .5, linetype = 1,
                          lineend = "butt"),
      rect = element_rect(fill = NA, color = NA, linewidth = .5, linetype = 1),
      text = element_text(color = "lightgray"),
      
      axis.text.x = element_text(color = "white"),
      axis.text.y = element_text(color = "white"),
      
      axis.ticks = element_line(color = "white", linewidth = .5),
      
      axis.line   = element_line(color = "white", linewidth = .5,
                                 lineend = "square"),
      axis.line.x = NULL,
      axis.line.y = NULL,
      
      panel.background = element_blank(),
      panel.border     = element_blank(),
      panel.grid       = element_blank(),
      
      plot.background = element_blank()
    )
  )
}

#' Creates a ggplot `theme`
#' 
#' @returns ggplot2:theme, the created theme
get_doc_theme <- function(){
  return(  
    theme(
      line = element_line(color = "white", linewidth = .5, linetype = 1,
                          lineend = "butt"),
      rect = element_rect(fill = "black", color = NA, linewidth = .5, linetype = 1),
      text = element_text(color = "lightgray"),
      
      axis.text.x = element_text(color = "white"),
      axis.text.y = element_text(color = "white"),
      
      axis.ticks = element_line(color = "white", linewidth = .5),
      
      axis.line   = element_line(color = "white", linewidth = .5,
                                 lineend = "square"),
      axis.line.x = NULL,
      axis.line.y = NULL,
      
      plot.title = element_text(size=22),
      
      panel.border     = element_blank(),
      panel.grid       = element_blank(),
      
      #plot.background = element_blank())
    )
  )
}
