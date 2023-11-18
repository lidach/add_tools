#' @title theme_lc
#' 
#' @description custom theme for ggplot (based on ggsidekik from S. Andersen)
#' 
#' @param base_size base size
#' @param base_family base family
#' 
#' @importFrom ggplot2 element_text element_rect element_blank theme rel theme_light
#' @importFrom grid unit
#' 
#' @export
#' 

theme_lc <- function(base_size = 12,
                     base_family = "") {
  half_line <- base_size / 2
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(family = base_family, size = base_size),
      plot.margin = unit(c(1, 1, .5, .7), "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "grey30"),
      strip.text.y = element_text(colour = "grey30"),
      axis.text = element_text(colour = "grey30"),
      axis.title = element_text(colour = "grey30"),
      legend.title = element_text(colour = "grey30", size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.7), colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = "grey30", size = rel(1)),
      plot.subtitle = element_text(colour = "grey30", size = rel(.85)) 
    )
}