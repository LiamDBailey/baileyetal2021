#' Personal theme for making plots
#'
#' @param base_size Base font size
#' @param base_family Base font family
#' @param base_line_size Standard line size
#' @param base_rect_size Standard rectangle size
#' @param legend Default value for theme(legend.position)
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' library(ggplot2)
#'
#' #Don't use Ubuntu font because it may not be loaded
#' ggplot() +
#' geom_point(data = mtcars, aes(x = disp, y = mpg)) +
#' theme_ubuntu(base_family = "sans")
theme_ubuntu <- function(base_size = 12, base_family = "Ubuntu",
                         base_line_size = base_size / 20,
                         base_rect_size = base_size / 20,
                         legend = "none") {
  half_line <- base_size / 2

  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others. These set the defaults for line, rectangle and text elements.
    line =               element_line(
      colour = "black", size = base_line_size,
      linetype = 1, lineend = "round"
    ),
    rect =               element_rect(
      fill = "white", colour = "black",
      size = base_rect_size, linetype = 1
    ),
    text =               element_text(
      family = base_family, face = "plain",
      colour = "black", size = base_size,
      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = margin(), debug = FALSE
    ),

    axis.line =          element_line(lineend = "round", colour = "black"),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(size = rel(0.8), colour = "black", face = "bold"),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks =         element_line(colour = "black", lineend = "round", size = ),
    axis.ticks.length =  unit(half_line / 2, "pt"),
    axis.title.x =       element_text(
      margin = margin(t = half_line * 1.5),
      vjust = 1,
      face = "bold"
    ),
    axis.title.x.top =   element_text(
      margin = margin(b = half_line),
      vjust = 0,
      face = "bold"
    ),
    axis.title.y =       element_text(
      angle = 90,
      margin = margin(r = half_line * 1.5),
      vjust = 0,
      face = "bold"
    ),
    axis.title.y.right = element_text(
      angle = -90,
      margin = margin(l = half_line),
      vjust = 0,
      face = "bold"
    ),

    legend.background = element_rect(colour = NA),
    legend.spacing = unit(2 * half_line, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = margin(half_line, half_line, half_line, half_line),
    legend.key = element_rect(fill = NA, colour = "white"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.position = legend,
    legend.text = element_text(family = base_family, size = rel(0.8)),

    panel.background =   element_rect(fill = "white", colour = NA),
    panel.border =       element_blank(),
    panel.grid.major =   element_blank(),
    panel.grid.minor =   element_blank(),
    panel.spacing =      unit(half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_rect(fill = NA, colour = "black"),
    strip.text =         element_text(
      colour = "black",
      size = rel(0.8),
      margin = margin(half_line, half_line, half_line, half_line)
    ),
    strip.text.x =       NULL,
    strip.text.y =       element_text(angle = -90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),

    plot.background =    element_rect(colour = "white"),
    plot.title =         element_text(
      size = rel(1.2),
      hjust = 0.5, vjust = 1,
      margin = margin(b = half_line * 1.2)
    ),
    plot.subtitle =      element_text(
      size = rel(0.9),
      hjust = 0.5, vjust = 1,
      margin = margin(b = half_line * 0.9)
    ),
    plot.caption =       element_text(
      size = rel(0.9),
      hjust = 0.5, vjust = 1,
      margin = margin(t = half_line * 0.9)
    ),
    plot.margin =        margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )
}
