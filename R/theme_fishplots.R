#' @title A ggplot2 theme for the ggFishPlots package
#' @param grid.col Character code specifying the color of grid lines. Use \code{NA} to remove the grid lines.
#' @param grid.size Numeric value specifying the width of grid lines.
#' @param ... additional arguments passed to \code{\link[ggplot2]{ggtheme}}.
#' @import ggplot2
#' @return A ggplot2 theme layer.
#' @family customize shapefiles
#' @export

theme_fishplots <- function(..., grid.col, grid.size) {
    theme_classic(...) %+replace%
    theme(strip.background = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          plot.margin = margin(c(5.5, 10, 5.5, 5.5)) # perhaps not needed?
    )
  }
