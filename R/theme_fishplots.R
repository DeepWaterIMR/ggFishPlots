#' @title A ggplot2 theme for the ggFishPlots package
#' @param ... additional arguments passed to \code{\link[ggplot2]{ggtheme}}.
#' @import ggplot2
#' @return A ggplot2 theme layer.
#' @family customize shapefiles
#' @export

theme_fishplots <- function(...) {
      theme_classic(...) %+replace%
            theme(
                  strip.background = element_blank(),
                  panel.background = element_blank(),
                  plot.background = element_blank(),
                  legend.background = element_blank(),
                  legend.box.background = element_blank(),
                  plot.margin = margin(5.5, 10, 5.5, 5.5) # perhaps not needed?
            )
}
