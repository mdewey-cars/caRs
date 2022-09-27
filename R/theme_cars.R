#' A Cars Theme to ggplot2 Functions
#'
#' @description This function provides a wrapper around some ggplot2 theme
#' arguments and creates a unified CARS graphing theme, along with
#' other color functions provided in caRs
#' @param transparent_background A boolean for whether or not to include
#' a transparent background. Set to false if sharing a .png file
#'
#' @return a gglot theme
#' @export
#'
#' @importFrom ggplot2 '%+replace%'
theme_cars <- function(transparent_background = T) {
  # For ease of declaration, we will establish some common fonts here, cribbed from
  # the website's CSS
  # cars_font <- ""
  plot_title_text_size <- 20
  title_text_size <- 16
  regular_text_size <- 14
  base_theme <- ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        # This is the "lighter grey" color from cars
        color = '#e6e6e6',
        linetype = 'solid',
        lineend = 'square',
        size = 0.8
      ),
      plot.title = ggplot2::element_text(
        #family = cars_font,
        size = plot_title_text_size,
        face = 'bold',
        hjust = 0,
        vjust = 2
      ),
      plot.subtitle = ggplot2::element_text(
        #family = cars_font,
        size = regular_text_size,
        face = 'italic'
      ),
      axis.title = ggplot2::element_text(
        #family = cars_font,
        size = title_text_size
      ),
      axis.text = ggplot2::element_text(
        #family = cars_font,
        size = regular_text_size
      ),
      legend.title = ggplot2::element_text(
        #family = cars_font,
        size = title_text_size
      ),
      legend.text = ggplot2::element_text(
        #family = cars_font,
        size = regular_text_size
      ),
      panel.border = ggplot2::element_rect(
        fill = NA,
        color = NA)
    )

  if (transparent_background) {
    theme_minimal() %+replace%
      base_theme
  } else {
    theme_bw() %+replace%
      base_theme
  }
}
