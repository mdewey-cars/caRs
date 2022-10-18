#' A Complete list of the palettes available to the cars_palette function
#'
#' This draws heavy inspiration from the wonderful `wesanderson` library,
#' down to this structure to hopefully allow for easy color palette creation
#' in some preset themes
#'
#'
#' use \code{\link{\cars_palette}} to construct the palettes of desired length a given palette
#'
#' @export
cars_palettes <- list(
  cars_greyscale = c('#ffffff', '#e6e6e6', '#bdbdbd', '#767676', '#212121'),
  cars = c('#532380', '#bb00cc', '#00bfde', '#eb9628', '#93a533', '#cc1100'),
  di = c('#000000', '#007cba', '#006ba1', '#005a87'),
  fuel = c('#cf2e2e', '#f78da7', '#ff6900', '#fcb900', '#7bdcb5', '#00d084', '#8ed1fc', '#0693e3', '#9b51e0'),
  ##TODO: Continue to flesh these out
  dealer_rater = c('#3e7397', '#552c87')
)




#' A CARS themed color palette function
#'
#' @description This function provides a way to quickly generate a color palette
#' which is in the cars color scheme. It draws inspiration from the `wesanderson`
#' library and it's ability to dynamically define palettes from themes
#'
#' @param selection A vector of numeric indices to pull out of the palette of interest
#' @param name Accepts a few different arguments based upon the business area.
#' Defaults to cars
#' @param type Either "discrete" or "continuous". Use continuous to automatically
#' interpolate between colors. Useful for heatmaps.
#' @param cont_n If continuous is selected, you must provide a number of colors
#' to request.
#'
#' @return a vector with hex codes of colors
#'

cars_palette <- function(selection,
                         name = 'cars',
                         type = c('discrete', 'continuous'),
                         cont_n = NULL) {
  type = match.arg(type)

  palette <- cars_palettes[[name]]
  if (is.null(palette)) {
    stop('Selected palette not found. Perhaps you misspelled the palette you\'re looking for?')
  }

  if (missing(selection)) {
    # I prefer names as the cannonical method of selection. But that's subject to change as I change the API
    selection <- names(palette)
  }

  if (type == 'discrete' && length(selection) > length(palette)) {
    stop("Requested number of colors is larger than the palette size.")
    print(str_c("The palette you requested has ", length(palette), " colors available"))
  }

  if (type == 'continuous' && is.null(cont_n)) {
    stop('You must provide a count with cont_n when the palette is continuous')
  }
  out <- switch(type,
                continuous = grDevices::colorRampPalette(palette[selection])(cont_n),
                discrete = palette[selection])
  structure(out, class = 'palette', name = name)
}
