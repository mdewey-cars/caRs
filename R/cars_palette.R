#' A Complete list of the palettes available to the cars_palette function
#'
#' This draws heavy inspiration from the wonderful `wesanderson` library,
#' down to this structure to hopefully allow for easy color palette creation
#' in some preset themes
#'
#'
#' use \code{\link{\cars_palette}} to construct the palettes of desired length a given palette
#'
cars_palettes <- list(
  cars_greyscale = c('white' = '#fff', 'lighter_grey' = '#e6e6e6', 'light_grey' = '#bdbdbd', 'dark_grey' = '#767676', 'black' = '#212121'),
  cars = c('brand' = '#532380', 'accent' = '#00bfde'),

)




#' A CARS themed color palette function
#'
#' @description This function provides a way to quickly generate a color palette
#' which is in the cars color scheme. It draws inspiration from the `wesanderson`
#' library and it's ability to dynamically define palettes from themes
#' @param business_area Accepts a few different arguments based upon the business area.
#' Defaults to cars
#'
#' @return a vector with hex codes of colors
#'

cars_palette <- function(business_area = c('Cars', 'DI')) {}
