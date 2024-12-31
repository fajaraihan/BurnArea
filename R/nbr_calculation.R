#' Calculate Normalized Burn Ratio (NBR)
#'
#' @param preprocessed_stack Preprocessed raster stack
#' @return NBR raster
#' @importFrom raster stack subset
#' @export
calculate_nbr <- function(preprocessed_stack) {
  nir <- raster::subset(preprocessed_stack, 1)
  swir <- raster::subset(preprocessed_stack, 2)

  # Hitung NBR
  nbr <- (nir - swir) / (nir + swir)

  # Normalisasi hasil ke range -1 sampai 1
  nbr <- raster::calc(nbr, function(x) {
    x[x < -1] <- -1
    x[x > 1] <- 1
    return(x)
  })

  return(nbr)
}

