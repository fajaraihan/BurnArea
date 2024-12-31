#' Calculate Delta Normalized Burn Ratio (dNBR)
#'
#' This function calculates the difference between pre-fire NBR and post-fire NBR
#' to assess burn severity.
#'
#' @param nbr_pre Raster object of pre-fire NBR
#' @param nbr_post Raster object of post-fire NBR
#' @return Raster object of dNBR values or classified burn severity
#' @export
calculate_dnbr <- function(nbr_pre, nbr_post) {
  # Check if inputs are raster objects
  if (!inherits(nbr_pre, "Raster") || !inherits(nbr_post, "Raster")) {
    stop("Both nbr_pre and nbr_post must be Raster objects")
  }

  # Check if the extents match
  if (!identical(raster::extent(nbr_pre), raster::extent(nbr_post))) {
    stop("The extents of pre and post NBR rasters must match")
  }

  # Calculate dNBR
  dnbr <- nbr_pre - nbr_post

  return(dnbr)
}
