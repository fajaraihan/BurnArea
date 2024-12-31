#' Convert shp reference to reference raster
#'
#' @param predicted_raster Raster of predicted burn area (binary: 1 for burned, 0 for unburned)
#' @param reference_shp Shapefile of reference burn area (polygon)
#' @return List containing omission error, commission error, and ICSI value
#' @importFrom raster rasterize
#' @export
shp_to_raster_reference <- function(reference_shp, predicted_raster) {
  # Convert polygon shapefile reference to raster format
  reference_raster <- raster::rasterize(
    reference_shp,               # Input shapefile
    predicted_raster,           # Template raster
    field = 1,                  # Assign value 1 to polygons
    background = 0,             # Areas outside polygons get 0
    touches = TRUE              # Include cells touched by polygons
  )

  return(reference_raster)
}
