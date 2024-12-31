#' Export Burn Area Results
#'
#' @param burn_raster Burn area raster
#' @param filename Output filename
#' @param format Output format ("GTiff", "ASCII", "ESRI Shapefile")
#' @importFrom raster writeRaster rasterToPolygons
#' @importFrom rgdal writeOGR
#' @export
export_burns <- function(burn_raster, filename, format = "GTiff") {
  switch(format,
         "GTiff" = raster::writeRaster(burn_raster, filename, format="GTiff"),
         "ASCII" = raster::writeRaster(burn_raster, filename, format="ASCII"),
         "ESRI Shapefile" = {
           burn_poly <- raster::rasterToPolygons(burn_raster)
           rgdal::writeOGR(burn_poly, dirname(filename),
                           basename(filename), driver="ESRI Shapefile")
         }
  )
}
