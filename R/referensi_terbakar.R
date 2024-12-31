#' Upload Reference Shapefile Data
#'
#' @param shp_path Character string path to shapefile
#' @param crs_proj Projection string (optional)
#' @return SpatialPolygonsDataFrame object
#' @importFrom rgdal readOGR
#' @importFrom sp spTransform CRS
#' @export
upload_reference_shp <- function(shp_path, crs_proj = NULL) {

  # Validasi input path
  if(!file.exists(shp_path)) {
    stop("Shapefile path does not exist")
  }

  # Baca shapefile
  tryCatch({
    reference_shp <- rgdal::readOGR(shp_path)
  }, error = function(e) {
    stop("Error reading shapefile: ", e$message)
  })

  # Transform CRS jika diperlukan
  if(!is.null(crs_proj)) {
    reference_shp <- sp::spTransform(reference_shp,
                                     sp::CRS(crs_proj))
  }

  # Validasi atribut
  required_fields <- c("burn_sev", "area_ha")
  missing_fields <- required_fields[!required_fields %in% names(reference_shp)]

  if(length(missing_fields) > 0) {
    warning("Missing required fields: ",
            paste(missing_fields, collapse=", "))
  }

  # Print summary
  cat("Reference shapefile loaded:\n")
  cat("Number of features:", length(reference_shp), "\n")
  cat("Projection:", sp::proj4string(reference_shp), "\n")
  cat("Attributes:", paste(names(reference_shp), collapse=", "), "\n")

  return(reference_shp)
}

# Contoh penggunaan:
#' \dontrun{
#' # Load reference shapefile
#' ref_shp <- upload_reference_shp(
#'   shp_path = "path/to/reference.shp",
#'   crs_proj = "+proj=utm +zone=48s +datum=WGS84"
#' )
#'
#' # Konversi ke raster untuk validasi
#' ref_raster <- raster::rasterize(
#'   ref_shp,
#'   raster_template,
#'   field = "burn_sev"
#' )
#' }
