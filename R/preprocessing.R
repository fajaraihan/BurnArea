#' Preprocessing Data for NBR Analysis
#'
#' @param nir_band Near-infrared band raster
#' @param swir_band Short-wave infrared band raster
#' @param mask Optional mask raster
#' @return Preprocessed raster stack
#' @importFrom raster stack mask writeRaster
#' @importFrom methods is
#' @export
preprocess_nbr <- function(nir_band, swir_band, mask = NULL) {
  # Hapus require(raster) karena sudah menggunakan namespace eksplisit

  # Check input validity menggunakan methods::is
  if(!methods::is(nir_band, "RasterLayer") || !methods::is(swir_band, "RasterLayer")) {
    stop("Input bands must be RasterLayer objects")
  }

  # Radiometric correction dengan namespace eksplisit
  # Asumsikan radiometric_correction adalah fungsi dari package yang sama
  nir_corr <- radiometric_correction(nir_band)
  swir_corr <- radiometric_correction(swir_band)

  # Apply mask if provided menggunakan raster::mask
  if(!is.null(mask)) {
    nir_corr <- raster::mask(nir_corr, mask)
    swir_corr <- raster::mask(swir_corr, mask)
  }

  # Menggunakan raster::stack untuk mengembalikan hasil
  return(raster::stack(nir_corr, swir_corr))
}
