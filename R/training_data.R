#' Generate Training Data from Polygon Shapefile
#'
#' @param nbr_pre Raster NBR pre-fire
#' @param nbr_post Raster NBR post-fire
#' @param dnbr Raster dNBR
#' @param training_poly SpatialPolygonsDataFrame berisi area training
#' @return List berisi training data pre, post fire dan dNBR
#' @importFrom raster extract
#' @export
generate_training_data <- function(nbr_pre, nbr_post, dnbr, training_poly) {

  # Validasi input
  if(!methods::is(nbr_pre, "RasterLayer") ||
     !methods::is(nbr_post, "RasterLayer") ||
     !methods::is(dnbr, "RasterLayer")) {
    stop("Input raster harus berupa RasterLayer")
  }

  if(!methods::is(training_poly, "SpatialPolygonsDataFrame")) {
    stop("training_poly harus berupa SpatialPolygonsDataFrame")
  }

  # Extract nilai dari polygon training
  pre_samples <- raster::extract(nbr_pre, training_poly)
  post_samples <- raster::extract(nbr_post, training_poly)
  delta_samples <- raster::extract(dnbr, training_poly)

  # Flatten list hasil extract
  pre_samples <- unlist(pre_samples)
  post_samples <- unlist(post_samples)
  delta_samples <- unlist(delta_samples)

  # Hapus NA dan Inf
  valid_idx <- which(!is.na(pre_samples) & !is.na(post_samples) & !is.na(delta_samples) &
                       is.finite(pre_samples) & is.finite(post_samples) & is.finite(delta_samples))

  pre_samples <- pre_samples[valid_idx]
  post_samples <- post_samples[valid_idx]
  delta_samples <- delta_samples[valid_idx]

  if(length(valid_idx) == 0) {
    stop("Tidak ada data valid dalam polygon training")
  }

  # Hitung statistik
  pre_stats <- list(
    mean = mean(pre_samples),
    sd = sd(pre_samples)
  )

  post_stats <- list(
    mean = mean(post_samples),
    sd = sd(post_samples)
  )

  delta_stats <- list(
    mean = mean(delta_samples),
    sd = sd(delta_samples)
  )

  # Hitung threshold
  nbr_threshold <- post_stats$mean + (2 * post_stats$sd)
  dnbr_threshold <- delta_stats$mean - (2 * delta_stats$sd)

  return(list(
    pre = pre_samples,
    post = post_samples,
    delta = delta_samples,
    pre_stats = pre_stats,
    post_stats = post_stats,
    delta_stats = delta_stats,
    thresholds = list(
      nbr = nbr_threshold,
      dnbr = dnbr_threshold
    )
  ))
}
