#' Calculate basic statistics
#' @noRd
calculate_stats <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]  # Remove NA and Inf values
  if (length(x) == 0) {
    stop("No valid data points found in the raster")
  }
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  return(list(mean = mean_val, sd = sd_val))
}

#' Classify Burn Severity Levels Based on Burned Area
#'
#' @param nbr_post Post-fire NBR raster
#' @param dnbr Delta NBR raster
#' @param training_data Training data for threshold calculation
#' @return Classified raster of burn severity
#' @export
classify_severity <- function(nbr_post, dnbr, training_data) {
  # Calculate thresholds
  nbr_stats <- calculate_stats(training_data$post)
  dnbr_stats <- calculate_stats(training_data$delta)

  alpha_nbr <- nbr_stats$mean + (2 * nbr_stats$sd)
  beta_nbr <- dnbr_stats$mean - (2 * dnbr_stats$sd)

  # Create binary mask (1 for burned, 0 for unburned)
  burn_mask <- (nbr_post <= alpha_nbr & dnbr >= beta_nbr)

  # Convert logical to numeric (TRUE/FALSE to 1/0)
  burn_mask <- raster::calc(burn_mask, fun = function(x) {
    ifelse(x == TRUE, 1, 0)
  })

  # Create mask only for value 1
  burn_mask_binary <- raster::calc(burn_mask, fun = function(x) {
    ifelse(x == 1, 1, NA)
  })

  # Apply mask to dnbr
  burn_area <- raster::mask(dnbr, burn_mask_binary)

  # Calculate severity thresholds
  thresh_low <- dnbr_stats$mean - (2 * dnbr_stats$sd)
  thresh_med <- dnbr_stats$mean - dnbr_stats$sd
  thresh_high <- dnbr_stats$mean

  # Klasifikasi hanya pada area terbakar
  severity_classified <- raster::calc(burn_area, function(x) {
    ifelse(is.na(x), NA,
           ifelse(x >= thresh_high, 3,
                  ifelse(x >= thresh_med, 2,
                         ifelse(x >= thresh_low, 1, NA))))
  })

  return(severity_classified)
}


