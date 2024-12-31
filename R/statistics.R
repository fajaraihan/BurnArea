#' Calculate Burn Area Statistics
#'
#' @param severity_raster Raster of classified burn severity
#' @param training_data Training data for statistics calculation
#' @param pixel_size Pixel size in hectares (default = 0.09 for 30m Landsat resolution)
#' @return List containing burn area statistics
#' @export
calculate_burn_stats <- function(severity_raster, training_data, pixel_size = 0.09) {

  # Extract values
  severity_values <- raster::values(severity_raster)
  severity_values <- severity_values[!is.na(severity_values)]

  # Calculate statistics from training data
  stats <- calculate_stats(training_data$delta)

  # Define thresholds berdasarkan dokumen
  thresh_low <- stats$mean - 2 * stats$sd
  thresh_med <- stats$mean - stats$sd
  thresh_high <- stats$mean

  # Count pixels for each severity class
  low_pixels <- sum(severity_values == 1, na.rm = TRUE)
  med_pixels <- sum(severity_values == 2, na.rm = TRUE)
  high_pixels <- sum(severity_values == 3, na.rm = TRUE)

  # Calculate areas in hectares
  low_area <- low_pixels * pixel_size
  med_area <- med_pixels * pixel_size
  high_area <- high_pixels * pixel_size

  # Calculate total burned area
  total_burned_area <- sum(low_area, med_area, high_area)

  # Calculate percentages
  low_pct <- (low_area / total_burned_area) * 100
  med_pct <- (med_area / total_burned_area) * 100
  high_pct <- (high_area / total_burned_area) * 100

  # Create summary dataframe
  summary_stats <- data.frame(
    Severity_Class = c("Low", "Medium", "High"),
    Threshold = c(
      paste(round(thresh_low,3), "to", round(thresh_med,3)),
      paste(round(thresh_med,3), "to", round(thresh_high,3)),
      paste(">", round(thresh_high,3))
    ),
    Pixel_Count = c(low_pixels, med_pixels, high_pixels),
    Area_Ha = c(low_area, med_area, high_area),
    Percentage = c(low_pct, med_pct, high_pct)
  )

  # Return results
  return(list(
    summary_table = summary_stats,
    total_burned_area = total_burned_area,
    total_pixels = sum(low_pixels, med_pixels, high_pixels),
    thresholds = list(
      low = thresh_low,
      medium = thresh_med,
      high = thresh_high
    )
  ))
}
