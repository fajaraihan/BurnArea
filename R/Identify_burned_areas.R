#' Identify Burned Areas Using NBR Thresholds
#'
#' @param nbr_post Post-fire NBR raster
#' @param dnbr Delta NBR raster
#' @param training_data Training data for threshold calculation
#' @return Raster of identified burned areas
#' @export
identify_burned_areas <- function(nbr_post, dnbr, training_data) {
  # Calculate thresholds
  nbr_stats <- calculate_stats(training_data$post)
  dnbr_stats <- calculate_stats(training_data$delta)

  alpha_nbr <- training_data$post_stats$mean + (2 * training_data$post_stats$sd)
  beta_nbr <- training_data$delta_stats$mean - (2 * training_data$delta_stats$sd)

  # Apply threshold conditions
  burned_areas <- (nbr_post <= alpha_nbr & dnbr >= beta_nbr)

  return(burned_areas)
}
