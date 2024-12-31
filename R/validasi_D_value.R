#' Calculate Distance Index for NBR
#'
#' @param training_data Training data for threshold calculation
#' @return Distance index value
#' @export
calculate_distance <- function(training_data) {
  # Calculate statistics

  # Calculate D value
  D <- abs((training_data$post_stats$mean - training_data$pre_stats$mean) /
             (training_data$post_stats$sd + training_data$pre_stats$sd))

  return(D)
}
