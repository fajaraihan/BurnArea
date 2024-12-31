#' Plot Validation
#'
#' @param predicted_raster Raster of predicted burn area (binary: 1 for burned, 0 for unburned)
#' @param reference_shp Shapefile of reference burn area (polygon)
#' @return List containing omission error, commission error, and ICSI value
#' @importFrom raster rasterize
#' @importFrom raster getValues
#' @importFrom raster extent
#' @import sf
#' @import ggplot2
#' @importFrom grDevices png
#' @export
plot_burn_accuracy <- function(predicted_raster, reference_shp) {
  # Validasi input
  if (is.null(predicted_raster) || is.null(reference_shp)) {
    stop("Both predicted raster and reference shapefile must be provided")
  }

  # Crop predicted raster ke extent reference
  pred_extent <- raster::extent(reference_shp)
  predicted_raster <- raster::crop(predicted_raster, pred_extent)

  # Membuat mask dari reference shapefile
  reference_mask <- raster::rasterize(
    reference_shp,
    predicted_raster,
    field = 1,
    background = NA,
    touches = TRUE
  )

  # Mengaplikasikan mask ke predicted raster
  predicted_masked <- predicted_raster * reference_mask

  # Pastikan predicted raster bersifat biner (0/1)
  predicted_binary <- raster::calc(predicted_masked, fun = function(x) {
    ifelse(x > 0, 1, 0)
  })

  # Buat raster untuk klasifikasi hasil
  # Kode klasifikasi:
  # 0: Area tidak terbakar (correct rejection)
  # 1: True Positive (correctly detected burn)
  # 2: False Positive (commission error)
  # 3: False Negative (omission error)
  accuracy_raster <- raster::overlay(predicted_binary, reference_mask,
                                     fun = function(pred, ref) {
                                       result <- numeric(length(pred))
                                       result[pred == 1 & ref == 1] <- 1  # True Positive
                                       result[pred == 1 & ref == 0] <- 2  # Commission Error
                                       result[pred == 0 & ref == 1] <- 3  # Omission Error
                                       return(result)
                                     }
  )

  # Konversi raster ke dataframe untuk plotting
  accuracy_df <- as.data.frame(raster::rasterToPoints(accuracy_raster))
  colnames(accuracy_df) <- c("x", "y", "value")

  # Buat color palette dan labels
  color_palette <- c(
    "0" = "white",
    "1" = "green",
    "2" = "red",
    "3" = "blue"
  )

  legend_labels <- c(
    "Tidak Terbakar",
    "Terdeteksi Benar",
    "Commission Error",
    "Omission Error"
  )

  # Buat plot
  plot <- ggplot2::ggplot(data = accuracy_df) +
    ggplot2::geom_raster(ggplot2::aes(x = .data$x,
                                      y = .data$y,
                                      fill = factor(.data$value))) +
    ggplot2::scale_fill_manual(
      values = color_palette,
      labels = legend_labels,
      name = "Klasifikasi"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Validasi Area Kebakaran",
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "right"
    ) +
    ggplot2::coord_equal()

  # Tambahkan boundary reference shapefile
  plot <- plot +
    ggplot2::geom_sf(data = sf::st_as_sf(reference_shp),
                     fill = NA,
                     color = "black",
                     size = 0.5)

  return(plot)
}

