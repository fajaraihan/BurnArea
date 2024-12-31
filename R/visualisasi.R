#' Plot Burn Severity Classification
#'
#' @param severity_raster Raster hasil klasifikasi dari classify_severity
#' @param title Judul plot (optional)
#' @param save_plot Boolean untuk menyimpan plot (default: FALSE)
#' @param output_path Path untuk menyimpan plot (optional)
#' @import rasterVis
#' @import lattice
#' @import grDevices
#' @return Plot tingkat keparahan kebakaran
#' @export
plot_burn_severity <- function(severity_raster,
                               title = "Burn Severity Classification",
                               save_plot = FALSE,
                               output_path = NULL) {

  # Memastikan raster hanya memiliki 3 kelas (1,2,3)
  severity_raster <- raster::clamp(severity_raster, lower=1, upper=3)

  # Definisi warna yang diskrit untuk setiap kelas
  severity_colors <- c("#FFFF00", "#FFA500", "#FF0000")
  names(severity_colors) <- c("Low", "Moderate", "High")

  # Membuat breaks yang tepat untuk 3 kelas
  breaks <- c(0.5, 1.5, 2.5, 3.5)

  # Mendapatkan extent raster untuk membuat grid
  ext <- raster::extent(severity_raster)

  # Membuat plot dasar
  plot <- levelplot(severity_raster,
                    main = "Burn Severity Classification",
                    margin = FALSE,
                    colorkey = list(
                      space = "right",
                      at = seq(1, 3, length.out = 3),
                      labels = c("Low", "Moderate", "High"),
                      raster = TRUE
                    ),
                    col.regions = c("#FFFF00", "#FFA500", "#FF0000"),
                    at = seq(1, 3, length.out = 3),
                    panel = function(...) {
                      panel.levelplot(...)
                      panel.grid()
                    },
                    xlab = "Longitude",
                    ylab = "Latitude"
  )

  # Menyimpan plot jika diminta
  if (save_plot) {
    if (is.null(output_path)) {
      output_path <- "burn_severity_plot.png"
    }
    grDevices::png(output_path, width = 800, height = 600, res = 100)
    print(plot)
    grDevices::dev.off()
    cat("Plot saved to:", output_path, "\n")
  }

  return(plot)
}
