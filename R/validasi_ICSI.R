#' Calculate Omission and Commission Errors using shapefile reference
#'
#' @param predicted_raster Raster of predicted burn area (binary: 1 for burned, 0 for unburned)
#' @param reference_shp Shapefile of reference burn area (polygon)
#' @return List containing omission error, commission error, and ICSI value
#' @importFrom raster rasterize
#' @importFrom raster getValues
#' @importFrom raster extent
#' @import sf
#' @export
validate_icsi_shp <- function(predicted_raster, reference_shp) {
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
    background = NA,  # Mengubah background menjadi NA
    touches = TRUE
  )

  # Mengaplikasikan mask ke predicted raster
  predicted_masked <- predicted_raster * reference_mask

  # Pastikan predicted raster bersifat biner (0/1)
  predicted_masked <- raster::calc(predicted_masked, fun = function(x) {
    ifelse(x > 0, 1, 0)
  })

  # Konversi reference shapefile ke raster
  reference_raster <- raster::rasterize(
    reference_shp,
    predicted_masked,
    field = 1,
    background = 0,
    touches = TRUE
  )

  # Print ringkasan untuk debugging
  cat("Summary of predicted raster:\n")
  print(table(raster::getValues(predicted_masked), useNA = "always"))
  cat("\nSummary of reference raster:\n")
  print(table(raster::getValues(reference_raster), useNA = "always"))

  # Ambil nilai cell dan pastikan numerik
  pred_cells <- as.numeric(raster::getValues(predicted_masked))
  ref_cells <- as.numeric(raster::getValues(reference_raster))

  # Hapus nilai NA
  valid_cells <- !is.na(pred_cells) & !is.na(ref_cells)
  pred_cells <- pred_cells[valid_cells]
  ref_cells <- ref_cells[valid_cells]

  # Pastikan nilai strictly 0 atau 1
  pred_cells <- round(pred_cells)
  ref_cells <- round(ref_cells)

  # Hitung elemen confusion matrix
  true_positive <- sum(pred_cells == 1 & ref_cells == 1)
  false_positive <- sum(pred_cells == 1 & ref_cells == 0)
  false_negative <- sum(pred_cells == 0 & ref_cells == 1)
  true_negative <- sum(pred_cells == 0 & ref_cells == 0)

  # Hitung total area terbakar referensi
  total_ref_burned <- sum(ref_cells == 1)

  # Cek apakah ada area referensi
  if (total_ref_burned == 0) {
    warning("No burned area in reference data. Check if:\n",
            "1. Reference shapefile contains valid polygons\n",
            "2. CRS of both datasets match\n",
            "3. Extents overlap\n",
            "4. Rasterization parameters are appropriate")
    return(list(
      omission_error = NA,
      commission_error = NA,
      icsi = NA
    ))
  }

  # Hitung error
  omission <- false_negative / (true_positive + false_negative)
  commission <- false_positive / (true_positive + false_negative)

  # Hitung ICSI
  icsi <- (1 - (omission + commission)) * 100

  # Buat hasil
  results <- list(
    omission_error = omission,
    commission_error = commission,
    icsi = icsi,
    confusion_matrix = list(
      true_positive = true_positive,
      false_positive = false_positive,
      false_negative = false_negative,
      true_negative = true_negative
    ),
    summary = list(
      total_reference = total_ref_burned,
      total_predicted = sum(pred_cells == 1)
    )
  )

  return(results)
}
