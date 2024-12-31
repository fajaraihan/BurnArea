#' Validate Burn Area Detection
#'
#' @param predicted_burn Predicted burn area raster
#' @param reference_burn Reference burn area shapefile (polygon)
#' @return Validation metrics
#' @importFrom raster values extent resample rasterize
#' @importFrom sf st_crs st_transform
#' @export
validate_burns <- function(predicted_burn, reference_burn) {
  # Periksa input types
  if (!inherits(predicted_burn, "RasterLayer")) {
    stop("predicted_burn must be a raster object")
  }
  if (!inherits(reference_burn, "sf") && !inherits(reference_burn, "SpatialPolygonsDataFrame")) {
    stop("reference_burn must be a shapefile polygon (sf or SpatialPolygonsDataFrame object)")
  }

  # Sesuaikan CRS jika berbeda
  if (inherits(reference_burn, "sf")) {
    ref_crs <- st_crs(reference_burn)
    pred_crs <- st_crs(predicted_burn)

    if (!identical(ref_crs, pred_crs)) {
      reference_burn <- st_transform(reference_burn, st_crs(predicted_burn))
    }
  }

  # Crop predicted raster ke extent reference
  pred_extent <- raster::extent(reference_burn)
  predicted_raster <- raster::crop(predicted_raster, pred_extent)

  # Membuat mask dari reference shapefile
  reference_raster <- raster::rasterize(
    reference_burn,
    predicted_raster,
    field = 1,
    background = NA,  # Mengubah background menjadi NA
    touches = TRUE
  )

  # Rasterize reference polygon
  #reference_raster <- rasterize(reference_burn, predicted_burn, field=1, background=0)  # Menggunakan background=0

  # Mengaplikasikan mask ke predicted raster
  predicted_masked <- predicted_raster * reference_raster

  # Pastikan predicted raster bersifat biner (0/1)
  predicted_masked <- raster::calc(predicted_masked, fun = function(x) {
    ifelse(x > 0, 1, 0)
  })

  # Ambil nilai raster
  pred_values <- raster::values(predicted_masked)
  ref_values <- raster::values(reference_raster)

  # Hitung jumlah pixel terbakar (tidak NA)
  burned_pred <- sum(pred_values == 1, na.rm = TRUE)
  burned_ref <- sum(ref_values == 1, na.rm = TRUE)

  # Konversi NA menjadi 0 di predicted values
  pred_values[is.na(pred_values)] <- 0

  # Buat factor dengan levels yang sama
  pred_factor <- factor(pred_values, levels=c(0,1))
  ref_factor <- factor(ref_values, levels=c(0,1))

  # Buat confusion matrix
  conf_matrix <- table(Predicted=pred_factor, Reference=ref_factor)

  # Hitung metrik
  TP <- conf_matrix[2,2]  # True Positive
  TN <- conf_matrix[1,1]  # True Negative
  FP <- conf_matrix[2,1]  # False Positive
  FN <- conf_matrix[1,2]  # False Negative

  # Hitung metrik evaluasi
  total_valid <- sum(conf_matrix)
  accuracy <- (TP + TN) / total_valid
  precision <- ifelse(TP + FP > 0, TP / (TP + FP), 0)
  recall <- ifelse(TP + FN > 0, TP / (TP + FN), 0)
  f1_score <- ifelse(precision + recall > 0,
                     2 * (precision * recall) / (precision + recall),
                     0)

  # Hitung Kappa
  #po <- accuracy
  #pe <- (((TN + FN)*(TN + FP)) + ((FP + TP)*(FN + TP))) / (total_valid^2)
  #kappa <- (po - pe) / (1 - pe)

  #po <- (TP + TN) / (TP + TN + FP + FN)

  #menggunakan caret
  #kappa <- caret::confusionMatrix(pred_factor, ref_factor)

  # Print hasil
  cat("\nConfusion Matrix:\n")
  print(conf_matrix)
  cat("\nMetrics Summary:\n")
  cat("Accuracy:", round(accuracy, 4), "\n")
  cat("Precision:", round(precision, 4), "\n")
  cat("Recall:", round(recall, 4), "\n")
  cat("F1 Score:", round(f1_score, 4), "\n")
  #cat("Kappa:", round(kappa, 4), "\n")

  # Hitung persentase area terbakar
  pred_burned_percent <- (burned_pred / total_valid) * 100
  ref_burned_percent <- (burned_ref / total_valid) * 100

  cat("\nBurned Area Percentage:\n")
  cat("Predicted:", round(pred_burned_percent, 2), "%\n")
  cat("Reference:", round(ref_burned_percent, 2), "%\n")

  # Return hasil
  return(list(
    confusion_matrix = conf_matrix,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1_score = f1_score,
    #kappa = kappa,
    burned_area_predicted = pred_burned_percent,
    burned_area_reference = ref_burned_percent,
    total_pixels = total_valid
  ))
}
