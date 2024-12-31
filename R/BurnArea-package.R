#' BurnArea: A package for burn area detection and severity classification
#'
#' @name BurnArea
#' @aliases BurnArea
#'
#' @description
#' A comprehensive package for detecting burned areas and classifying burn severity
#' using optical satellite imagery. The package provides functions for NBR calculation,
#' burn severity classification, statistical analysis, and visualisation.
#'
#' @section Burn Severity Classification:
#' The package implements burn severity classification based on PPPJL (2015) and
#' Dewi (2017) using NBR thresholds calculated from mean and standard deviation.
#'
#' @references
#' 1. Addabbo P, Focareta M, Marcuccio S, Votto C, Ullo SL. 2016. Contribution of
#' Sentinel-2 data for application in vegetation monitoring. Acta Imeiko. 5(2):44-54.
#' 2. Dewi R. 2017. Estimasi tingkat keparahan kebakaran hutan dan lahan menggunakan
#' citra landsat 8 di Kabupaten Rokan Hilir Provinsi Riau [skripsi].
#' Bogor: Institut Pertanian Bogor.
#' 3. Fraser, R.H., Li, Z. and Cihlar, J., 2000. Hotspot and NDVI differencing synergy
#' (HANDS): A new technique for burned area mapping over boreal forest.
#' Remote sensing of Environment, 74(3), pp.362-376.
#' 4.  Kaufman YJ, Remer LA. 1994. Detection of forests fire using mid-IR reflectance:
#' an application for aerosol studies. IEEE Transactions on Geoscience and
#' Remote Sensing. 32:672-683.
#' 5. Koukoulas S, Blackburn GA. 2001. Intoducing new indices for accuracy evaluation
#' of classified images representing semi-natural woodland environments.
#' Photogrammetric Engineering & Remote Sensing. 67(4):499-510.
#' 6. [PPPJL] Pusat Pemanfaatan Penginderaan Jauh Lembaga Penerbangan dan
#' Antariksa Nasional. 2015. Pedoman Pemanfaatan Data Landsat 8 untuk
#' Deteksi Daerah Terbakar (Burned Area). Jakarta: Lembaga Penerbangan dan
#' Antariksa Nasional.
#'
#' @import rgdal
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats predict quantile aggregate weighted.mean density update
#' @importFrom raster raster stack brick writeRaster
#' @importFrom methods is
#' @importFrom graphics legend
#' @importFrom raster res
#' @importFrom raster raster
#' @importFrom raster writeRaster
#' @importFrom raster stack
#' @importFrom raster brick
#' @importFrom raster extent
#' @importFrom raster projection
#' @importFrom raster resample
#' @importFrom raster crop
#' @importFrom raster mask
#' @importFrom raster cellStats
#' @importFrom stats predict
#' @importFrom stats quantile
#' @importFrom stats aggregate
#' @importFrom stats weighted.mean
#' @importFrom stats density
#' @importFrom stats update
#' @importFrom stats median
#' @importFrom stats sd
#' @import ggplot2
#' @importFrom methods is
#' @importFrom sp CRS
#' @importFrom rgdal writeOGR
#' @importFrom RColorBrewer brewer.pal
#' @import rasterVis
#' @import lattice
#' @import grDevices

#'
#' @examples
#' \dontrun{
#' # Load data
#' nir_band_pre <- raster("nir_band_pre")
#' swir_band_pre <- raster("swir_band_pre")
#'
#' nir_band_post <- raster("nir_band_post")
#' swir_band_post <- raster("swir_band_post")
#'
#' # Stacking raster
#' stack_pre <- raster::stack(nir_band_pre, swir_band_pre)
#' stack_post <- raster::stack(nir_band_post, swir_band_post)
#'
#' # Calculate NBR for pre and post fire
#' nbr_pre <- calculate_nbr(stack_pre)
#' nbr_post <- calculate_nbr(stack_post)

#  # Calculate dNBR
#' dnbr <- calculate_dnbr(nbr_pre, nbr_post)
#'
#' # Identify Burn Areas
#' training_shp <- rgdal::readOGR("path/folder/file.shp")
#'
#' #generate training data
#' training_data <- generate_training_data(nbr_pre, nbr_post, dnbr, training_shp)
#'
#' #Identify Burn Area
#' burned_areas <- identify_burned_areas(nbr_post, dnbr, training_data)
#'
#' # Classify burn severity
#' klasifikasi <- classify_severity(nbr_post, dnbr, training_data)
#'
#' # Analyze burn statistics #(pixel size 0,01 for 10m sentinel resolution)
#' burn_stats <- calculate_burn_stats(severity_raster, training_data, pixel_size = 0.01)
#'
#' # Visualisation
#' burn_severitu_plot <- plot_burn_severity(severity_raster,
#' title = "Burn Severity Classification",
#' save_plot = FALSE,
#' output_path = NULL)
#'
#  # Display statistics
#' print("Burn Area Statistics:")
#' print(paste("Total Burned Area (ha):", burn_stats$total_area/10000))
#' print(paste("Mean NBR:", round(burn_stats$mean_nbr, 3)))
#' print(paste("Standard Deviation NBR:", round(burn_stats$std_nbr, 3)))
#'
#' # Accuracy D value
#' d_value <- calculate_distance(training_data)
#'
#' # Accuracy icsi
#' icsi <- validate_icsi_shp(predicted_raster, reference_shp)
#'
#' # Accuracy
#' Accuracy <- validate_burns(predicted_burn, reference_burn)
#'
#' # Visualisation Accuracy
#' plot_accuracy  <- plot_burn_accuracy(predicted_raster, reference_shp)
#'
#' # Display validation metrics
#' print("\nValidation Results:")
#' print(paste("Overall Accuracy:", round(validation_results$accuracy, 3)))
#' print(paste("Kappa Coefficient:", round(validation_results$kappa, 3)))
#' print("\nConfusion Matrix:")
#' print(validation_results$confusion_matrix)
#'
#' # Export
#' export<- export_burns(burn_raster, filename, format = "GTiff")
#'
#' }
NULL

#' @keywords internal
"_PACKAGE"
