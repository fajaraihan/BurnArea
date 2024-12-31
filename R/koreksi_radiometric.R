#' Radiometric Correction for Satellite Imagery
#'
#' @description
#' Performs radiometric correction on satellite imagery bands by converting Digital Numbers (DN)
#' to reflectance values. This includes both DN to radiance conversion and radiance to reflectance conversion.
#'
#' @param band RasterLayer object containing the satellite band data
#' @param gain Multiplicative rescaling factor (default values for Landsat 8)
#' @param offset Additive rescaling factor (default values for Landsat 8)
#' @param sun_elevation Sun elevation angle in degrees
#' @param earth_sun_dist Earth-Sun distance in astronomical units
#'
#' @return RasterLayer containing radiometrically corrected values
#'
#' @details
#' The function applies the following corrections:
#' 1. DN to Radiance: L = gain * DN + offset
#' 2. Radiance to Reflectance: ρ = (π * L * d²) / (ESUN * cos(θ))
#' where:
#' - L is radiance
#' - d is Earth-Sun distance
#' - ESUN is solar irradiance
#' - θ is solar zenith angle (90 - sun elevation)
#'
#' @importFrom raster raster values calc writeRaster
#' @importFrom methods is
#'
#' @examples
#' \dontrun{
#' # Load a satellite band
#' band <- raster::raster("landsat_band.tif")
#'
#' # Apply radiometric correction
#' corrected_band <- radiometric_correction(
#'   band,
#'   gain = 0.00002,
#'   offset = -0.1,
#'   sun_elevation = 68.5,
#'   earth_sun_dist = 1.014
#' )
#' }
#'
#' @export
radiometric_correction <- function(band,
                                   gain = 0.00002,
                                   offset = -0.1,
                                   sun_elevation = NULL,
                                   earth_sun_dist = NULL) {

  # Input validation
  if (!methods::is(band, "RasterLayer")) {
    stop("Input 'band' must be a RasterLayer object")
  }

  # Check for invalid values
  if (any(raster::values(band) < 0, na.rm = TRUE)) {
    warning("Input band contains negative values")
  }

  # Convert to radiance
  radiance <- function(x) {
    rad <- gain * x + offset
    return(rad)
  }

  # Apply radiance conversion
  radiance_band <- raster::calc(band, radiance)

  # If sun elevation and earth-sun distance are provided, convert to reflectance
  if (!is.null(sun_elevation) && !is.null(earth_sun_dist)) {
    # Constants
    PI <- 3.14159265359
    ESUN <- 1000  # Example solar irradiance value (should be band-specific)

    # Convert sun elevation to zenith angle (in radians)
    solar_zenith <- (90 - sun_elevation) * PI / 180

    # Radiance to reflectance conversion
    reflectance <- function(x) {
      refl <- (PI * x * earth_sun_dist^2) / (ESUN * cos(solar_zenith))
      return(refl)
    }

    # Apply reflectance conversion
    corrected_band <- raster::calc(radiance_band, reflectance)

    # Scale reflectance values to 0-1 range
    corrected_band <- raster::calc(corrected_band, function(x) {
      x[x < 0] <- 0
      x[x > 1] <- 1
      return(x)
    })

  } else {
    corrected_band <- radiance_band
  }

  # Copy metadata from input
  corrected_band@crs <- band@crs
  corrected_band@extent <- band@extent

  # Add processing history
  history <- paste("Radiometric correction applied:",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  # Set layer name
  names(corrected_band) <- paste0(names(band), "_corrected")

  return(corrected_band)
}

#' Calculate Earth-Sun Distance
#'
#' @param doy Day of year (1-366)
#' @return Earth-Sun distance in astronomical units
#' @export
calculate_earth_sun_distance <- function(doy) {
  if (doy < 1 || doy > 366) {
    stop("Day of year must be between 1 and 366")
  }

  # Calculate Earth-Sun distance using Spencer's formula
  angle <- 2 * pi * (doy - 1) / 365
  distance <- 1.000110 + 0.034221 * cos(angle) + 0.001280 * sin(angle) +
    0.000719 * cos(2 * angle) + 0.000077 * sin(2 * angle)

  return(distance)
}

# Example usage:
#' \dontrun{
#' # Load required libraries
#' library(raster)
#'
#' # Read satellite band
#' nir_band <- raster::raster("NIR_band.tif")
#'
#' # Calculate Earth-Sun distance for a specific date (e.g., day 180 of the year)
#' earth_sun_dist <- calculate_earth_sun_distance(180)
#'
#' # Apply radiometric correction
#' corrected_nir <- radiometric_correction(
#'   band = nir_band,
#'   gain = 0.00002,
#'   offset = -0.1,
#'   sun_elevation = 65.5,
#'   earth_sun_dist = earth_sun_dist
#' )
#'
#' # Save corrected band
#' raster::writeRaster(corrected_nir,
#'                     filename = "corrected_NIR.tif",
#'                     format = "GTiff",
#'                     overwrite = TRUE)
#' }
