#' Download hourly
#'
#' This function
#'
#' @param aoi any sf object
#' @param aoi_name string
#' @param years numeric from 1979-2021
#' @param months numeric from 1-12
#' @param days numeric from 1-31
#' @param hours numeric from 0-23
#' @param variables 'surface_net_solar_radiation','2m_temperature','total_precipitation','snow_depth_water_equivalent', ...
#' @param user your user id as a string
#' @param key your api key as a string
#' @param format format of the data. `"netcdf"` (default) or `"grib"`
#' @param download_dir download directory
#' @param ... further parameters passed to [ecmwfr::wf_request()]
#' #'
#' @return A matrix of the infile
#' @export

era5land_download_hourly <- function(aoi = aoi,
                                 aoi_name = "name",
                                 years = 2021:2021,
                                 months = 5:8,
                                 days = 1:31,
                                 hours = 0:23,
                                 variables = c('surface_net_solar_radiation'),
                                 user = "",
                                 key = "",
                                 download_dir = ".",
                                 format = c("netcdf", "grib"),
                                 ...){

  format <- match.arg(format)

  ### AUTHENTICATE ####

  cds_auth(user)

  #### FORMAT REQUEST ####

  # Format years
  years <- as.character(years)

  # Format months
  months <- stringr::str_pad(string = months, width = 2, side = "left", pad = 0)

  # Format days
  days <- stringr::str_pad(string = days, width = 2, side = "left", pad = "0") # MAX 31

  # Format time
  hours <- paste0(stringr::str_pad(string = hours, width = 2, side = "left", pad = "0"),":00") # MAX 23

  ext <- ifelse(format == "netcdf", "nc", "grib")

  # Out name
  target <- paste0("ERA5-land-hourly_",
                   aoi_name, "_",
                   min(years),"-",
                   max(years),"y_",
                   min(months),"-",
                   max(months),"m_",
                   min(days),"-",
                   max(days),"d_",
                   min(hours),"-",
                   max(hours),"h_",
                   length(variables),
                   "vars.", ext)

  # FORMAT BOUNDS

  bounds = format_bounds(aoi)

  # Setup request
  request <- list("dataset_short_name" = 'reanalysis-era5-land',
                  'product_type' = 'reanalysis',
                  "variable" = variables,
                  "year" = years,
                  "month" = months,
                  "day" = days,
                  "time" = hours,
                  "area" = bounds,
                  "format" = format,
                  "target" = target)

  file <- ecmwfr::wf_request(user     = user,
                     request  = request,
                     transfer = TRUE,
                     path     = download_dir,
                     ...)

  return(file)
  }

#' Download monthly
#'
#' This function
#'
#' @inheritParams era5land_download_hourly
#' @return A matrix of the infile
#' @export

era5land_download_monthly <- function(aoi = aoi,
                                  aoi_name = name,
                                  years = 2021:2021,
                                  months = 5:8,
                                  variables = c('2m_temperature', 'total_precipitation'),
                                  user = "",
                                  download_dir = ".",
                                  format = c("netcdf", "grib"),
                                  ...){

  format <- match.arg(format)

  ### AUTHENTICATE ####

  cds_auth(user)

  #### FORMAT REQUEST ####

  # Format years
  years <- as.character(years)

  # Format months
  months <- stringr::str_pad(string = months, width = 2, side = "left", pad = 0)

  ext <- ifelse(format == "netcdf", "nc", "grib")

  # Out name
  target <- paste0("ERA5-land-monthly_",
                   aoi_name, "_",
                   min(years),"-",
                   max(years),"y_",
                   min(months),"-",
                   max(months),"m_",
                   length(variables),
                   "vars.", ext)

  # FORMAT BOUNDS

  bounds = format_bounds(aoi)

  # Setup request
  request <- list("dataset_short_name" = 'reanalysis-era5-land-monthly-means',
                  "product_type" = "monthly_averaged_reanalysis",
                  "variable" = variables,
                  "year" = years,
                  "month" = months,
                  "time" = "00:00",
                  "area" = bounds,
                  "format" = format,
                  "target" = target)

  file <- ecmwfr::wf_request(user = user,
                     request  = request,
                     transfer = TRUE,
                     path     = download_dir,
                     ...)

  return(file)
}

cds_auth <- function(user) {
  if (!nzchar(user)) stop("PLease specify your user id")

  keytry <- try(ecmwfr::wf_get_key(user = user, service = "cds"))

  if (inherits(keytry, "try-error")) {
    stop("You have not set your API key. Please do so with",
         "ecmwfr::sf_set_key()")
  }

  invisible(TRUE)
}

format_bounds <- function(aoi) {
  paste(sf::st_bbox(aoi)[4] %>% as.numeric() %>% round(2),
        sf::st_bbox(aoi)[1] %>% as.numeric() %>% round(2),
        sf::st_bbox(aoi)[2] %>% as.numeric() %>% round(2),
        sf::st_bbox(aoi)[3] %>% as.numeric() %>% round(2), sep = "/")
}
