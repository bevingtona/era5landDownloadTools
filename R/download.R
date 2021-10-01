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
#' @param download_dir download directory
#' @return A matrix of the infile
#' @export

era5_download_hourly <- function(aoi = aoi,
                                 aoi_name = name,
                                 years = 2021:2021,
                                 months = 5:8,
                                 days = 1:31,
                                 hours = 0:23,
                                 variables = c('surface_net_solar_radiation'),
                                 user = "71149",
                                 key = "76f3cbc5-bb4f-4347-8a51-80dc2bf1ca66",
                                 download_dir = "."){

  ### AUTHENTICATE ####

  wf_set_key(user = user,
             key = key,
             service = "cds")

  #### FORMAT REQUEST ####

  # Format years
  years <- as.character(years)

  # Format months
  months <- str_pad(string = months, width = 2, side = "left", pad = 0)

  # Format days
  days <- str_pad(string = days, width = 2, side = "left", pad = "0") # MAX 31

  # Format time
  hours <- paste0(str_pad(string = hours, width = 2, side = "left", pad = "0"),":00") # MAX 23

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
                   length(variables),"vars.grib")

  # FORMAT BOUNDS

  bounds = paste(st_bbox(aoi)[4] %>% as.numeric() %>% round(0),
                 st_bbox(aoi)[1] %>% as.numeric() %>% round(0),
                 st_bbox(aoi)[2] %>% as.numeric() %>% round(0),
                 st_bbox(aoi)[3] %>% as.numeric() %>% round(0), sep = "/")

  # Setup request
  request <- list("dataset_short_name" = 'reanalysis-era5-land',
                  'product_type' = 'reanalysis',
                  "variable" = variables,
                  "year" = years,
                  "month" = months,
                  "day" = days,
                  "time" = hours,
                  "area" = bounds,
                  "format" = "grib",
                  "target" = target)

  file <- wf_request(user     = "71149",
                     request  = request,
                     transfer = TRUE,
                     path     = download_dir)

  return(file)
  }

#' Download monthly
#'
#' This function
#'
#' @param aoi any sf object
#' @param aoi_name string
#' @param years numeric from 1979-2021
#' @param months numeric from 1-12
#' @param variables 'surface_net_solar_radiation','2m_temperature','total_precipitation','snow_depth_water_equivalent', ...
#' @param user your user id as a string
#' @param key your api key as a string
#' @param download_dir download directory
#' @return A matrix of the infile
#' @export



era5_download_monthly <- function(aoi = aoi,
                                  aoi_name = name,
                                  years = 2021:2021,
                                  months = 5:8,
                                  variables = c('2m_temperature', 'total_precipitation'),
                                  user = "71149",
                                  key = "76f3cbc5-bb4f-4347-8a51-80dc2bf1ca66",
                                  download_dir = "."){

  ### AUTHENTICATE ####

  wf_set_key(user = user,
             key = key,
             service = "cds")

  #### FORMAT REQUEST ####

  # Format years
  years <- as.character(years)

  # Format months
  months <- str_pad(string = months, width = 2, side = "left", pad = 0)

  # Out name
  target <- paste0("ERA5-land-monthly_",
                   aoi_name, "_",
                   min(years),"-",
                   max(years),"y_",
                   min(months),"-",
                   max(months),"m_",
                   length(variables),"vars.grib")

  # FORMAT BOUNDS

  bounds = paste(st_bbox(aoi)[4] %>% as.numeric() %>% round(0),
                 st_bbox(aoi)[1] %>% as.numeric() %>% round(0),
                 st_bbox(aoi)[2] %>% as.numeric() %>% round(0),
                 st_bbox(aoi)[3] %>% as.numeric() %>% round(0), sep = "/")

  # Setup request
  request <- list("dataset_short_name" = 'reanalysis-era5-land-monthly-means',
                  "product_type" = "monthly_averaged_reanalysis",
                  "variable" = variables,
                  "year" = years,
                  "month" = months,
                  "time" = "00:00",
                  "area" = bounds,
                  "format" = "grib",
                  "target" = target)

  file <- wf_request(user     = "71149",   # user ID (for authentification)
                     request  = request,  # the request
                     transfer = TRUE,     # download the file
                     path     = download_dir)      # store data in current working directory

  return(file)
}
