#' Read hourly
#'
#' This function
#'
#' @param aoi_name string
#' @param years numeric from 1979-2021
#' @param months numeric from 1-12
#' @param days numeric from 1-31
#' @param hours numeric from 0-23
#' @param variables 'surface_net_solar_radiation','2m_temperature','total_precipitation','snow_depth_water_equivalent', ...
#' @param download_dir download directory
#' @return A matrix of the infile
#' @export

era5land_read_hourly_stars <- function(aoi_name = name,
                                   years = 2021:2021,
                                   months = 5:8,
                                   days = 1:31,
                                   hours = 0:23,
                                   variables = c('surface_net_solar_radiation'),
                                   download_dir = "."){

  # Format years
  years <- as.character(years)

  # Format months
  months <- stringr::str_pad(string = months, width = 2, side = "left", pad = 0)

  # Format days
  days <- stringr::str_pad(string = days, width = 2, side = "left", pad = "0") # MAX 31

  # Format time
  hours <- paste0(stringr::str_pad(string = hours, width = 2, side = "left", pad = "0"),":00") # MAX 23

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

  # Make band names for output
  bands <-
    do.call(c, lapply(years, function(y){
      out <- c()
      for(m in months){
        for(d in days){
          for(h in hours){
            for(v in variables) {
              out <- c(out,paste0(y,"-",m,"-",d,"-",h,"!",v))
            }}}}
      return(out)}))


  my_stack <- stars::read_stars(target)
  st_crs(my_stack) <- 4326
  my_stack <- my_stack %>% stars::st_set_dimensions(which = 3, values = bands)

  return(my_stack)
  }

#' Read monthly
#'
#' This function
#'
#' @param aoi_name string
#' @param years numeric from 1979-2021
#' @param months numeric from 1-12
#' @param variables 'surface_net_solar_radiation','2m_temperature','total_precipitation','snow_depth_water_equivalent', ...
#' @param download_dir download directory
#' @return A matrix of the infile
#' @export

era5land_read_monthly_stars <- function(aoi_name = name,
                                    years = 2021:2021,
                                    months = 5:8,
                                    variables = c('surface_net_solar_radiation'),
                                    download_dir = "."){

  # Format years
  years <- as.character(years)

  # Format months
  months <- stringr::str_pad(string = months, width = 2, side = "left", pad = 0)

  # Out name
  target <- paste0("ERA5-land-monthly_",
                   aoi_name, "_",
                   min(years),"-",
                   max(years),"y_",
                   min(months),"-",
                   max(months),"m_",
                   length(variables),"vars.grib")

  # Make band names for output
  bands <-
    do.call(c, lapply(years, function(y){
      out <- c()
      for(m in months){
        for(v in variables) {
            out <- c(out,paste0(y,"-",m,"!",v))
            }}
      return(out)}))


  my_stack <- stars::read_stars(target)
  st_crs(my_stack) <- 4326
  my_stack <- my_stack %>% stars::st_set_dimensions(which = 3, values = bands)

  return(my_stack)
}
