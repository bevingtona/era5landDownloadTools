## Installation

``` r
devtools::install_github("bevingtona/era5landDownloadTools")
#   ecmwfr (>= 1.3.0),
#   sf (>= 1.9-8),
#   dplyr (>= 1.0.6),
#   stringr (>= 1.4.0),
#   stars (>= 1.5-2)
```

## Prep example

``` r
aoi = mapedit::editMap()
name = "test"
```

## Download data

``` r
era5land_download <- era5land_download_monthly(
  aoi = aoi,
  aoi_name = name,
  years = 2021:2021,
  months = 5:8,
  variables = c('2m_temperature'),
  user = "",
  key = "",
  download_dir = ".") 
```

## Read GRIB

``` r
era5land_stars <- era5land_read_monthly_stars(
  aoi_name = name,
  years = 2021:2021,
  months = 5:8,
  variables = c('2m_temperature'),
  download_dir = ".")
```

## Comments

-   Hourly can be very slow! like, 20 hours on the server!
-   Check on your requests here:
    <https://cds.climate.copernicus.eu/cdsapp#!/yourrequests>

## Similar packages

-   This package relies on the `ecmwfr` package:
    <https://cran.r-project.org/web/packages/ecmwfr/index.html>
