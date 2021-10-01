# Installation

``` r
devtools::install_github("bevingtona/era5landDownloadTools")
```

# Download data

``` r
era_download <- era5_download_monthly(
  aoi = aoi,
  aoi_name = name,
  years = 2021:2021,
  months = 5:8,
  variables = c('2m_temperature'),
  user = "",
  key = "",
  download_dir = ".") 
```

# Read GRIB

``` r
era_stars <- era5_read_monthly_stars(
  aoi_name = name,
  years = 2021:2021,
  months = 5:8,
  variables = c('2m_temperature'),
  download_dir = ".")
```
