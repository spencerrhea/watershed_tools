library(tidyverse)
library(sf)
# devtools::install_github('https://github.com/MacroSHEDS/macrosheds')
# library(macrosheds)
source('src/ms_internals.R')
source('src/ms_delinate.R')


# DYnamic world thing https://sites.google.com/view/dynamic-world/home
# Create table of pour points
pour_points <- tibble(name = c('rio_napo_below__misahualli', 'rio_misahualli',
                               'rio_azul'),
                      long = c(-76.999811, -77.664703, -77.802525),
                      lat = c(-0.481384, -1.033150, -1.054648))

# Convert table to sf object
pour_points_sf <- pour_points %>%
    st_as_sf(., coords = c('long', 'lat'), crs = 4326)

# View points
mapview::mapview(pour_points_sf)

# delineate watersheds
out <- ms_delineate_watershed(lat = -1.043827,
                              long = -77.654237,
                              write_dir = 'data/ws_boundaries',
                              write_name = 'rio_napo_below',
                              crs = 4326,
                              spec_dem_resolution = 7,
                              spec_buffer_radius_m = 100000)

out <- ms_delineate_watershed(lat = -1.017753,
                              long = -77.670566,
                              write_dir = 'data/ws_boundaries/rio_misahualli',
                              write_name = 'rio_misahualli',
                              crs = 4326,
                              # spec_dem_resolution = 10,
                              spec_buffer_radius_m = 100000)

out <- ms_delineate_watershed(lat = -1.054648,
                              long = -77.802525,
                              write_dir = 'data/ws_boundaries/rio_azul',
                              write_name = 'rio_azul',
                              crs = 4326,
                              # spec_dem_resolution = 10,
                              spec_buffer_radius_m = 100000)

