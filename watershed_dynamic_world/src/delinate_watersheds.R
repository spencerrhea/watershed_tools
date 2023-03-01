#### This script dletes watershed using the scheme developed for the macrosheds::ms_delinate
#### Function. Needed to have more control and wated to view flow accumulation to
#### select pour points.

# Alternative to ms function, better because you can just look at the flow accumulation and
# Select a lat long. Could improve to have a click select lat long

library(tidyverse)
library(sf)
# devtools::install_github('https://github.com/MacroSHEDS/macrosheds')
# library(macrosheds)
source('src/helpers.R')

# Set up info for first watershed
dem_resolution <- 9
buffer_radius <- 1e+05
lat = -1.043827
long = -77.654237
crs <- 4326
flat_increment <- 0.01
snap_method <- 'jenson'

# Create temp directories to save the output of whitebox

scratch_dir = tempdir()
inspection_dir <- file.path(scratch_dir, 'INSPECT_THESE')
point_dir <- file.path(scratch_dir, 'POINT')
dem_f <- file.path(scratch_dir, 'dem.tif')
point_f <- file.path(scratch_dir, 'point.shp')
streams_f <- file.path(scratch_dir, 'streams.shp')
roads_f <- file.path(scratch_dir, 'roads.shp')
d8_f <- file.path(scratch_dir, 'd8_pntr.tif')
flow_f <- file.path(scratch_dir, 'flow.tif')
wb_f <- file.path(scratch_dir, 'wb_f.tif')
wb_f_new <- file.path(scratch_dir, 'wb_f_new.tif')


# Start watershed delination
proj <- choose_projection(lat = lat,
                          long = long)

site <- tibble(x = lat,
               y = long) %>%
    sf::st_as_sf(coords = c("y", "x"),
                 crs = crs) %>%
    sf::st_transform(proj)

site_buf <- sf::st_buffer(x = site,
                          dist = buffer_radius)

dem <- expo_backoff(
    expr = {
        elevatr::get_elev_raster(locations = site_buf,
                                 z = dem_resolution,
                                 verbose = FALSE,
                                 override_size_check = TRUE)
    },
    max_attempts = 5
)

# terra::writeRaster(x = dem,
terra::writeRaster(x = dem,
                   filename = dem_f,
                   overwrite = TRUE)

sf::st_write(obj = site,
             dsn = point_f,
             delete_layer = TRUE,
             quiet = TRUE)

whitebox::wbt_fill_single_cell_pits(dem = dem_f,
                                    output = dem_f) %>% invisible()

whitebox::wbt_breach_depressions(
    dem = dem_f,
    output = dem_f,
    flat_increment = flat_increment)

whitebox::wbt_d8_pointer(dem = dem_f,
                         output = d8_f) %>% invisible()

whitebox::wbt_d8_flow_accumulation(input = dem_f,
                                   output = flow_f,
                                   out_type = 'catchment area') %>% invisible()

snap1_f <- glue::glue(scratch_dir, '/snap1_jenson_dist150.shp')
whitebox::wbt_jenson_snap_pour_points(pour_pts = point_f,
                                      streams = flow_f,
                                      output = snap1_f,
                                      snap_dist = 150) %>% invisible()

whitebox::wbt_watershed(d8_pntr = d8_f,
                        pour_pts = snap1_f,
                        output = wb_f) %>% invisible()

wb <- terra::rast(wb_f)

wb_sf <- wb %>%
    terra::as.polygons() %>%
    sf::st_as_sf() %>%
    sf::st_buffer(dist = 0.1) %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    fill_sf_holes() %>%
    sf::st_transform(4326) %>%
    mutate(area = as.numeric(st_area(x))/1e+6,
           site_code = 'rio_napo_below_misahualli') %>%
    rename(geometry = x)

st_write(wb_sf,
         'data/ws_boundaries/rio_napo_below_misahualli',
         driver = 'ESRI Shapefile',
         delete_dsn = TRUE)

# mapview::mapview(wb_sf)
mapview::mapview(log10(raster::raster(flow_f)))

# Rio Azul
lat_ <- -1.07686
long_ <- -77.79694
proj_ <- choose_projection(lat = lat_,
                           long = long_)

site_ <- tibble(x = lat_,
                y = long_) %>%
    sf::st_as_sf(coords = c("y", "x"),
                 crs = crs) %>%
    sf::st_transform(proj)

new_snap_file <- file.path(scratch_dir, 'new_basin_point.shp')

sf::st_write(obj = site_,
             dsn = new_snap_file,
             delete_layer = TRUE,
             quiet = TRUE)

new_snaped_file <- file.path(scratch_dir, 'new_basin_point_snaped.tif')
whitebox::wbt_jenson_snap_pour_points(pour_pts = new_snap_file,
                                      streams = flow_f,
                                      output = new_snaped_file,
                                      snap_dist = 150) %>% invisible()


whitebox::wbt_watershed(d8_pntr = d8_f,
                        pour_pts = new_snap_file,
                        output = wb_f_new) %>% invisible()

wb_new <- terra::rast(wb_f_new)

wb_sf_new <- wb_new %>%
    terra::as.polygons() %>%
    sf::st_as_sf() %>%
    sf::st_buffer(dist = 0.1) %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    fill_sf_holes() %>%
    sf::st_transform(4326) %>%
    mutate(area = as.numeric(st_area(x))/1e+6,
           site_code = 'rio_azul') %>%
    rename(geometry = x)

mapview::mapview(wb_sf_new)

st_write(wb_sf_new,
         'data/ws_boundaries/rio_azul',
         driver = 'ESRI Shapefile',
         delete_dsn = TRUE)

# Rio Nago above Azul
lat_ <- -1.06198
long_ <- -77.81176
proj_ <- choose_projection(lat = lat_,
                           long = long_)

site_ <- tibble(x = lat_,
                y = long_) %>%
    sf::st_as_sf(coords = c("y", "x"),
                 crs = crs) %>%
    sf::st_transform(proj)

new_snap_file <- file.path(scratch_dir, 'new_basin_point.shp')

sf::st_write(obj = site_,
             dsn = new_snap_file,
             delete_layer = TRUE,
             quiet = TRUE)

new_snaped_file <- file.path(scratch_dir, 'new_basin_point_snaped.tif')
whitebox::wbt_jenson_snap_pour_points(pour_pts = new_snap_file,
                                      streams = flow_f,
                                      output = new_snaped_file,
                                      snap_dist = 150) %>% invisible()


whitebox::wbt_watershed(d8_pntr = d8_f,
                        pour_pts = new_snap_file,
                        output = wb_f_new) %>% invisible()

wb_new <- terra::rast(wb_f_new)

wb_sf_new <- wb_new %>%
    terra::as.polygons() %>%
    sf::st_as_sf() %>%
    sf::st_buffer(dist = 0.1) %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    fill_sf_holes() %>%
    sf::st_transform(4326) %>%
    mutate(area = as.numeric(st_area(x))/1e+6,
           site_code = 'rio_nago_above_azul') %>%
    rename(geometry = x)

mapview::mapview(wb_sf_new)

st_write(wb_sf_new,
         'data/ws_boundaries/rio_nago_above_azul',
         driver = 'ESRI Shapefile',
         delete_dsn = TRUE)


# Llandia
lat_ <- -1.27134
long_ <- -77.89216
proj_ <- choose_projection(lat = lat_,
                           long = long_)

site_ <- tibble(x = lat_,
                y = long_) %>%
    sf::st_as_sf(coords = c("y", "x"),
                 crs = crs) %>%
    sf::st_transform(proj)

new_snap_file <- file.path(scratch_dir, 'new_basin_point.shp')

sf::st_write(obj = site_,
             dsn = new_snap_file,
             delete_layer = TRUE,
             quiet = TRUE)

new_snaped_file <- file.path(scratch_dir, 'new_basin_point_snaped.tif')
whitebox::wbt_jenson_snap_pour_points(pour_pts = new_snap_file,
                                      streams = flow_f,
                                      output = new_snaped_file,
                                      snap_dist = 150) %>% invisible()


whitebox::wbt_watershed(d8_pntr = d8_f,
                        pour_pts = new_snap_file,
                        output = wb_f_new) %>% invisible()

wb_new <- terra::rast(wb_f_new)

wb_sf_new <- wb_new %>%
    terra::as.polygons() %>%
    sf::st_as_sf() %>%
    sf::st_buffer(dist = 0.1) %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    fill_sf_holes() %>%
    sf::st_transform(4326) %>%
    mutate(area = as.numeric(st_area(x))/1e+6,
           site_code = 'Llandia') %>%
    rename(geometry = x)

mapview::mapview(wb_sf_new)

st_write(wb_sf_new,
         'data/ws_boundaries/Llandia',
         driver = 'ESRI Shapefile',
         delete_dsn = TRUE)

# Rio Misahualli
lat_ <- -1.02562
long_ <- -77.67168
proj_ <- choose_projection(lat = lat_,
                           long = long_)

site_ <- tibble(x = lat_,
                y = long_) %>%
    sf::st_as_sf(coords = c("y", "x"),
                 crs = crs) %>%
    sf::st_transform(proj)

new_snap_file <- file.path(scratch_dir, 'new_basin_point.shp')

sf::st_write(obj = site_,
             dsn = new_snap_file,
             delete_layer = TRUE,
             quiet = TRUE)

new_snaped_file <- file.path(scratch_dir, 'new_basin_point_snaped.tif')
whitebox::wbt_jenson_snap_pour_points(pour_pts = new_snap_file,
                                      streams = flow_f,
                                      output = new_snaped_file,
                                      snap_dist = 150) %>% invisible()


whitebox::wbt_watershed(d8_pntr = d8_f,
                        pour_pts = new_snap_file,
                        output = wb_f_new) %>% invisible()

wb_new <- terra::rast(wb_f_new)

wb_sf_new <- wb_new %>%
    terra::as.polygons() %>%
    sf::st_as_sf() %>%
    sf::st_buffer(dist = 0.1) %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    fill_sf_holes() %>%
    sf::st_transform(4326) %>%
    mutate(area = as.numeric(st_area(x))/1e+6,
           site_code = 'rio_misahualli') %>%
    rename(geometry = x)

mapview::mapview(wb_sf_new)

st_write(wb_sf_new,
         'data/ws_boundaries/rio_misahualli',
         driver = 'ESRI Shapefile',
         delete_dsn = TRUE)

# Rio Tena
lat_ <- -0.99291
long_ <- -77.82052
proj_ <- choose_projection(lat = lat_,
                           long = long_)

site_ <- tibble(x = lat_,
                y = long_) %>%
    sf::st_as_sf(coords = c("y", "x"),
                 crs = crs) %>%
    sf::st_transform(proj)

new_snap_file <- file.path(scratch_dir, 'new_basin_point.shp')

sf::st_write(obj = site_,
             dsn = new_snap_file,
             delete_layer = TRUE,
             quiet = TRUE)

new_snaped_file <- file.path(scratch_dir, 'new_basin_point_snaped.tif')
whitebox::wbt_jenson_snap_pour_points(pour_pts = new_snap_file,
                                      streams = flow_f,
                                      output = new_snaped_file,
                                      snap_dist = 150) %>% invisible()


whitebox::wbt_watershed(d8_pntr = d8_f,
                        pour_pts = new_snap_file,
                        output = wb_f_new) %>% invisible()

wb_new <- terra::rast(wb_f_new)

wb_sf_new <- wb_new %>%
    terra::as.polygons() %>%
    sf::st_as_sf() %>%
    sf::st_buffer(dist = 0.1) %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    fill_sf_holes() %>%
    sf::st_transform(4326) %>%
    mutate(area = as.numeric(st_area(x))/1e+6,
           site_code = 'rio_tena') %>%
    rename(geometry = x)

mapview::mapview(wb_sf_new)

st_write(wb_sf_new,
         'data/ws_boundaries/rio_tena',
         driver = 'ESRI Shapefile',
         delete_dsn = TRUE)


# mining_stream
lat_ <- -1.100397
long_ <- -77.819133
proj_ <- choose_projection(lat = lat_,
                           long = long_)

site_ <- tibble(x = lat_,
                y = long_) %>%
    sf::st_as_sf(coords = c("y", "x"),
                 crs = crs) %>%
    sf::st_transform(proj)

new_snap_file <- file.path(scratch_dir, 'new_basin_point.shp')

sf::st_write(obj = site_,
             dsn = new_snap_file,
             delete_layer = TRUE,
             quiet = TRUE)

new_snaped_file <- file.path(scratch_dir, 'new_basin_point_snaped.tif')
whitebox::wbt_jenson_snap_pour_points(pour_pts = new_snap_file,
                                      streams = flow_f,
                                      output = new_snaped_file,
                                      snap_dist = 150) %>% invisible()


whitebox::wbt_watershed(d8_pntr = d8_f,
                        pour_pts = new_snap_file,
                        output = wb_f_new) %>% invisible()

wb_new <- terra::rast(wb_f_new)

wb_sf_new <- wb_new %>%
    terra::as.polygons() %>%
    sf::st_as_sf() %>%
    sf::st_buffer(dist = 0.1) %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    fill_sf_holes() %>%
    sf::st_transform(4326) %>%
    mutate(area = as.numeric(st_area(x))/1e+6,
           site_code = 'mining_shed') %>%
    rename(geometry = x)

mapview::mapview(wb_sf_new)

st_write(wb_sf_new,
         'data/ws_boundaries/mining_shed',
         driver = 'ESRI Shapefile',
         delete_dsn = TRUE)


