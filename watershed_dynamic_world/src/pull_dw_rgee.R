library(sf)
library(tidyverse)
library(rgee)

rgee::ee_Initialize(user = 'spencerrhea41@gmail.com')

# Load landcover defs
color_key = tibble(id = 0:8,
                   col = c('#419BDF', '#397D49', '#88B053',
                           '#7A87C6', '#E49635', '#DFC35A',
                           '#C4281B', '#A59B8F', '#B39FE1'),
                   label = c('water', 'trees', 'grass',
                             'flooded_vegetation', 'crops',
                             'shrub_and_scrub', 'built', 'bare',
                             'snow_and_ice')) %>%
    mutate(id = as.character(id))

# Get watersheds
all_fils <- list.files('data/ws_boundaries/', full.names = TRUE)

all_sheds <- tibble()
for(i in 1:length(all_fils)){

    ws_name <- str_split_fixed(all_fils[i], '/', n = Inf)[,4]
    site <- st_read(all_fils[i]) %>%
        mutate(site_code = !!ws_name,
               area = st_area(geometry))

    if('FID' %in% names(site)){
        site <- site %>%
            select(-FID)
    }


    all_sheds <- rbind(all_sheds, site)

    print(i)
}

sf::write_sf(all_sheds,
             'ecuador',
             driver = 'ESRI Shapefile')

st_read(da)

mapview::mapview(all_sheds)

# STart ee tasks
all_ee_task <- c()
needed_files <- c()
dynamicworld_all <- tibble()
sites <- all_sheds$site_code
for(s in 1:length(sites)){
    # Get site boundary and check if the watershed is in Puerto Rico, Alaska, or Hawaii
    site_boundary <- all_sheds %>%
        filter(site_code == sites[s]) %>%
        st_make_valid()

    # 2015-06-23
    sheds_bb <- sf::st_bbox(all_sheds) %>%
        sf::st_as_sfc(., crs = 4326)

    nlcd_epochs = as.character(c(1992, 2001, 2004, 2006, 2008, 2011, 2013, 2016))


    user_info <- rgee::ee_user_info(quiet = TRUE)
    asset_folder <- glue('{a}/macrosheds_ws_boundaries/{d}/',
                         a = user_info$asset_home,
                         d = domain)

    asset_path <- rgee::ee_manage_assetlist(asset_folder)

    if(nrow(asset_path) == 1){
        ws_boundary_asset <- ee$FeatureCollection(asset_path$ID)
        filter_ee <- ee$Filter$inList('site_code', c(sites[s], sites[s]))
        site_ws_asset <- ws_boundary_asset$filter(filter_ee);
    } else {
        ws_boundary_asset <- str_split_fixed(asset_path$ID, '/', n = Inf)
        ws_boundary_asset <- ws_boundary_asset[,ncol(ws_boundary_asset)]

        this_asset <- asset_path[grep(sites[s], ws_boundary_asset),]
        site_ws_asset <- ee$FeatureCollection(this_asset$ID)
    }

    for(e in nlcd_epochs){

        #subset_id = paste0('NLCD', as.character(e))
        img = ee$ImageCollection('USGS/NLCD_RELEASES/2016_REL')$
            select('landcover')$
            filter(ee$Filter$eq('system:index', e))$
            first()$
            clip(site_ws_asset)

        ee_description <-  glue('{n}_{d}_{s}_{p}_{e}',
                                d = domain,
                                n = network,
                                s = sites[s],
                                p = str_split_fixed(prodname_ms, '__', n = Inf)[1,1],
                                e = e)

        file_name <- paste0('nlcdX_X', e, 'X_X', sites[s])
        ee_task <- ee$batch$Export$image$toDrive(image = img,
                                                 description = ee_description,
                                                 folder = 'GEE',
                                                 # crs = 'ESPG:4326',
                                                 region = site_ws_asset$geometry(),
                                                 fileNamePrefix = file_name,
                                                 maxPixels=NULL)

        needed_files <- c(needed_files, file_name)
        all_ee_task <- c(all_ee_task, ee_description)

        try(googledrive::drive_rm(paste0('GEE/', file_name, '.tif')),
            silent = TRUE) #in case previous drive_rm failed

        start_mess <- try(ee_task$start())
        if(class(start_mess) == 'try-error'){
            return(generate_ms_err(glue('error in retrieving {s}',
                                        s = site_code)))
        }
        #ee_monitoring(ee_task)
    }
}

needed_files <- paste0(needed_files, '.tif')

talsk_running <- rgee::ee_manage_task()

talsk_running <- talsk_running %>%
    filter(DestinationPath %in% all_ee_task)

while(any(talsk_running$State %in% c('RUNNING', 'READY'))) {
    talsk_running <- rgee::ee_manage_task()

    talsk_running <- talsk_running %>%
        filter(DestinationPath %in% all_ee_task)

    Sys.sleep(5)
}
temp_rgee <- tempfile(fileext = '.tif')

for(i in 1:length(needed_files)){

    rel_file <- needed_files[i]

    string <- str_match(rel_file, '(.+?)X_X(.+?)X_X(.+?)\\.tif')[2:4]
    year <- string[2]
    site <- string[3]

    file_there <- googledrive::drive_get(paste0('GEE/', rel_file))

    if(nrow(file_there) == 0){ next }
    expo_backoff(
        expr = {
            googledrive::drive_download(file = paste0('GEE/', rel_file),
                                        temp_rgee,
                                        overwrite = TRUE)
        },
        max_attempts = 5
    ) %>% invisible()

    nlcd_rast <- terra::rast(temp_rgee)

    nlcd_rast[as.vector(terra::values(nlcd_rast)) == 0] <- NA

    googledrive::drive_rm(rel_file)

    tabulated_values = terra::values(nlcd_rast) %>%
        table() %>%
        as_tibble() %>%
        rename(id = '.',
               CellTally = 'n')

    if(length(str_split_fixed(year, '_', n = Inf)[1,]) == 2){
        year <- as.numeric(str_split_fixed(year, pattern = '_', n = Inf)[1,1])
    }

    if(year == 1992){

        nlcd_e = full_join(nlcd_summary_1992,
                           tabulated_values,
                           by = 'id') %>%
            mutate(sum = sum(CellTally, na.rm = TRUE))

        nlcd_e_1992names <- nlcd_e %>%
            mutate(percent = round((CellTally/sum)*100, 1)) %>%
            mutate(percent = ifelse(is.na(percent), 0, percent)) %>%
            select(var = macrosheds_1992_code, val = percent) %>%
            mutate(year = !!year)

        nlcd_e_norm_names <- nlcd_e %>%
            group_by(macrosheds_code) %>%
            summarise(CellTally1992 = sum(CellTally, na.rm = TRUE)) %>%
            ungroup() %>%
            mutate(sum = sum(CellTally1992, na.rm = TRUE)) %>%
            mutate(percent = round((CellTally1992/sum)*100, 1)) %>%
            mutate(percent = ifelse(is.na(percent), 0, percent)) %>%
            select(var = macrosheds_code, val = percent) %>%
            mutate(year = !!year)

        nlcd_e <- rbind(nlcd_e_1992names, nlcd_e_norm_names) %>%
            mutate(site_code = !!site)

    } else{

        nlcd_e = full_join(nlcd_summary,
                           tabulated_values,
                           by = 'id')

        nlcd_e <- nlcd_e %>%
            mutate(percent = round((CellTally*100)/sum(CellTally, na.rm = TRUE), 1)) %>%
            mutate(percent = ifelse(is.na(percent), 0, percent)) %>%
            select(var = macrosheds_code, val = percent) %>%
            mutate(year = !!year) %>%
            mutate(site_code = !!site)
    }
    nlcd_all = rbind(nlcd_all, nlcd_e)
}

nlcd_final <- nlcd_all %>%
    mutate(year = as.numeric(year)) %>%
    select(year, site_code, var, val)

nlcd_final <- append_unprod_prefix(nlcd_final, prodname_ms)

save_general_files(final_file = nlcd_final,
                   domain_dir = nlcd_dir)