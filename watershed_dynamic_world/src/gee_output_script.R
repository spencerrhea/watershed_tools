#### This script post processes the output of a GEE script here:
#### https://code.earthengine.google.com/fe12b3f5e66f5bd2056301d091325afd
#### The GEE script takes a feature collection of polygons and extracts the pixel
#### Count for each dynamic world landcover class. This scrip processes that
#### out put to calculate the percent landcover for a watershed over time.

#### You can Explore Dynamic World change in a watershed here:
#### https://code.earthengine.google.com/79620985b72a731aaabc734f5bb2ea42

#### Start o script
library(tidyverse)

# Read in GEE output
dw_pixle_count <- read_csv('data/pixel_counts.csv')


# Loop though all rows in GEE output dataframe to read in data and fix weird
# GEE output. The GEE script could be improved so this post processing step is
# not needed but this works just fine

all_class_names <- c('null', 0:8)

fin_watershed_cover <- tibble()
for(i in 1:nrow(dw_pixle_count)){

    this_cell <- dw_pixle_count[i,3]
    year <- str_split_fixed(dw_pixle_count[i,1], '[.]', n = Inf)[1,1]
    sitr_code <- dw_pixle_count[i,4]

    class_string <- str_split_fixed(this_cell, ',', n = Inf) %>%
        str_trim() %>%
        str_remove(., '[}]') %>%
        str_remove(., '[{]')

    class_name <- str_split_fixed(class_string, '=', n = Inf)[,1]
    append_classes <- all_class_names[! all_class_names %in% class_name]

    class_name_full <- append(class_name, append_classes)

    class_value <-  str_split_fixed(class_string, '=', n = Inf)[,2]

    class_value_full <- append(class_value, vector(mode = 'numeric', length = length(append_classes)))

    if(! length(class_name_full) == length(class_value_full)){
        prtin('you messed up')
        stop()
    }

    site_year_table <- tibble(year = year,
                              site_code = sitr_code,
                              class = class_name_full,
                              pixle_count = as.numeric(class_value_full)) %>%
         mutate(total_pixles = sum(pixle_count)) %>%
         mutate(percent_shed = round((pixle_count/total_pixles)*100, 3))

    fin_watershed_cover <- rbind(fin_watershed_cover, site_year_table)
}

# Join dynamic world codes to their description
dynamicworld_codes <- tibble(class = c(0:8, 'null'),
                             description = c('water', 'trees', 'grass',
                                             'flooded_vegetation', 'crops',
                                             'shrub_and_scrub', 'built',
                                             'bare', 'snow_and_ice',
                                             'null'))

palette <- c('#A59B8F', '#C4281B', '#E49635', '#7A87C6', '#88B053',
             '#000000', '#DFC35A', '#B39FE1', '#397D49', '#419BDF')

fin_watershed_cover <- left_join(fin_watershed_cover, dynamicworld_codes)

# Plot landcover change over time
# Can see this watershed is losing forest cover since 2017 and bare cover
# increased in 2019 and 2020 but shrub/scrub is increasing after 2020. Maybe the
# Land is being cleared of forest (bare class) and vegetation is taking the land over
# after being cleared (shrub/scrub)

fin_watershed_cover %>%
    filter(site_code == 'mining_shed') %>%
    filter(year != 2015,
           year != 2016,
           year != 2023) %>%
    ggplot(., aes(x = year, y = percent_shed, col = description, group = description)) +
    geom_point() +
    scale_color_manual(values = palette) +
    geom_line()

# Visualize Watersheds
# Get watersheds
library(sf)
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
}

mapview::mapview(all_sheds)


