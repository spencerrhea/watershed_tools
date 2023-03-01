#### This script contains helper function to pull data from dynamic world in
#### in google earth engine

# This function is from the macrosheds project
# Creates a projection string given a latlong
choose_projection <- function(lat = NULL,
                              long = NULL,
                              unprojected = FALSE){

    #TODO: CHOOSE PROJECTIONS MORE CAREFULLY

    if(unprojected){
        PROJ4 <- glue::glue('+proj=longlat +datum=WGS84 +no_defs ',
                            '+ellps=WGS84 +towgs84=0,0,0')
        return(PROJ4)
    }

    if(is.null(lat) || is.null(long)){
        stop('If projecting, lat and long are required.')
    }

    abslat <- abs(lat)

    # if(abslat < 23){ #tropical
    #     PROJ4 = glue::glue('+proj=laea +lon_0=', long)
    #              # ' +datum=WGS84 +units=m +no_defs')
    # } else { #temperate or polar
    #     PROJ4 = glue::glue('+proj=laea +lat_0=', lat, ' +lon_0=', long)
    # }

    #this is what the makers of https://projectionwizard.org/# use to choose
    #a suitable projection: https://rdrr.io/cran/rCAT/man/simProjWiz.html
    # THIS WORKS (PROJECTS STUFF), BUT CAN'T BE READ AUTOMATICALLY BY sf::st_read
    if(abslat < 70){ #tropical or temperate
        PROJ4 <- glue::glue('+proj=cea +lon_0={lng} +lat_ts=0 +x_0=0 +y_0=0 ',
                            '+ellps=WGS84 +datum=WGS84 +units=m +no_defs',
                            lng = long)
    } else { #polar
        PROJ4 <- glue::glue('+proj=laea +lat_0={lt} +lon_0={lng} +x_0=0 +y_0=0 ',
                            '+ellps=WGS84 +datum=WGS84 +units=m +no_defs',
                            lt = lat,
                            lng = long)
    }

    ## UTM/UPS would be nice for watersheds that don't fall on more than two zones
    ## (incomplete)
    # if(lat > 84 || lat < -80){ #polar; use Universal Polar Stereographic (UPS)
    #     PROJ4 <- glue::glue('+proj=ups +lon_0=', long)
    #              # ' +datum=WGS84 +units=m +no_defs')
    # } else { #not polar; use UTM
    #     PROJ4 <- glue::glue('+proj=utm +lat_0=', lat, ' +lon_0=', long)
    # }

    ## EXTRA CODE FOR CHOOSING PROJECTION BY LATITUDE ONLY
    # if(abslat < 23){ #tropical
    #     PROJ4 <- 9835 #Lambert cylindrical equal area (ellipsoidal; should spherical 9834 be used instead?)
    # } else if(abslat > 23 && abslat < 66){ # middle latitudes
    #     PROJ4 <- 5070 #albers equal area conic
    # } else { #polar (abslat >= 66)
    #     PROJ4 <- 9820 #lambert equal area azimuthal
    #     # PROJ4 <- 1027 #lambert equal area azimuthal (spherical)
    # }
    # PROJ4 <- 3857 #WGS 84 / Pseudo-Mercator
    # PROJ4 <- 2163

    return(PROJ4)
}

expo_backoff <- function(expr,
                         max_attempts = 10,
                         verbose = TRUE){

    for(attempt_i in seq_len(max_attempts)){

        results <- try(expr = expr,
                       silent = TRUE)

        if(inherits(results, 'try-error')){

            if(attempt_i == max_attempts){
                stop(attr(results, 'condition'))
            }

            backoff <- runif(n = 1,
                             min = 0,
                             max = 2^attempt_i - 1)

            if(verbose){
                print(glue::glue("Backing off for ", round(backoff, 1), " seconds."))
            }

            Sys.sleep(backoff)

        } else {

            # if(verbose){
            #     print(paste0("Request succeeded after ", attempt_i, " attempt(s)."))
            # }

            break
        }
    }

    return(results)
}

fill_sf_holes <- function(x){

    #x: an sf object (probably needs to be projected)

    #if there are spaces in a shapefile polygon that are not filled in,
    #   this fills them.

    #if the first element of an sf geometry (which is a list) contains multiple
    #   elements, every element after the first is a hole. the first element
    #   is the outer geometry. so replace the geometry with a new polygon that
    #   is only the outer geometry

    wb_geom <- sf::st_geometry(x)
    # wb_geom_crs <- sf::st_crs(wb_geom)

    n_polygons <- length(wb_geom[[1]])
    if(n_polygons > 1){
        wb_geom[[1]] <- sf::st_polygon(wb_geom[[1]][1])
    }

    # if(length(wb_geom) != 1){
    #     wb_geom <- sf::st_combine(wb_geom)
    # }

    sf::st_geometry(x) <- wb_geom

    return(x)
}
