#' A function to map a river profile automatically from a named waterway in the Freshwater Atlas of British Columbia.
#'
#' This function allows you to activate assets using the Planet API. Assets cannot be downloaded until activated.
#' @param rivername Name of river in Freshwater Atlas Rivers dataset. Exact spelling required.
#' @param pt_per_km Density of points per km. Default is: 1
#' @param check_tiles Check if DEM tiles are present (Yes is slow)
#' @keywords Freshwater Atlas, River, Elevation, Profile
#' @export
#' @examples
#' \dontrun{
#' bowron_sf_1km <- fwa_river_profile(rivername = "Bowron River", pt_per_km = 1, check_tiles = T)
#' bowron_sf_250m <- fwa_river_profile(rivername = "Bowron River", pt_per_km = 4, check_tiles = T)
#' }

#' fwa_river_profile()

library(stars)
library(bcmaps)
library(bcdata)
library(sf)
library(tidyverse)

fwa_river_profile <- function(
  rivername = "Bowron River",
  pt_per_km = 1,
  check_tiles = T){

  # FRESHWATER ATLAS

    # Get River Polygons
    my_river <- bcdc_query_geodata("freshwater-atlas-rivers") %>%
      filter(GNIS_NAME_1 == rivername) %>%
      collect()

    # Get Unique Code
    my_river_code <- unique(my_river$FWA_WATERSHED_CODE)

    # Get Stream Network (lines)
    my_stream_network <- bcdc_query_geodata("freshwater-atlas-stream-network") %>%
      filter(FWA_WATERSHED_CODE == my_river_code) %>%
      collect()

    # GET MAINSTEM ONLY
    my_stream_network <-
      my_stream_network %>%
      filter(BLUE_LINE_KEY == unique(my_stream_network$WATERSHED_KEY)) %>% st_as_sf()

    # Combine River Segments
    my_stream_network <- st_cast(st_line_merge(
      st_union(st_cast(my_stream_network, "MULTILINESTRING"))), "LINESTRING") %>% st_zm()

  # SAMPLE ELEVATION AT POINTS

    # GET DEM
    dem <- cded_stars(my_stream_network, check_tiles = check_tiles)

    # Make Sample Points
    my_points <- my_stream_network %>%
      st_line_sample(density = units::set_units(pt_per_km, 1/km)) %>%
      st_cast("POINT") %>%
      st_as_sf() %>%
      st_transform(st_crs(dem))

    # Extract DEM Values at Points
    my_points_dem <- dem %>%
      st_extract(my_points)  %>%
      mutate(dist_seg_m = replace_na(as.numeric(st_distance(x, lag(x), by_element = TRUE)),0),
             dist_tot_m = cumsum(dist_seg_m),
             id = row_number(),
             river_name = rivername)

    return(my_points_dem)
    }
