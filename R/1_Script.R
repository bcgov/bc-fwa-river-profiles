# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.



library(bcmaps)
library(tidyverse)
library(sf)
library(stars)


fwa_river_profile <- function(rivername = "Bowron River", pt_per_km = 1, check_tiles = F){

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
    
    # Combine River Segments
    my_stream_network <-
      my_stream_network %>%
      st_cast("MULTILINESTRING") %>%
      st_combine() %>% 
      st_cast("LINESTRING") %>% 
      st_as_sf() %>% 
      st_zm();
      
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
      arrange(elevation) %>% 
      mutate(dist_seg_m = replace_na(as.numeric(st_distance(x, lag(x), by_element = TRUE)),0), 
             dist_tot_m = cumsum(dist_seg_m))
    
    return(my_points_dem)
    }
      

