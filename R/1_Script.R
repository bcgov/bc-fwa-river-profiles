
# require(rgdal)
library(gridExtra)
library(rgeos)
library(ggspatial)
library(raster)
library(bcmaps)
library(tidyverse)
library(sf)
library(bcdata)
library(strucchange)


# Import Omineca
# rom <- nr_regions() %>% filter(ORG_UNIT == "ROM")
# write_sf(rom, "../omineca.sqlite")
# plot(rom$geometry)

# Import DEM and clip to Omineca

# dem <- raster("N:/Data_DEM/DEM_ASTER/ASTER_GDEM.vrt")
# rom <-  st_transform(rom, crs = crs(dem))
# dem_rom <- crop(dem, rom)
# dem_rom <- mask(dem_rom, rom)
# writeRaster(dem_rom, "dem/dem_omineca.tif")
# dem_rom <- raster("dem/dem_omineca.tif")
# plot(dem_rom)
# dem_rom_controur <- raster::rasterToContour(dem_rom)
# plot(dem_rom_controur)

# dem <- raster("N:/Data_DEM/DEM_ALOS/ALOS_DSM.vrt")
# rom <-  st_transform(rom, crs = crs(dem))
# dem_rom <- crop(dem, rom)
# dem_rom <- mask(dem_rom, rom)
# writeRaster(dem_rom, "dem/alos_dem_omineca.tif")
dem_rom <- raster("dem/alos_dem_omineca.tif")
# plot(dem_rom)
# dem_rom_controur <- raster::rasterToContour(dem_rom)
# plot(dem_rom_controur)

rivernames <- c("Stuart River",
                "Nechako River",
                "Middle River",
                "Tachie River",
                "Driftwood River",
                "Table River",
                "Misinchinka River",
                "Anzac River",
                "Missinka River",
                "Hominka River",
                "Misinchinka River",
                "Colbourne Creek",
                "Reynolds Creek")

for(rivername in rivernames){
  rivername <- rivernames[13]
  print(rivername)
  # Import Rivers



  my_river <- bcdc_query_geodata("freshwater-atlas-rivers") %>%
    filter(GNIS_NAME_1 == rivername) %>%
    collect()

  my_river_code <- unique(my_river$FWA_WATERSHED_CODE)

  # my_network
  my_stream_network <- bcdc_query_geodata("freshwater-atlas-stream-network") %>%
    filter(FWA_WATERSHED_CODE == my_river_code) %>%
    collect()
  my_stream_network <- my_stream_network %>%
    filter(BLUE_LINE_KEY == unique(my_stream_network$WATERSHED_KEY))
  # plot(my_stream_network)

  my_stream_network <-
    my_stream_network %>%
    # filter(STREAM_ORDER > 3) %>%
    st_cast("MULTILINESTRING") %>%
    st_union();
  # plot(my_stream_network)


  my_stream_network_WGS <- my_stream_network %>%
    st_transform(crs(dem_rom)) %>%
    st_zm() %>%
    # summarise() %>%
    as_Spatial()

  lines <- gLineMerge(my_stream_network_WGS)
  points <- lines %>% st_as_sf() %>% st_cast("POINT")
  # points %>% as("Spatial")
  # %>% write_sf("point.shp")

  # lines %>%
  #   st_as_sf() %>%
  #   st_transform(3005) %>%
  #   write_sf("temp.shp")
  # s <- rasterize(my_stream_network_WGS, dem_rom)
  # plot(s)
  # Project the raster to the same crs as the line
  # RStoolbox::ggR(dem_rom) +
  #   geom_sf(data = my_stream_network_WGS, size = 2, color = "red")

  # Extract values along the lines
  # rm(transect)

  transect <- raster::extract(dem_rom, points %>% as("Spatial"), cellnumbers=T)
  transect_df <- as_tibble(transect)
  transect_df_resample <- transect_df#[seq(1,nrow(transect_df),10),]
  transect_coords <- xyFromCell(dem_rom, transect_df_resample$cells)
  pair_dist = geosphere::distGeo(transect_coords)[-nrow(transect_coords)]
  transect_df_resample$x = as_tibble(transect_coords)$x
  transect_df_resample$y = as_tibble(transect_coords)$y
  transect_df_resample$dist = c(0, cumsum(pair_dist))
  transect_df_resample <- transect_df_resample %>% filter(!is.na(alos_dem_omineca))


  transect_df_resample$distkm <-  transect_df_resample$dist/1000
  # ggplot(transect_df_resample) + geom_point(aes(distkm, alos_dem_omineca))
  breaks <- strucchange::breakpoints(transect_df_resample$alos_dem_omineca ~ transect_df_resample$distkm)
  # breaks$breakpoints
  breakf <- strucchange::breakfactor(obj = breaks)
  # length(breakf)
  transect_df_resample$fit <- breakf

  lmfit <- transect_df_resample %>% group_by(fit) %>%
    summarise(
      meanX = mean(distkm),
      meanY = mean(alos_dem_omineca),
      slope = coef(lm(alos_dem_omineca~distkm))[2])

  p <- grid.arrange(
    ggplot() +
      annotation_map_tile(zoom = 10, cachedir = system.file("rosm.cache", package = "ggspatial")) +
      layer_spatial(data = lines) +
      geom_sf(data = st_as_sf(transect_df_resample, coords = c("x", "y"), crs = 4326), aes(color = fit), show.legend = F) +
      scale_colour_brewer(palette = "Set1") +
      theme(aspect.ratio = 1) +
      theme_void(),
    ggplot(transect_df_resample, aes(distkm, alos_dem_omineca)) +
      geom_smooth(aes(group = fit, color = fit), method = "lm", show.legend = F, se = F, size = 2, alpha = 0.8) +
      geom_line(size = 1.2) +
      geom_text(data = lmfit, aes(x= meanX, y = meanY, label = paste(signif(slope,2), "m/km"), group = fit),
                hjust=-1, angle = 90) +
      theme_classic() +
      scale_colour_brewer(palette = "Set1") +
      geom_vline(data = as_tibble(breaks$breakpoints), aes(xintercept = transect_df_resample[value,]$distkm), linetype = 2) +
      labs(title = rivername, x = "Distance (km)", y = "Elevation (m) [Source: ALOS]") +
      theme(aspect.ratio = 0.6),ncol = 1)

  ggsave(plot = p, filename = paste0("plots/alos_",sub(" ", "_",rivername),"2.pdf"))
}

