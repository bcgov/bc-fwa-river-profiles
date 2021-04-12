![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)

River Profiles using the Freshwater Atlas of British Columbia
========================

### Overview

Simply input a river name from the Freshwater Atlas of BC, and the script will download the DEM, sample along that river, and return an `sf` object with elevation values and distances along the river. 

### Usage

Step 1: Just type the name of the river! 

``` r
library(stars)
library(bcmaps)
library(bcdata)
library(sf)
library(tidyverse)

rivername = "Willow River"
dat <- fwa_river_profile(rivername = rivername, pt_per_km = 4)  
print(dat)
#> Simple feature collection with 882 features and 4 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -122.5083 ymin: 53.10169 xmax: -121.5686 ymax: 54.08714
#> Geodetic CRS:  NAD83
#> First 10 features:
#>    elevation                          x dist_seg_m dist_tot_m id
#> 1        573 POINT (-122.5083 54.08655)     0.0000     0.0000  1
#> 2        574 POINT (-122.5045 54.08686)   247.2377   247.2377  2
#> 3        578 POINT (-122.5007 54.08714)   250.7904   498.0282  3
#> 4        573  POINT (-122.4981 54.0861)   208.3280   706.3562  4
#> 5        574 POINT (-122.4975 54.08392)   245.8678   952.2240  5
#> 6        575 POINT (-122.4943 54.08434)   210.5140  1162.7380  6
#> 7        576 POINT (-122.4912 54.08468)   211.0918  1373.8298  7
#> 8        575 POINT (-122.4907 54.08264)   229.8827  1603.7125  8
#> 9        575 POINT (-122.4923 54.08067)   244.5614  1848.2739  9
#> 10       578 POINT (-122.4919 54.07867)   224.8936  2073.1675 10

dat <- dat %>% 
  mutate(slope = (
    (lag(elevation,2)-lead(elevation,2))/
    (lag(dist_tot_m,2)-lead(dist_tot_m,2)))*100) 

dat %>% 
  ggplot() + 
    geom_point(aes(dist_tot_m/1000, elevation, color = slope), show.legend = T) + 
    scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(9, "Spectral")), 
                          guide = guide_colourbar(barheight = 10, frame.colour = "black", ticks.colour = "black")) +
    labs(x = "Distance (km)", y = "Elevation (m)", title = rivername, color = "Slope (%)") + 
    theme_bw() +
    theme(aspect.ratio = 0.5)
```
![](https://i.imgur.com/DaO4sbR.png)

### Project Status

Functional, but in development!

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/Project_River_Profiles/issues/).

### How to Contribute

If you would like to contribute, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### License

    Copyright 2019 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

------------------------------------------------------------------------

*This project was created using the
[bcgovr](https://github.com/bcgov/bcgovr) package.*
