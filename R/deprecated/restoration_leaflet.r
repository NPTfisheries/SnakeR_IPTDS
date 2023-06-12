# Author: Mike Ackerman
# Purpose: Compile available juvenile and redd data for candidate restoration sites and create a leaflet
# of that data
# Created: 11/19/2019
# Last Modified:

#-----------------------------------------------------------------
rm(list = ls())

#-----------------------------------------------------------------
# load needed libraries
# install.packages("leaflet")
# install.packages("leaflet.extras")
# install.packages("htmlwidgets")
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(tidyverse)
library(sf)
library(janitor)
library(lubridate)
library(readxl)
library(scales)
#library(raster)

#-----------------------------------------------------------------
# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd('C:/Users/mikea/Dropbox/Projects/BOR/MRA2/analysis/')
getwd()

# read in temperature data (this will be useful later)
lemhi_tmp_2012 = st_read('data/lemhi_temp_shapefiles/Lemhi_2012/Lem_2012_8D_Mn.shp') %>%
  mutate_at(vars(starts_with('Tmn')), list(~na_if(., '-9999')))

####################################
# SITE ABUNDANCE/DENSITY ESTIMATES #
####################################
# read in Braden's abundance/density summary
site_abund_est = read_csv('data/AbundanceEstRestoration.csv') %>%
  clean_names() %>%
  select(species, site_name, year, n, fish_m) %>%
  mutate(year = as.factor(year))

# plot fish densities by species and year
site_dens_p = site_abund_est %>%
  ggplot(aes(x = year, y = fish_m)) +
  geom_col(colour = 'steelblue', fill = 'steelblue') +
  theme_bw() +
  labs(x = 'Year',
       y = 'Fish/m') +
  facet_grid(species ~ site_name)
site_dens_p
# ggsave(site_dens_p, file = 'figures/site_densities_plot.png')
  
# plot fish abundance by species and year
# site_abund_p = site_abund_est %>%
#   ggplot(aes(x = year, y = n)) +
#   geom_col(colour = 'steelblue', fill = 'steelblue') +
#   theme_bw() +
#   labs(x = 'Year',
#        y = 'Abundance') +
#   facet_grid(species ~ site_name)
# site_abund_p

##########################################
# BEYELER JUVENILE FISH OBSERVATION DATA #
##########################################
# read in data
# juv_df = st_read('data/shapefiles/Juv_Chnk_13to18.shp') %>%
#   filter(Species == c('Chinook', 'Steelhead'))
 
# plot Beyeler juvenile observations
# juv_p = juv_df %>%
#   ggplot() +
#   geom_sf(aes(color = Species, fill = Species)) +
#   theme_classic()
# juv_p

#####################################
# ALL LEMHI ROVING OBSERVATION DATA #
#####################################
# juvenile fish observation data from Braden's accessDB
rov_df = read_csv('data/1_RovingFishSurveys.csv',
                     col_types = cols(
                       LatitudeDD = col_double(),
                       LongitudeDD = col_double(),
                       GRTS_SubSite = col_character(),
                       HabitatType = col_character(),
                       HabitatFeature = col_character(),
                       TagStatus = col_character(),
                       HabitatUnit = col_character(),
                       HabitatSegment = col_character(),
                       HabitatReach = col_character()
                     )) %>%
  clean_names() %>%
  filter(species %in% c('Brook Trout', 'Bull Trout', 'Chinook', 
                        'Cutthroat Trout', 'Steelhead')) %>%
  filter(fish_status != 'Dead') %>%
  filter(latitude_dd != 'NA') %>%
  filter(longitude_dd < 0) %>% # 3 Chinook in another hemisphere
  select(stream_name, site_name, dce_name, survey_date_time, species, fish_count, latitude_dd, longitude_dd,
         fish_observation_key) %>%
  mutate(survey_date_time = as.POSIXct(survey_date_time, format = '%m/%d/%Y'),
         year = as.factor(year(survey_date_time))) %>%
  filter(year != '2010') %>%
  filter(stream_name %in% c('Agency Creek', 'Bear Valley Creek', 'Big Bear Creek', 'Big Eightmile Creek',
                           'Big Springs Creek', 'Big Timber Creek', 'Bohannon Creek', 'Canyon Creek',
                           'Cow Creek', 'Cruikshank Creek', 'East Fork Bohannon Creek', 'Hawley Creek',
                           'Hayden Creek', 'Kenney Creek', 'Lee Creek', 'Lemhi River', 'Little Springs Creek',
                           'Mill Creek', 'Pratt creek', 'Pratt Creek', 'Reservoir Creek', 'Wimpey Creek')) %>%
  droplevels()
# write_csv(rov_df, 'data/roving_df.csv')

# I exported the above as a .csv, imported into ArcMap, and then exported as a shapefile
# read roving data in as a spatial feature (sf)
rov_sf = st_read('data/shapefiles/roving_df.shp')

# plot juvenile fish observations
rov_p = rov_sf %>%
  ggplot() +
  geom_sf(aes(color = species, fill = species)) +
  theme_classic()
rov_p

######################################################
# SPATIALLY CONTINUOUS JUV CHINOOK DENSITY ESTIMATES #
######################################################
chnk2015 = st_read('data/shapefiles/Chnk_Continuous_2015.shp') %>%
  clean_names() %>%
  select(strm_nm, shp_lng, chnnl_ty, species, pop_est, s_e, fish_m, geometry) %>%
  rename(n = pop_est,
         se = s_e) %>%
  mutate(year = '2015')

chnk2016 = st_read('data/shapefiles/Chnk_Continuous_2016.shp') %>%
  clean_names() %>%
  select(strm_nm, shp_lng, chnnl_ty, species, pop_est, s_e, fish_m, geometry) %>%
  rename(n = pop_est,
         se = s_e) %>%
  mutate(year = '2016')

chnk2017 = st_read('data/shapefiles/Chnk_Continuous_2017.shp') %>%
  clean_names() %>%
  select(strm_nm, shp_lng, chnnl_ty, species, n, se, fish_m, geometry) %>%
  mutate(year = '2017')

chnk2018 = st_read('data/shapefiles/Chnk_Continuous_2018.shp') %>%
  clean_names() %>%
  select(strm_nm, shp_lng, chnnl_ty, species, n, se, fish_m, geometry) %>%
  mutate(year = '2018')

chnk_cont = rbind(chnk2015, chnk2016, chnk2017, chnk2018) %>%
  filter(fish_m != 'NA') %>%
  st_transform(st_crs(4326))

# plot spatially continuous estimates of juvenile Chinook density where available
chnk_cont_p = chnk_cont %>%
  ggplot() +
  geom_sf(aes(color = fish_m)) +
  scale_colour_gradient(low = 'green', high = 'red') +
  labs(color = 'Fish/m') +
  theme_classic() +
  facet_wrap(~ year)
chnk_cont_p

#############
# REDD DATA #
#############
# read in redd data
redd_df = read_csv('data/ReddData_With_RID_MEAS.csv') %>%
  clean_names() %>%
  filter(species == 'Chinook' & basin == 'Lemhi') %>%
  select(index, meas, distance, year, species, run, basin, stream,
         latitude, longitude, near_dist, near_x, near_y) %>%
  mutate(year = as.factor(year)) %>%
  st_as_sf(coords = c('near_x', 'near_y'), crs = 4326) #%>%     # convert to sf object
  #st_transform(st_crs(lemhi_tmp_2012))                         # set crs to same as lemhi_tmp_2012

# plot redds
redd_p = redd_df %>%
  ggplot() +
  geom_sf(aes(color = year, fill = year)) +
  theme_bw() +
  scale_x_continuous(labels = number_format(accuracy = 0.01)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  facet_wrap(~ year)
redd_p

#############################
# PARSING DATA FOR LEAFLETS #
#############################

# parse Chinook salmon redd data by years for leaflet
yrs = as.numeric(as.vector(unique(redd_df$year)))
for(y in yrs) {
  assign(paste('redds', y, sep = ''), filter(redd_df, year == as.character(y)))
}

# parse spatially continuous juvenile Chinook density estimates for leaflets
yrs = as.numeric(as.vector(unique(chnk_cont$year)))
for(y in yrs) {
  assign(paste('chnk', y, sep = ''), filter(chnk_cont, year == as.character(y)))
}

spc = unique(rov_sf$species)
for(s in spc) {
  assign(
    paste('rov_', make_clean_names(s), sep = ''),
    filter(rov_sf, species == as.character(s))
  )
}

#######################################
# RESTORATION SITE PROJECT BOUNDARIES #
#######################################

# get list of all layers in the .kml file containing site boundaries
proj_layers = st_layers('data/LemPahCandidateSites.kml')

# lemhi projects
evr_sr1     = st_read('data/LemPahCandidateSites.kml', layer = 'Eagle Valley SR-1.kmz')
split_river = st_read('data/LemPahCandidateSites.kml', layer = 'Split River.kmz')
beyeler     = st_read('data/LemPahCandidateSites.kml', layer = 'Beyeler.kmz')

# pahsimeroi projects
last_chance = st_read('data/LemPahCandidateSites.kml', layer = 'Last Chance Ranch.kmz')
whitworth   = st_read('data/LemPahCandidateSites.kml', layer = 'Whitworth.kmz')

# list of projects
projects = c(evr_sr1, split_river, beyeler, last_chance, whitworth)

# plot one project boundary as an example
# proj_p = beyeler %>%
#   ggplot() +
#   geom_sf() +
#   theme_classic()
# proj_p

#####################
# CREATING LEAFLETS #
#####################

# create project outlines
beyeler_outline     = st_zm(beyeler, drop = TRUE, what = "ZM")
evr_sr1_outline     = st_zm(evr_sr1, drop = TRUE, what = "ZM")
split_river_outline = st_zm(split_river, drop = TRUE, what = "ZM")
last_chance_outline = st_zm(last_chance, drop = TRUE, what = "ZM")
whitworth_outline   = st_zm(whitworth, drop = TRUE, what = "ZM")

#---------------------------------------
# BEYELER
beyeler_leaflet = leaflet()
beyeler_leaflet = beyeler_leaflet %>%
  addProviderTiles('Esri.WorldImagery', group = 'Imagery') %>%
  addProviderTiles('Esri', group = 'Esri') %>%
  addProviderTiles('OpenTopoMap', group = 'Topo') %>%
  addPolygons(data = beyeler_outline, weight = 3, color = 'black', fillOpacity = 0.1,
              group = 'Project Outline') %>%
  addCircleMarkers(data = redds1999, radius = 0.5, color = 'red', group = 'Redds 1999') %>%
  addCircleMarkers(data = redds2000, radius = 0.5, color = 'red', group = 'Redds 2000') %>%
  addCircleMarkers(data = redds2001, radius = 0.5, color = 'red', group = 'Redds 2001') %>%
  addCircleMarkers(data = redds2002, radius = 0.5, color = 'red', group = 'Redds 2002') %>%
  addCircleMarkers(data = redds2003, radius = 0.5, color = 'red', group = 'Redds 2003') %>%
  addCircleMarkers(data = redds2004, radius = 0.5, color = 'red', group = 'Redds 2004') %>%
  addCircleMarkers(data = redds2005, radius = 0.5, color = 'red', group = 'Redds 2005') %>%
  addCircleMarkers(data = redds2006, radius = 0.5, color = 'red', group = 'Redds 2006') %>%
  addCircleMarkers(data = redds2007, radius = 0.5, color = 'red', group = 'Redds 2007') %>%
  addCircleMarkers(data = redds2008, radius = 0.5, color = 'red', group = 'Redds 2008') %>%
  addCircleMarkers(data = redds2009, radius = 0.5, color = 'red', group = 'Redds 2009') %>%
  addCircleMarkers(data = redds2010, radius = 0.5, color = 'red', group = 'Redds 2010') %>%
  addCircleMarkers(data = redds2011, radius = 0.5, color = 'red', group = 'Redds 2011') %>%
  addCircleMarkers(data = redds2012, radius = 0.5, color = 'red', group = 'Redds 2012') %>%
  addCircleMarkers(data = redds2013, radius = 0.5, color = 'red', group = 'Redds 2013') %>%
  addCircleMarkers(data = redds2014, radius = 0.5, color = 'red', group = 'Redds 2014') %>%
  addCircleMarkers(data = redds2015, radius = 0.5, color = 'red', group = 'Redds 2015') %>%
  setView(beyeler_leaflet, lng = -113.36707, lat = 44.69181, zoom = 14) %>%
  addLegend(position = 'topright', 
            colors = c('black', 'red'),
            labels = c('Project Outline', 'Redds 1999 - 2015')) %>%
  addLayersControl(baseGroups = c('Imagery', 'Topo', 'Esri'),
                   overlayGroups = c('Project Outline', 'Redds 1999', 'Redds 2000',
                                     'Redds 2001', 'Redds 2002', 'Redds 2003', 'Redds 2004',
                                     'Redds 2005', 'Redds 2006', 'Redds 2007', 'Redds 2008',
                                     'Redds 2009', 'Redds 2010', 'Redds 2011', 'Redds 2012',
                                     'Redds 2013', 'Redds 2014', 'Redds 2015'))
beyeler_leaflet
# save leaflet
path = 'leaflets/beyeler_leaflet.html'
saveWidget(beyeler_leaflet, file.path(normalizePath(dirname(path)),basename(path)))

#---------------------------------------
# ALL LEMHI PROJECTS
restoration_leaflet = leaflet()
restoration_leaflet = restoration_leaflet %>%
  addProviderTiles('Esri.WorldImagery', group = 'Imagery') %>%
  addProviderTiles('Esri', group = 'Esri') %>%
  addProviderTiles('OpenTopoMap', group = 'Topo') %>%
  addPolygons(data = beyeler_outline, weight = 3, color = 'black', fillOpacity = 0.1,
              group = 'Beyeler Project Outline', popup = 'Beyeler') %>%
  addPolygons(data = split_river_outline, weight = 3, color = 'black', fillOpacity = 0.1,
              group = 'Split River Project Outline', popup = 'Split River') %>%
  addPolygons(data = evr_sr1_outline, weight = 3, color = 'black', fillOpacity = 0.1,
              group = 'EVR SR1 Outline', popup = 'EVR SR1') %>%
  addPolygons(data = last_chance_outline, weight = 3, color = 'black', fillOpacity = 0.1,
              group = 'Last Chance Project Outline', popup = 'Last Chance') %>%
  addPolygons(data = whitworth_outline, weight = 3, color = 'black', fillOpacity = 0.1,
              group = 'Whitworth Outline', popup = 'Whitworth') %>%
  addPolylines(data = chnk2015, color = 'blue', group = 'Chinook Densities 2015',
               popup = paste('Stream Name:', chnk2015$strm_nm, '<br>',
                             'Channel Type =', chnk2015$chnnl_ty, '<br>',
                             'Species =', chnk2015$species, '<br>',
                             'N = ', chnk2015$n, '<br>',
                             'SE =', chnk2015$se, '<br>',
                             'Fish/m =', chnk2015$fish_m)) %>%
  addPolylines(data = chnk2016, color = 'blue', group = 'Chinook Densities 2016', 
               popup = paste('Stream Name:', chnk2016$strm_nm, '<br>',
                             'Channel Type =', chnk2016$chnnl_ty, '<br>',
                             'Species =', chnk2016$species, '<br>',
                             'N = ', chnk2016$n, '<br>',
                             'SE =', chnk2016$se, '<br>',
                             'Fish/m =', chnk2016$fish_m)) %>%
  addPolylines(data = chnk2017, color = 'blue', group = 'Chinook Densities 2017', 
               popup = paste('Stream Name:', chnk2017$strm_nm, '<br>',
                             'Channel Type =', chnk2017$chnnl_ty, '<br>',
                             'Species =', chnk2017$species, '<br>',
                             'N = ', chnk2017$n, '<br>',
                             'SE =', chnk2017$se, '<br>',
                             'Fish/m =', chnk2017$fish_m)) %>%
  addPolylines(data = chnk2018, color = 'blue', group = 'Chinook Densities 2018', 
               popup = paste('Stream Name:', chnk2018$strm_nm, '<br>',
                             'Channel Type =', chnk2018$chnnl_ty, '<br>',
                             'Species =', chnk2018$species, '<br>',
                             'N = ', chnk2018$n, '<br>',
                             'SE =', chnk2018$se, '<br>',
                             'Fish/m =', chnk2018$fish_m)) %>%
  addCircleMarkers(data = rov_chinook, radius = 0.2, color = 'green', group = 'Chinook Obs',
                   popup = paste('Species:', rov_chinook$species, '<br>',
                                 'Stream Name:', rov_chinook$stream_nam, '<br>',
                                 'Year:', rov_chinook$year, '<br>',
                                 'Fish Count:', rov_chinook$fish_count)) %>%
  addCircleMarkers(data = rov_steelhead, radius = 0.2, color = 'aquamarine', group = 'Steelhead Obs',
                   popup = paste('Species:', rov_steelhead$species, '<br>',
                                 'Stream Name:', rov_steelhead$stream_nam, '<br>',
                                 'Year:', rov_steelhead$year, '<br>',
                                 'Fish Count:', rov_steelhead$fish_count)) %>%
  addCircleMarkers(data = rov_bull_trout, radius = 0.2, color = 'gray', group = 'Bull Obs',
                   popup = paste('Species:', rov_bull_trout$species, '<br>',
                                 'Stream Name:', rov_bull_trout$stream_nam, '<br>',
                                 'Year:', rov_bull_trout$year, '<br>',
                                 'Fish Count:', rov_bull_trout$fish_count)) %>%
  addCircleMarkers(data = rov_cutthroat_trout, radius = 0.2, color = 'gold', group = 'Cutthroat Obs',
                   popup = paste('Species:', rov_cutthroat_trout$species, '<br>',
                                 'Stream Name:', rov_cutthroat_trout$stream_nam, '<br>',
                                 'Year:', rov_cutthroat_trout$year, '<br>',
                                 'Fish Count:', rov_cutthroat_trout$fish_count)) %>%
  addCircleMarkers(data = rov_brook_trout, radius = 0.2, color = 'darkred', group = 'Brook Obs',
                   popup = paste('Species:', rov_brook_trout$species, '<br>',
                                 'Stream Name:', rov_brook_trout$stream_nam, '<br>',
                                 'Year:', rov_brook_trout$year, '<br>',
                                 'Fish Count:', rov_brook_trout$fish_count)) %>%
  addCircleMarkers(data = redds1999, radius = 0.5, color = 'red', group = 'Redds 1999') %>%
  addCircleMarkers(data = redds2000, radius = 0.5, color = 'red', group = 'Redds 2000') %>%
  addCircleMarkers(data = redds2001, radius = 0.5, color = 'red', group = 'Redds 2001') %>%
  addCircleMarkers(data = redds2002, radius = 0.5, color = 'red', group = 'Redds 2002') %>%
  addCircleMarkers(data = redds2003, radius = 0.5, color = 'red', group = 'Redds 2003') %>%
  addCircleMarkers(data = redds2004, radius = 0.5, color = 'red', group = 'Redds 2004') %>%
  addCircleMarkers(data = redds2005, radius = 0.5, color = 'red', group = 'Redds 2005') %>%
  addCircleMarkers(data = redds2006, radius = 0.5, color = 'red', group = 'Redds 2006') %>%
  addCircleMarkers(data = redds2007, radius = 0.5, color = 'red', group = 'Redds 2007') %>%
  addCircleMarkers(data = redds2008, radius = 0.5, color = 'red', group = 'Redds 2008') %>%
  addCircleMarkers(data = redds2009, radius = 0.5, color = 'red', group = 'Redds 2009') %>%
  addCircleMarkers(data = redds2010, radius = 0.5, color = 'red', group = 'Redds 2010') %>%
  addCircleMarkers(data = redds2011, radius = 0.5, color = 'red', group = 'Redds 2011') %>%
  addCircleMarkers(data = redds2012, radius = 0.5, color = 'red', group = 'Redds 2012') %>%
  addCircleMarkers(data = redds2013, radius = 0.5, color = 'red', group = 'Redds 2013') %>%
  addCircleMarkers(data = redds2014, radius = 0.5, color = 'red', group = 'Redds 2014') %>%
  addCircleMarkers(data = redds2015, radius = 0.5, color = 'red', group = 'Redds 2015') %>%
  setView(lemhi_leaflet, lng = -113.62725, lat = 44.86833, zoom = 13) %>%
  addLegend(position = 'topright', 
            colors = c('black', 'green', 'aquamarine', 'gray', 
                       'gold', 'darkred', 'blue', 'red'),
            labels = c('Project Outlines',
                       'Chinook Observations',
                       'Steelhead Observations',
                       'Bull Trout Observations',
                       'Cutthroat Trout Observations',
                       'Brook Trout Observatoins',
                       'Juvenile Chinook Densities 2015 - 2018',
                       'Redds 1999 - 2015')) %>%
  addLayersControl(baseGroups = c('Imagery', 'Topo', 'Esri'),
                   overlayGroups = c('Beyeler Project Outline', 'EVR SR1 Outline', 'Split River Project Outline',
                                     'Last Chance Project Outline', 'Whitworth Outline',
                                     'Chinook Obs', 'Steelhead Obs', 'Bull Obs', 'Cutthroat Obs', 'Brook Obs',
                                     'Chinook Densities 2015', 'Chinook Densities 2016',
                                     'Chinook Densities 2017', 'Chinook Densities 2018',
                                     'Redds 1999', 'Redds 2000',
                                     'Redds 2001', 'Redds 2002', 'Redds 2003', 'Redds 2004',
                                     'Redds 2005', 'Redds 2006', 'Redds 2007', 'Redds 2008',
                                     'Redds 2009', 'Redds 2010', 'Redds 2011', 'Redds 2012',
                                     'Redds 2013', 'Redds 2014', 'Redds 2015')) %>%
  hideGroup(c('Bull Obs', 'Cutthroat Obs', 'Brook Obs',
              'Last Chance Project Outline', 'Whitworth Outline',
              'Chinook Densities 2015', 'Chinook Densities 2016', 'Chinook Densities 2017',
              'Chinook Densities 2018',
              'Redds 1999', 'Redds 2000', 'Redds 2001', 'Redds 2002', 'Redds 2003', 'Redds 2004',
              'Redds 2005', 'Redds 2006', 'Redds 2007', 'Redds 2008', 'Redds 2009', 'Redds 2010',
              'Redds 2011', 'Redds 2012', 'Redds 2013', 'Redds 2014'))
restoration_leaflet
# save leaflet
path = 'leaflets/restoration_leaflet.html'
saveWidget(restoration_leaflet, file.path(normalizePath(dirname(path)),basename(path)))
