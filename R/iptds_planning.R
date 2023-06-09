# Purpose: Create maps for IPTDS prioritization and planning.
# Author: Ryan N. Kinzer
# Created: 6/2/21
# Modified: 5/16/22
# Notes: Section 3 - contains the most up to date gis layers and approaches for 
# constructing maps. The section also provides the backbone of the IPTDS
# prioritization application.

# Load needed libraries -----
library(tidyverse)
library(sf)
#library(ggmap)
#library(maps)
library(leaflet)
library(leafpop)

#library(albersusa)
#library(scales)

# Section 1----
# Gather Data

# get states
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

pnw <- states %>% filter(ID %in% c('idaho', 'oregon', 'washington')) %>%
  st_transform(crs = 4326)

# load polygons
load('./data/polygons/SR_pops.rda') # populations, spsm_pop, fall_pop, sth_pop

# Subset Pops/Remove NF Clearwater ----
sth_pop <- sth_pop %>%
  filter(TRT_POPID != 'CRNFC-s')

spsm_pop <- spsm_pop %>%
  filter(!grepl('NC', TRT_POPID))

# load rivers and trim
load("./data/flowlines/large_rivers.rda")
load("./data/flowlines/SR_streams.rda")

pnw_rivers <- st_intersection(pnw_rivers %>% 
                                st_transform(crs = 4326), sth_pop)

snake_rivers <- st_intersection(snake_rivers %>%
                                  st_transform(crs = 4326), sth_pop)

# load ictrt IP flowlines
st_read()

# load points and metadata----
#configuration <- PITcleanr::buildConfig()
#save(configuration, file = './data/points/site_config.rda')
load('./data/points/site_config.rda')


config <- configuration %>%
  filter(!is.na(longitude)) %>%
  #filter(site_type == 'INT') %>%
  #filter(site_type_name %in% c('Instream Remote Detection System', 'Adult Fishway', 'Hatchery Returns')) %>%
  group_by(site_code) %>%
  slice(which.max(start_date)) %>%
  arrange(desc(start_date))%>%
  select(contains('site'), contains('rkm'), start_date, end_date, latitude, longitude) %>%
  distinct() %>%
  mutate(current_op = case_when(
    end_date < Sys.Date() ~ FALSE,
    TRUE ~ TRUE))

site_meta <- readxl::read_excel('../../DFRM Research Division/PIT_Array/Draft Proposals/PIT_array_project_planning_v5.xlsx', sheet = 'IPTDS_Sites')

#site_meta <- read_csv('./data/points/site_metadata.csv')

sites <- config %>%
  inner_join(site_meta, by = c('site_code' = 'site')) %>%
  st_as_sf(coords = c('longitude','latitude'),
           crs = 4326)

integrated_sites <- sites %>%
  filter(integrated_contract)

operational_sites <- sites %>%
  filter(operational)

ptagis_ops <- sites %>%
  filter(current_op)

anti_join(ptagis_ops, operational_sites %>% st_set_geometry(NULL), by = 'site_code')
anti_join(operational_sites, ptagis_ops %>% st_set_geometry(NULL), by = 'site_code')

pop_sp <- as_Spatial(sth_pop)
b <- sp::bbox(pop_sp) # get bounding box

map_center <- apply(b,1,mean)

p <- ggmap(get_googlemap(center = map_center, #c(lon = -122.335167, lat = 47.608013), #center = map_center,
                         zoom = 7, scale = 2,
                         maptype ='satellite',
                         color = 'color'))

# Checkout package 'basemap'
# library(basemaps)
# data(ext)
# p <- ggplot() +
#   basemap_gglayer(ext)
# 
# pop_ext <- st_bbox(sth_pop)
# tmp <- basemap(ext = ext, map_service = 'esri', may_type = 'world_topo_map', class = 'ggplot')

iptds_map <- p +
  geom_sf(data = sth_pop, inherit.aes = FALSE, alpha = .5) +
  geom_sf(data = snake_rivers, colour = 'cyan', inherit.aes = FALSE) +
  geom_sf(data = sites, aes(colour = current_om_funding, shape = operational), size = 2, inherit.aes = FALSE) +
  #scale_colour_manual(values = c('firebrick', 'darkgreen', 'grey10')) +
  theme_void() +
  theme(text = element_text(family = 'serif'),
        plot.title = element_text(face = 'bold'),
        plot.subtitle = element_text(colour = 'grey35'),
        legend.position = c(.15,.25),
        legend.background = element_rect(fill = alpha('grey75', .85))) +
  labs(title = "Snake River Basin PIT-tag Detection Locations and ICTRT Steelhead Populations",
       subtitle = "IPTDS sites necessary for primary populations (n = 24), secondary populations (n = 17), and \n sub-population (n = 49) estimates.",
       colour = ' Abundance Objective',
       shape = ' Detection Type')

iptds_map
ggsave('./maps/iptds_prioritization.png', iptds_map, width = 7.5, height = 7.5)


# Section 2----
# Interacitve Map

# basemap
l <- leaflet() %>%
  setView(lng = -115.5660,
          lat = 45.4000,#44.9218,
          zoom = 7.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap)%>%
  addPolylines(data = pnw_rivers, color = 'blue', weight = 1) %>%
  addPolylines(data = snake_rivers, color = 'blue', weight = 1)

# assign icons

# rst_icon <- makeIcon(iconUrl = "C:/Users/ryank/Pictures/icons/red_circle.png",
#                      iconWidth = 18)
# 
# weir_icon <- makeIcon(iconUrl = "C:/Users/ryank/Pictures/icons/blue_square.png",
#                       iconWidth = 18)
# 
# iptds_icon <- makeIcon(iconUrl = "C:/Users/ryank/Pictures/icons/green_triangle.png",
#                        iconWidth = 16)
# 
# icon_legend <- "<img src='image/red_circle.png'>Rotary Screw Trap<br/>
# <img src='image/blue_square.png'>Adult Weir<br/>
# <img src='image/green_triangle.png'>Instream Detection System"
# 
# obs_icon <- iconList(
#   red_circle <- rst_icon,
#   blue_square <- weir_icon,
#   green_triangle <- iptds_icon
# )

# add colors

sth_col <- colorFactor(palette = 'viridis', domain = sth_pop$MPG)
spsm_col <- colorFactor(palette = 'viridis', domain = spsm_pop$MPG)
#sthd_release_col <- colorFactor(palette = 'BuPl', domain = sthd_release$Species)
tier_col <- colorFactor(c('firebrick', 'darkorange', 'gold'), domain = sites$pop_tier)
funding_col <- colorFactor(palette = 'Dark2', domain = sites$BPA_funded)


iptds_leaflet_map <- l %>%
  # add steelhead
  addPolygons(data = sth_pop, group = 'Steelhead Populations',
              fillColor = ~sth_col(MPG),
              fillOpacity = .2
              ,stroke = TRUE, weight = 2, color = 'black', opacity = 1
              ,label = ~TRT_POPID
  ) %>%
  addPolygons(data = spsm_pop, group = 'Chinook Populations',
              fillColor = ~spsm_col(MPG),
              fillOpacity = .2
              ,stroke = TRUE, weight = 3,color = 'black', opacity = 1
              ,label = ~TRT_POPID
  ) %>%
  # add markers
  addCircleMarkers(data = sites, group = "Population Priority", label = ~site_code,
                   color = ~tier_col(pop_tier),
             popup = paste("<b>Site Name:</b>",sites$site_name,"</br>",
                           "<b>Site Code:</b>",sites$site_code,"</br>",
                           "<b>Population Monitoring Tier:</b>",sites$pop_tier,"</br>",
                           "<b>Current Funding:</b>",sites$current_om_funding,"</br>",
                           "<b>BPA Funding Type:</b>",sites$BPA_funded,"</br>",
                           "<b>Biomark Integrated Contract:</b>",sites$integrated_contract,"</br>",
                           "<b>Agency O&M:</b>",sites$om_agency,"</br>",
                           "<b>Operational:</b>",sites$operational,"</br>",
                           "<b>Number of Nodes:</b>",sites$node_count,"</br>",
                           "<b>Detection Probabilities:</b>",sites$detection_prob,"</br>")) %>%
  addCircleMarkers(data = sites, group = "O&M Funding Sources", label = ~site_code,
                   color = ~funding_col(BPA_funded),
                   popup = paste("<b>Site Name:</b>",sites$site_name,"</br>",
                                 "<b>Site Code:</b>",sites$site_code,"</br>",
                                 "<b>Population Monitoring Tier:</b>",sites$pop_tier,"</br>",
                                 "<b>Current Funding:</b>",sites$current_om_funding,"</br>",
                                 "<b>BPA Funding Type:</b>",sites$BPA_funded,"</br>",
                                 "<b>Biomark Integrated Contract:</b>",sites$integrated_contract,"</br>",
                                 "<b>Agency O&M:</b>",sites$om_agency,"</br>",
                                 "<b>Operational:</b>",sites$operational,"</br>",
                                 "<b>Number of Nodes:</b>",sites$node_count,"</br>",
                                 "<b>Detection Probabilities:</b>",sites$detection_prob,"</br>")) %>%
  addCircles(data = integrated_sites, group = "Integrated O&M Sites", label = ~site_code,
             color = 'black', radius = 20) %>%
  addCircles(data = operational_sites, group = "Operational Sites", label = ~site_code,
             color = 'black', radius = 20) %>%
  addLayersControl(
    overlayGroups = c("Steelhead Populations",
                      "Chinook Populations",
                      "O&M Funding Sources",
                      "Population Priority",
                      "Integrated O&M Sites",
                      "Operational Sites"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # addControl(html = "<img src='image\red_circle.png'/>Rotary Screw Trap", position = 'topleft') %>% 
  addLegend(data = sites, position = "bottomleft", pal = tier_col, values = ~pop_tier,
            title = "Population Monitoring Priority",
            group = 'Population Priority',
            opacity = .5) %>%
  addLegend(data = sites, position = "bottomleft", pal = funding_col, values = ~BPA_funded,
            title = "O&M Funding Sources",
            group = 'O&M Funding Sources',
            opacity = .5) %>%
  hideGroup('Chinook Populations') %>%
  hideGroup('Population Priority') %>%
  hideGroup('Integrated O&M Sites') %>%
  hideGroup('Operational Sites') %>%
  addMiniMap()

iptds_leaflet_map

path <- file.path(getwd(), "maps", "iptds_pop_prioritization.html")
htmlwidgets::saveWidget(iptds_leaflet_map, file = path)


# Section 3----
# load data points

# Load needed libraries -----
library(tidyverse)
library(sf)
#library(ggmap)
#library(maps)
library(leaflet)
library(leafpop)

load('./data/points/site_config.rda')

config <- configuration %>%
  filter(!is.na(longitude)) %>%
  #filter(site_type == 'INT') %>%
  #filter(site_type_name %in% c('Instream Remote Detection System', 'Adult Fishway', 'Hatchery Returns')) %>%
  group_by(site_code) %>%
  slice(which.max(start_date)) %>%
  arrange(desc(start_date))%>%
  select(contains('site'), contains('rkm'), start_date, end_date, latitude, longitude) %>%
  distinct() %>%
  mutate(current_op = case_when(
    end_date < Sys.Date() ~ FALSE,
    TRUE ~ TRUE))

site_meta <- readxl::read_excel('../../DFRM Research Division/PIT_Array/Site_Priority/PIT_array_project_planning_v5.xlsx', sheet = 'IPTDS_Sites')

sites <- config %>%
  inner_join(site_meta, by = c('site_code' = 'site')) %>%
  st_as_sf(coords = c('longitude','latitude'),
           crs = 4326)

integrated_sites <- sites %>%
  filter(integrated_contract)

operational_sites <- sites %>%
  filter(operational)

ptagis_ops <- sites %>%
  filter(current_op)

# load data points
load('C:/ryank/DFRM Projects/River_Mapping/data/steelhead_gis_data.rda')

# transform to WGS84 for leaflet map
sthd_critical_WGS84 <- sthd_critical %>%
  st_transform('EPSG:4326')

sthd_extant_WGS84 <- sthd_extant %>%
  group_by(SPECIES, RUN_TIMING, ESU_DPS, MPG, POP_NAME, TRT_POPID, POP_TYPE, ACCESS_POP) %>%
  summarise(SHAPE_Area = sum(SHAPE_Area)) %>%
  #filter(ACCESS_HUC == 'accessible') %>%
  filter(TRT_POPID != 'CRNFC-s') %>%
  st_transform('EPSG:4326')

#plot(sthd_extant_WGS84['ACCESS_POP'])

sthd_spawn_WGS84 <- sthd_spawn %>%
  st_transform('EPSG:4326')

#glimpse(sthd_spawn_WGS84)

# tmp1 <- sthd_extant_WGS84 %>%
#   st_set_geometry(NULL) %>%
#   select(MPG, POP_NAME, TRT_POPID) %>%
#   arrange(MPG, POP_NAME, TRT_POPID)
# 
# tmp2 <- sthd_spawn_WGS84 %>%
#   st_set_geometry(NULL) %>%
#   select(ESU_NAME, MPG_NAME, POP_NAME, TYPE, MSA_NAME) %>%
#   arrange(MPG = MPG_NAME, TRT_POPID = POP_NAME, TYPE, MSA_NAME)
# 
# list('populations' = tmp1,
#      'spawning_areas' = tmp2) %>%
# writexl::write_xlsx(path = './sthd_population_priorities.xlsx')

# set colors
sth_col <- colorFactor(palette = 'viridis', domain = sthd_extant_WGS84$MPG)
spawn_col <- colorFactor(palette = c('skyblue','navy'), domain = sthd_spawn_WGS84$TYPE, reverse = TRUE)
funding_col <- colorFactor(palette = 'Dark2', domain = sites$BPA_funded)
tier_col <- colorFactor(c('firebrick', 'darkorange', 'gold'), domain = sites$pop_tier)

# basemap
l <- leaflet() %>%
  setView(lng = -115.5660,
          lat = 45.4000,#44.9218,
          zoom = 7.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolylines(data = sthd_critical_WGS84, color = 'blue', weight = 1) %>%
  addPolylines(data = sthd_extant_WGS84, color = 'black', opacity = 1, weight = 2)


iptds_leaflet_map <- l %>%
  # add steelhead
  addPolygons(data = sthd_extant_WGS84, group = 'Steelhead Populations',
              fillColor = ~sth_col(MPG),
              fillOpacity = .2,
              stroke = TRUE, weight = 2, color = 'black', opacity = 1,
              label = ~TRT_POPID
  ) %>%
  addPolygons(data = sthd_spawn_WGS84, group = 'Spawning Areas',
              fillColor = ~spawn_col(TYPE),
              fillOpacity = .2,
              stroke = TRUE, weight = 1, color = 'black', opacity = 1,
              label = ~paste0(POP_NAME, ' - ',MSA_NAME)
  ) %>%
  # add markers
  addCircleMarkers(data = sites, group = "Population Priority", label = ~site_code,
                   color = ~tier_col(pop_tier),
                   popup = paste("<b>Site Name:</b>",sites$site_name,"</br>",
                                 "<b>Site Code:</b>",sites$site_code,"</br>",
                                 "<b>Population Monitoring Tier:</b>",sites$pop_tier,"</br>",
                                 "<b>Current Funding:</b>",sites$current_om_funding,"</br>",
                                 "<b>BPA Funding Type:</b>",sites$BPA_funded,"</br>",
                                 "<b>Biomark Integrated Contract:</b>",sites$integrated_contract,"</br>",
                                 "<b>Agency O&M:</b>",sites$om_agency,"</br>",
                                 "<b>Operational:</b>",sites$operational,"</br>",
                                 "<b>Number of Nodes:</b>",sites$node_count,"</br>",
                                 "<b>Detection Probabilities:</b>",sites$detection_prob,"</br>")) %>%
  addCircleMarkers(data = sites, group = "O&M Funding Sources", label = ~site_code,
                   color = ~funding_col(BPA_funded),
                   popup = paste("<b>Site Name:</b>",sites$site_name,"</br>",
                                 "<b>Site Code:</b>",sites$site_code,"</br>",
                                 "<b>Population Monitoring Tier:</b>",sites$pop_tier,"</br>",
                                 "<b>Current Funding:</b>",sites$current_om_funding,"</br>",
                                 "<b>BPA Funding Type:</b>",sites$BPA_funded,"</br>",
                                 "<b>Biomark Integrated Contract:</b>",sites$integrated_contract,"</br>",
                                 "<b>Agency O&M:</b>",sites$om_agency,"</br>",
                                 "<b>Operational:</b>",sites$operational,"</br>",
                                 "<b>Number of Nodes:</b>",sites$node_count,"</br>",
                                 "<b>Detection Probabilities:</b>",sites$detection_prob,"</br>")) %>%
  addCircles(data = operational_sites, group = "Operational Sites", label = ~site_code,
             color = 'black', opacity = 1, radius = 50, weight = 10, fill = TRUE,
             fillColor = 'black', fillOpacity = 1, stroke = TRUE,
             popup = paste("<b>Site Name:</b>",sites$site_name,"</br>",
                           "<b>Site Code:</b>",sites$site_code,"</br>",
                           "<b>Population Monitoring Tier:</b>",sites$pop_tier,"</br>",
                           "<b>Current Funding:</b>",sites$current_om_funding,"</br>",
                           "<b>BPA Funding Type:</b>",sites$BPA_funded,"</br>",
                           "<b>Biomark Integrated Contract:</b>",sites$integrated_contract,"</br>",
                           "<b>Agency O&M:</b>",sites$om_agency,"</br>",
                           "<b>Operational:</b>",sites$operational,"</br>",
                           "<b>Number of Nodes:</b>",sites$node_count,"</br>",
                           "<b>Detection Probabilities:</b>",sites$detection_prob,"</br>")) %>%
  addCircles(data = integrated_sites, group = "Integrated O&M Sites", label = ~site_code,
             color = 'white', radius = 20, fillOpacity = 1) %>%

  addLayersControl(
    baseGroups = c('Steelhead Populations',
                   'Spawning Areas'),
    overlayGroups = c('Spawning Areas',
                      "Population Priority",
                      "Operational Sites",
                      "O&M Funding Sources",
                      "Integrated O&M Sites"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # addLegend(data = sthd_extant_WGS84, position = "bottomleft", pal = sth_col, values = ~MPG,
  #           title = "Major Population Groups",
  #           group = 'Steelhead Populations',
  #           opacity = .5) %>% 
  addLegend(data = sthd_spawn_WGS84, position = "bottomleft", pal = spawn_col, values = ~TYPE,
            title = "Spawning Area Type",
            group = 'Spawning Areas',
            opacity = .5) %>%  
  addLegend(data = sites, position = "bottomleft", pal = tier_col, values = ~pop_tier,
            title = "Population Monitoring Priority",
            group = 'Population Priority',
            opacity = .5) %>%
  addLegend(data = sites, position = "bottomleft", pal = funding_col, values = ~BPA_funded,
            title = "O&M Funding Sources",
            group = 'O&M Funding Sources',
            opacity = .5) %>%
  hideGroup('Spawning Areas') %>%
  hideGroup('O&M Funding Sources') %>%
  hideGroup('Population Priority') %>%
  hideGroup('Integrated O&M Sites') %>%
  #hideGroup('Operational Sites') %>%
  addMiniMap()

iptds_leaflet_map

path <- file.path(getwd(), "maps", "iptds_pop_prioritization_v2.html")
htmlwidgets::saveWidget(iptds_leaflet_map, file = path)
