# -----------------------
# Author: Mike Ackerman
# Purpose: Create leaflet map to support Snake R. IPTDS prioritization and planning.
#   Contains information on recommendations for continued funding, decommission, candidates for project,
#   upgrades, etc. to aid in planning and discussions.
# 
# Created: April 11, 2024
#   Last Modified: 
# 
# Notes: Much of this is based on a previous script iptds_planning.R from RK

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)
library(readxl)
library(sf)
library(leaflet)
# library(htmlwidgets)

# -----------------------
# COMPILE DATA

# current iptds data
iptds_cur = read_excel(here("docs/Snake River IPTDS Prioritization 20240410.xlsx"),
                       sheet = "SR_IPTDS_Sites") %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)
# iptds recommendations
iptds_rec = read_excel(here("data/derived_data/proposed_iptds_sites_20240411.xlsx"),
                       sheet = "Sheet1") %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)
  
# populations
load(here("data/derived_data/spatial/SR_pops.rda")) ; rm(fall_pop)
sthd_pops = sth_pop %>%
  select(TRT_POPID, POP_NAME, MPG) ; rm(sth_pop)

chnk_pops = spsm_pop %>%
  select(TRT_POPID, POP_NAME, MPG) ; rm(spsm_pop)

# traps and weirs
load(here("data/derived_data/spatial/sr_traps_weirs.rda"))

# streams and steelhead major/minor spawning areas
load(here("data/derived_data/spatial/steelhead_gis_data.rda"))
streams = sthd_critical %>%
  st_transform("EPSG:4326")

sthd_spawn = sthd_spawn %>%
  st_transform("EPSG:4326") ; rm(sthd_critical, sthd_extant, sthd_ip)

# chinook major/minor spawning areas
chnk_spawn = readRDS(here("data/derived_data/spatial/spsm_spwn_areas.rds")) %>%
  st_transform("EPSG:4326")

# -----------------------
# SET SOME COLORS
sthd_mpg_col = colorFactor(palette = "Dark2", domain = sthd_pops$MPG)
sthd_spawn_col = colorFactor(palette = c('skyblue','navy'), domain = sthd_spawn$TYPE, reverse = TRUE)
chnk_mpg_col = colorFactor(palette = "Set1", domain = chnk_pops$MPG)
chnk_spawn_col = colorFactor(palette = c('springgreen','darkgreen'), domain = chnk_spawn$TYPE, reverse = TRUE)
rec_col = colorFactor(palette = c("black", "blue", "green", "red"), domain = iptds_rec$recommendation)

#int_om_col = colorFactor(c("gray", "red"), domain = iptds$integrated_om_site)
#status_and_trends_col = colorFactor(c('red', 'orange', 'yellow'), domain = iptds$adult_status_trends)
#funding_col <- colorFactor(palette = 'Set3', domain = iptds$bpa_funding)

# -----------------------
# BUILD LEAFLET
base = leaflet() %>%
  # base maps
  setView(lng = -115.5660, lat = 45.4000, zoom = 7.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolylines(data = streams, color = "blue", weight = 1)

sr_iptds_leaflet = base %>%
  # steelhead populations
  addPolygons(data = sthd_pops,
              group = "Steelhead Populations",
              fillColor = ~sthd_mpg_col(MPG),
              fillOpacity = 0.2,
              stroke = T,
              weight = 2,
              color = "black",
              opacity = 1,
              label = ~paste0(TRT_POPID, ": ", POP_NAME),
              popup = paste("<b>Steelhead</b></br>",
                            "<b>Pop ID:</b>", sthd_pops$TRT_POPID, "</br>",
                            "<b>Pop Name:</b>", sthd_pops$POP_NAME, "</br>",
                            "<b>MPG:</b>", sthd_pops$MPG, "</br>")) %>%
  # steelhead major/minor spawning areas
  addPolygons(data = sthd_spawn,
              group = "Steelhead Spawning Areas",
              fillColor = ~sthd_spawn_col(TYPE),
              fillOpacity = 0.2,
              stroke = T,
              weight = 1,
              color = "black",
              opacity = 1,
              label = ~paste0(POP_NAME, ": ", MSA_NAME)) %>%
  # chinook salmon populations
  addPolygons(data = chnk_pops,
              group = "Sp/Sum Chinook Populations",
              fillColor = ~chnk_mpg_col(MPG),
              fillOpacity = 0.2,
              stroke = T,
              weight = 2,
              color = "black",
              opacity = 1,
              label = ~paste0(TRT_POPID, ": ", POP_NAME),
              popup = paste("<b>sp/sum Chinook salmon</b></br>",
                            "<b>Pop ID:</b>", chnk_pops$TRT_POPID, "</br>",
                            "<b>Pop Name:</b>", chnk_pops$POP_NAME, "</br>",
                            "<b>MPG:</b>", chnk_pops$MPG, "</br>")) %>%
  # chinook major/minor spawning areas
  addPolygons(data = chnk_spawn,
              group = "Sp/Sum Chinook Spawning Areas",
              fillColor = ~chnk_spawn_col(TYPE),
              fillOpacity = 0.2,
              stroke = T,
              weight = 1,
              color = "black",
              opacity = 1,
              label = ~paste0(POP_NAME, ": ", MSA_NAME)) %>%
  # add iptds recommendations
  addCircles(data = iptds_rec,
             group = "IPTDS Recommendations",
             label = ~site_code,
             color = ~rec_col(recommendation),
             opacity = 1,
             weight = 10,
             popup = paste("<b>Site Code:</b>", iptds_rec$site_code, "</br>",
                           "<b>Recommendation:</b>", iptds_rec$recommendation, "</br>",
                           "<b>Priority:</b>", iptds_rec$action_priority, "</br>",
                           "<b>Potential Upgrades:</b>", iptds_rec$upgrades, "</br>",
                           "<b>Notes:</b>", iptds_rec$notes, "</br>")) %>%
  # control layers
  addLayersControl(baseGroups = c("Steelhead Populations",
                                  "Steelhead Spawning Areas",
                                  "Sp/Sum Chinook Populations",
                                  "Sp/Sum Chinook Spawning Areas"),
                   overlayGroups = c("IPTDS Recommendations"),
                   options = layersControlOptions(collapsed = FALSE))
sr_iptds_leaflet


sr_iptds_leaflet = base %>%
  # chinook salmon populations
  addPolygons(data = chnk_pops,
              group = "Chinook Salmon Populations",
              fillColor = ~chnk_mpg_col(MPG),
              #fillColor = "blue",
              fillOpacity = 0.2,
              stroke = T,
              weight = 2,
              color = "black",
              opacity = 1,
              label = ~paste0(TRT_POPID, ": ", POP_NAME),
              popup = paste("<b>sp/sum Chinook salmon</b></br>",
                            "<b>Pop ID:</b>", chnk_pops$TRT_POPID, "</br>",
                            "<b>Pop Name:</b>", chnk_pops$POP_NAME, "</br>",
                            "<b>MPG:</b>", chnk_pops$MPG, "</br>")) %>%
  # steelhead populations
  addPolygons(data = sthd_pops,
              group = "Steelhead Populations",
              fillColor = ~sthd_mpg_col(MPG),
              #fillColor = "red",
              fillOpacity = 0.2,
              stroke = T,
              weight = 2,
              color = "black",
              opacity = 1,
              label = ~paste0(TRT_POPID, ": ", POP_NAME),
              popup = paste("<b>Steelhead</b></br>",
                            "<b>Pop ID:</b>", sthd_pops$TRT_POPID, "</br>",
                            "<b>Pop Name:</b>", sthd_pops$POP_NAME, "</br>",
                            "<b>MPG:</b>", sthd_pops$MPG, "</br>")) %>%
  # chinook major/minor spawning areas
  addPolygons(data = chnk_spawn,
              group = "Chinook Spawning Areas",
              fillColor = ~chnk_spawn_col(TYPE),
              fillOpacity = 0.2,
              stroke = T,
              weight = 1,
              color = "black",
              opacity = 1,
              label = ~paste0(POP_NAME, ": ", MSA_NAME)) %>%
  # steelhead major/minor spawning areas
  addPolygons(data = sthd_spawn_wgs84,
              group = "Steelhead Spawning Areas",
              fillColor = ~sthd_spawn_col(TYPE),
              fillOpacity = 0.2,
              stroke = T,
              weight = 1,
              color = "black",
              opacity = 1,
              label = ~paste0(POP_NAME, ": ", MSA_NAME)) %>%
  # add iptds site information
  addCircles(data = iptds,
             group = "All IPTDS Sites",
             label = ~site_code,
             color = "black",
             opacity = 1,
             weight = 10,
             popup = paste("<b>Site Code:</b>", iptds$site_code, "</br>",
                           "<b>Site Name:</b>", iptds$site_name, "</br>",
                           "<b>Stream:</b>", iptds$stream, "</br>",
                           "<b>Node Count:</b>", iptds$node_count, "</br>",
                           "<b>Antenna Count:</b>", iptds$antenna_count, "</br>",
                           "<b>PTAGIS Active:</b>", iptds$ptagis_active, "</br>",
                           "<b>Detection Probabilities:</b>", iptds$detection_prob, "</br>",
                           "<b>Biomark Integrated O&M Site:</b>", iptds$integrated_om_site, "</br>",
                           "<b>Status and Trends:</b>", iptds$adult_status_trends, "</br>",
                           "<b>Current Funding:</b>", iptds$current_funding, "</br>",
                           "<b>BPA Funding Type:</b>", iptds$bpa_funding, "</br>",
                           "<b>O&M Agency:</b>", iptds$om_agency, "</br>",
                           "<b>Site Description:</b>", iptds$site_description, "</br>")) %>%
  # color by integrated O&M contract
  addCircleMarkers(data = iptds,
                   group = "Integrated O&M Sites",
                   color = ~int_om_col(integrated_om_site)) %>%
  # color by status and trends tier
  addCircleMarkers(data = iptds,
                   group = "Status and Trends",
                   color = ~status_and_trends_col(adult_status_trends)) %>%
  addCircleMarkers(data = iptds,
                   group = "O&M Funding Source(s)",
                   color = ~funding_col(bpa_funding)) %>%
  addCircles(data = sr_traps_weirs,
             group = "Traps and Weirs",
             label = ~paste0(site_code, ": ", name),
             color = "hotpink",
             opacity = 1,
             weight = 5) %>%
  # control layers
  addLayersControl(baseGroups = c("Chinook Salmon Populations",
                                  "Chinook Spawning Areas",
                                  "Steelhead Populations",
                                  "Steelhead Spawning Areas"),
                   overlayGroups = c("All IPTDS Sites",
                                     "Integrated O&M Sites",
                                     "Status and Trends",
                                     "O&M Funding Source(s)",
                                     "Traps and Weirs"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  # add legends
  addLegend(data = chnk_spawn,
            position = "bottomleft",
            pal = chnk_spawn_col,
            values = ~TYPE,
            title = "Spawning Area Type",
            group = "Chinook Spawning Areas",
            opacity = 0.5) %>%
  addLegend(data = sthd_spawn_wgs84,
            position = "bottomleft",
            pal = sthd_spawn_col,
            values = ~TYPE,
            title = "Spawning Area Type",
            group = "Steelhead Spawning Areas",
            opacity = 0.5) %>%
  addLegend(data = iptds, 
            position = "bottomleft",
            pal = int_om_col,
            values = ~integrated_om_site,
            title = "Integrated O&M Sites",
            group = "Integrated O&M Sites",
            opacity = 0.5) %>%
  addLegend(data = iptds, 
            position = "bottomleft",
            pal = status_and_trends_col,
            values = ~adult_status_trends,
            title = "Status and Trends",
            group = "Status and Trends",
            opacity = 0.5) %>%
  addLegend(data = iptds, 
            position = "bottomleft",
            pal = funding_col,
            values = ~bpa_funding,
            title = "O&M Funding Source(s)",
            group = "O&M Funding Source(s)",
            opacity = 0.5) %>%
  hideGroup("Integrated O&M Sites") %>%
  hideGroup("Status and Trends") %>%
  hideGroup("O&M Funding Source(s)") %>%
  hideGroup("Traps and Weirs") %>%
  addMiniMap()

sr_iptds_leaflet

# save leaflet
saveWidget(sr_iptds_leaflet, file = here("shiny/leaflet/sr_iptds_leaflet.html"))

### END SCRIPT
