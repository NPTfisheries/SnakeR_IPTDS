# -----------------------
# Author: Mike Ackerman
# Purpose: Query Snake River PIT tag MRR and INT sites from PTAGIS
# 
# Created: June 6, 2023
# Last Modified:
# Notes:
# -----------------------

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(janitor)
library(here)
library(sf)

# load PITcleanr
# remotes::install_github("mackerman44/PITcleanr@main", build_vignettes = T, force = T)
library(PITcleanr)
# browseVignettes("PITcleanr")

# build original configuration table from PTAGIS (requires interwebs connection); queries all INT and MRR sites in PTAGIS
og_config = buildConfig() 

# filter to only Snake River sites including IPTDS and adult fishways (CCW & IML)
sr_config = og_config %>%
  filter(str_starts(rkm, "522"),
         site_type == "INT",
         site_type_name %in% c("Instream Remote Detection System",
                               "Adult Fishway"))

# compile Snake River nodes; nodes typically = arrays
sr_nodes = sr_config %>%
  select(site_code,
         site_name,
         node,
         antenna_id,
         antenna_group,
         config_id)

# write_csv of nodes
write_csv(sr_nodes,
          file = here("data/derived_data/Snake_PTAGIS_IPTDS_nodes_20230609.csv"))

# nodes per site
# sr_nodes %>%
#   select(site_code,
#          node) %>%
#   distinct() %>%
#   group_by(site_code) %>%
#   summarize(node_count = n()) # %>%

# antennas per site
# sr_nodes %>%
#   select(site_code,
#          antenna_id) %>%
#   distinct() %>%
#   group_by(site_code) %>%
#   summarize(antenna_count = n())

# summarize Snake River sites
sr_sites = sr_config %>%
  # get miniumum start and maximum end dates among antennas within a site
  group_by(site_code) %>%
  mutate(min_start_date = as_date(min(start_date, na.rm = T)),
         max_end_date = as_date(max(end_date, na.rm = T))) %>%
  mutate(max_end_date = as_date(ifelse(max_end_date == -Inf, NA, max_end_date))) %>%
  mutate(node_count = n_distinct(node),
         antenna_count = n_distinct(antenna_id)) %>%
  select(site_code, 
         site_name,
         site_type_name,
         latitude, 
         longitude, 
         rkm, 
         rkm_total,
         node_count,
         antenna_count,
         min_start_date, 
         max_end_date, 
         site_description) %>%
  # trim down to just sites, not antennas
  distinct(site_code, .keep_all = T) %>%
  arrange(rkm)

# append SR ICTRT population names to sr_sites
load(here("data/derived_data/spatial/SR_pops.rda"))

sr_chnk_pops = st_as_sf(spsm_pop) %>%
  select(chnk_esu = ESU_DPS,
         chnk_mpg = MPG,
         chnk_trt_popid = TRT_POPID,
         chnk_pop_name = POP_NAME)

sr_sthd_pops = st_as_sf(sth_pop) %>%
  select(sthd_dps = ESU_DPS,
         shtd_mpg = MPG,
         sthd_trt_popid = TRT_POPID,
         sthd_pop_name = POP_NAME)

sr_sites = sr_sites %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%
  st_join(sr_chnk_pops) %>%
  st_join(sr_sthd_pops) %>%
  st_drop_geometry() %>%
  # and fix some designations
  mutate(across(c(chnk_trt_popid, chnk_pop_name), ~ifelse(grepl("^SW1$|^SW2$", site_code), "SEUMA/SEMEA/SEMOO", .))) %>%
  mutate(chnk_trt_popid = ifelse(grepl('^SC1$|^SC2$', site_code), 'SCUMA', chnk_trt_popid)) %>%
  mutate(chnk_pop_name = ifelse(grepl('^SC1$|^SC2$', site_code), 'Upper South Fork Clearwater', chnk_pop_name)) %>%
  mutate(sthd_trt_popid = ifelse(grepl('^SC1$|^SC2$', site_code), 'CRSFC-s', sthd_trt_popid)) %>%
  mutate(chnk_pop_name = ifelse(grepl('^SC1$|^SC2$', site_code), 'South Fork Clearwater River', chnk_pop_name))

# write_csvs
write_csv(sr_sites,
          file = here("data/derived_data/Snake_PTAGIS_IPTDS_sites_20230620.csv"))

# spsm_pop %>%
#   st_drop_geometry() %>%
#   write_csv(file = here("docs/Snake_R_spsum_Chinook_TRT_pops.csv"))
# 
# sth_pop %>%
#   st_drop_geometry() %>%
#   write_csv(file = here("docs/Snake_R_steelhead_TRT_pops.csv"))

# query MRR sites
sr_traps_weirs = queryMRRMeta() %>%
  clean_names() %>%
  filter(str_starts(rkm, "522"),
         type == "TraporWeir") %>%
  select(site_code,
         name,
         latitude,
         longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)

# save mrr_sites
save(sr_traps_weirs, file = here("data/derived_data/spatial/sr_traps_weirs.rda"))

