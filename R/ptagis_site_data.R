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
# library(sf)
# library(janitor)

# load PITcleanr
remotes::install_github("mackerman44/PITcleanr@main", build_vignettes = T, force = T)
library(PITcleanr)
browseVignettes("PITcleanr")

# build original configuration table from PTAGIS (requires interwebs connection); queries all INT and MRR sites in PTAGIS
og_config = buildConfig() 

# filter to only Snake River sites and IPTDS
sr_config = og_config %>%
  filter(str_starts(rkm, "522"),
         site_type == "INT",
         site_type_name == "Instream Remote Detection System") %>%
  # get miniumum start and maximum end dates among antennas within a site
  group_by(site_code) %>%
  mutate(min_start_date = as_date(min(start_date, na.rm = T)),
         max_end_date = as_date(max(end_date, na.rm = T))) %>%
  mutate(max_end_date = as_date(ifelse(max_end_date == -Inf, NA, max_end_date))) %>%
  #select(-start_date, -end_date) %>%
  select(site_code, 
         site_name, 
         latitude, 
         longitude, 
         rkm, 
         rkm_total, 
         min_start_date, 
         max_end_date, 
         site_description) %>%
  # trim down to just sites, not antennas
  distinct(site_code, .keep_all = T) %>%
  arrange(rkm)

# write_csv
write_csv(sr_config,
          file = here("data/derived_data/Snake_PTAGIS_IPTDS_20230608.csv"))
