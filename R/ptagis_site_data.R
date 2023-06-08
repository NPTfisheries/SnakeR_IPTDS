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
# remotes::install_github("mackerman44/PITcleanr@main", build_vignettes = F, force = T)
library(PITcleanr)
browseVignettes("PITcleanr")

# build original configuration table from PTAGIS (requires interwebs connection); queries all INT and MRR sites in PTAGIS
og_config = buildConfig() 

# filter to only Snake River sites and IPTDS
sr_config = og_config %>%
  filter(str_starts(rkm, "522"),
         site_type == "INT",
         site_type_name == "Instream Remote Detection System") %>%
  select(site_code, 
         site_name, 
         latitude, 
         longitude, 
         rkm, 
         rkm_total, 
         start_date, 
         end_date, 
         site_description) %>%
  distinct(site_code, .keep_all = T)

# write_csv
write_csv(sr_config,
          file = here("data/derived_data/Snake_PTAGIS_IPTDS_20230608.csv"))
