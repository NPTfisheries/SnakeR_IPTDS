# -----------------------
# Author: Mike Ackerman & Ryan N. Kinzer
# Purpose: Create interactive maps for Snake R. IPTDS prioritization and planning
# 
# Created: June 9, 2023
# Last Modified: June 9, 2023
# Notes: Much of this is based on a previous script iptds_planning.R from RK
# 

# -----------------------
# SETUP

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(here)
# library(janitor)
# library(here)

# -----------------------
# COMPILE DATA

# get states
pnw_states = ne_states(country = "united states of america", returnclass = "sf") %>%
  select(name) %>%
  filter(name %in% c("Idaho", "Oregon", "Washington"))

#load(here("data/derived_data/spatial/SR_pops.rda"))

# -----------------------
# BUILD LEAFLET

# use Section 3 in Ryan's iptds_planning script