# -----------------------
# Author: Mike Ackerman
# Purpose: Compile some information on IPTDS operational times
# 
# Created: April 22, 2024
#   Last Modified: 
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(PITcleanr)
library(janitor)

# query interrogation site metadata
iptds_meta = queryInterrogationMeta() %>%
  clean_names()
  
# snake river interrogation site meta
sr_iptds_meta = iptds_meta %>%
  filter(str_starts(rkm, "522"),
         site_type %in% c("Instream Remote Detection System",
                          "Adult Fishway")) %>%
  select(site_code,
         name,
         active,
         operational,
         first_year,
         last_year,
         first_date,
         last_date,
         last_file_opened_on,
         operation_period,
         operations_organization_code,
         rkm,
         site_type,
         latitude,
         longitude)

# snake river interrogation site configuration
sr_iptds_config = queryInterrogationConfig() %>%
  clean_names() %>%
  filter(site_code %in% sr_iptds_meta$site_code)

# virtual test tags
# get code from PITcleanr::queryTimerTagSite() 

### END SCRIPT

